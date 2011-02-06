/*

    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>

--------------------------------------------------------------------------
    This file is part of MyDLP.

    MyDLP is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MyDLP is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------
*/

/*
 * Include the core server components.
 */
#include "httpd.h"
#include "http_log.h"
#include "http_config.h"
#include "http_protocol.h"
#include "ap_config.h"
#include "util_filter.h"
#include "http_request.h"
#include "apr_strings.h"
#include "apr_thread_mutex.h"

#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

#include <protocol/TBinaryProtocol.h>
#include <transport/TSocket.h>
#include <transport/TTransportUtils.h>

#include "Mydlp_ui.h"

#undef strtoul

#define DEBUG 1

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace boost;

/************************
 * Forward Declarations *
 ************************/
extern "C" module AP_MODULE_DECLARE_DATA dlp_module;

static apr_thread_mutex_t* thrift_connect_mutex;

static ap_filter_rec_t * dlp_filter_handle; 

typedef struct {
	shared_ptr<TTransport> transport;
	shared_ptr<Mydlp_uiIf> client;
} mod_dlp_cfg ;

static void try_reconnect(mod_dlp_cfg* cfg)
{
	try 
	{
		apr_status_t rc = APR_SUCCESS;
		if ((rc = apr_thread_mutex_lock(thrift_connect_mutex)) != APR_SUCCESS) {
			ap_log_error(APLOG_MARK, APLOG_ERR, rc, NULL,
				"mod_dlp: Cannot lock thrift_connect_mutex");
			return;
		}
		cfg->transport->close();
		cfg->transport->open();
		if ((rc = apr_thread_mutex_unlock(thrift_connect_mutex)) != APR_SUCCESS) {
			ap_log_error(APLOG_MARK, APLOG_ERR, rc, NULL,
				"mod_dlp: Cannot unlock thrift_connect_mutex");
			return;
		}
	}
	catch (...)
	{
		ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL,
			"mod_dlp: Failed during trying to reconnect to thrift server.");
	}
}

static unsigned int init_entity(mod_dlp_cfg* cfg)
{
	try 
	{
		int entity_id = cfg->client->initEntity();
		return entity_id;
	}
	catch (...)
	{
		ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL,
			"mod_dlp: Error calling init_entity.");
		try_reconnect(cfg);
	}
	return 0;
}

static void push_data(mod_dlp_cfg* cfg, unsigned int entity_id, const string & data)
{
	try 
	{
		cfg->client->pushData(entity_id, data);
	}
	catch (...)
	{
		ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL,
			"mod_dlp: Error calling push_data.");
		try_reconnect(cfg);
	}
}

static bool analyze(mod_dlp_cfg* cfg, unsigned int entity_id)
{
	try 
	{
		bool result = cfg->client->analyze(entity_id);
		return result;
	}
	catch (...)
	{
		ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL,
			"mod_dlp: Error calling analyze.");
		try_reconnect(cfg);
	}
	return true;
}

static void close_entity(mod_dlp_cfg* cfg, unsigned int entity_id)
{
	try 
	{
		cfg->client->closeEntity(entity_id);
	}
	catch (...)
	{
		ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL,
			"mod_dlp: Error calling close_entity.");
		try_reconnect(cfg);
	}
}

/* The main filter */
static int dlp_filter (ap_filter_t* f, apr_bucket_brigade* bb)
{
	apr_bucket* b ;
	int entity_id;
	mod_dlp_cfg* cfg = (mod_dlp_cfg*)f->ctx ;
	entity_id = init_entity(cfg);
	bool analysis_result = true;
	if ( entity_id != 0 ) {
		for ( b = APR_BRIGADE_FIRST(bb) ;
				b != APR_BRIGADE_SENTINEL(bb) ;
				b = APR_BUCKET_NEXT(b) ) {
			if (APR_BUCKET_IS_EOS(b) || APR_BUCKET_IS_FLUSH(b))
				continue;
			const char* buf = 0 ;
			apr_size_t bytes = 0 ;
			apr_bucket_read(b, &buf, &bytes, APR_BLOCK_READ);
			string s(buf,bytes);
			push_data(cfg, entity_id, s);
		}
		analysis_result = analyze(cfg, entity_id);
		close_entity(cfg, entity_id);
	}
	if ( analysis_result )
		return ap_pass_brigade(f->next, bb) ;
	else
		return APR_SUCCESS;
}

static void insert_filters(request_rec *r) {
	mod_dlp_cfg* cfg = (mod_dlp_cfg*) ap_get_module_config(r->per_dir_config, &dlp_module) ;
	ap_add_output_filter_handle(dlp_filter_handle, cfg, r, r->connection) ;
}

static void mod_dlp_register_hooks (apr_pool_t *p)
{
	apr_status_t rc = APR_SUCCESS;
	rc = apr_thread_mutex_create(&thrift_connect_mutex, APR_THREAD_MUTEX_DEFAULT, p);
	if (rc != APR_SUCCESS) {
		ap_log_perror(APLOG_MARK, APLOG_ERR, rc, p,
			"Could not create thrift mutex");
		exit(1);
	}

	dlp_filter_handle = ap_register_output_filter("MODDLP", dlp_filter, NULL, AP_FTYPE_RESOURCE) ;
	ap_hook_insert_filter(insert_filters, NULL, NULL, APR_HOOK_MIDDLE) ;
}

static void* mod_dlp_config(apr_pool_t* pool, char* x) {
	mod_dlp_cfg* ret = (mod_dlp_cfg*) apr_palloc(pool, sizeof(mod_dlp_cfg)) ;

	shared_ptr<TTransport> socket(new TSocket("localhost", 9092));
	shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	shared_ptr<Mydlp_uiIf> client(new Mydlp_uiClient(protocol));

	try {
		apr_status_t rc = APR_SUCCESS;
		if ((rc = apr_thread_mutex_lock(thrift_connect_mutex)) != APR_SUCCESS) {
			ap_log_perror(APLOG_MARK, APLOG_ERR, rc, pool,
				"mod_dlp: config: Cannot lock thrift_connect_mutex");
		}
		transport->open();
		if ((rc = apr_thread_mutex_unlock(thrift_connect_mutex)) != APR_SUCCESS) {
			ap_log_perror(APLOG_MARK, APLOG_ERR, rc, pool,
				"mod_dlp: config: Cannot unlock thrift_connect_mutex");
		}
	} catch (...) {}

	ret->client = client;
	ret->transport = transport;
	return ret ;
}

/************************
 * Global Dispatch List *
 ************************/

// We have to use C style linkage for the API functions that will be
// linked to apache.
extern "C" {
	// Dispatch list for API hooks
	module AP_MODULE_DECLARE_DATA dlp_module =
	{
		STANDARD20_MODULE_STUFF,
		mod_dlp_config,
		NULL,
		NULL,
		NULL,
		NULL,
		mod_dlp_register_hooks,
	};
};
