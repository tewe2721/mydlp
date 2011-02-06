// Apache stuff
//  Its important to use "" instead of <> and to have the -I flags in
//  the right order in the Makefile because there is an Apache alloc.h
//  that is completely different from the system alloc.h.
#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"
#include "http_main.h"
#include "util_script.h"
#include "ap_config.h"
#include "http_log.h"

// Apache's compatibility warnings are of no concern to us.
#undef strtoul

// System include files
#include <string>

/************************
 * Forward Declarations *
 ************************/
extern "C" module MODULE_VAR_EXPORT cpphello_module;

/**************************
 * Configuration Handlers *
 **************************/

/* per directory configuration structure. */
typedef struct {
    char *hellomessage;
} cpphello_dir_config;

static void *cpphello_create_dir_config(pool *p, char *path)
{
    cpphello_dir_config *cfg = 
	(cpphello_dir_config *)ap_pcalloc(p, sizeof(cpphello_dir_config));
    cfg->hellomessage = "Hello, world!";
    return (void *)cfg;
}

typedef const char *(*CMD_HAND_TYPE) ();
static command_rec cpphello_cmds[] =
{
    {
	"HelloMessage",             /* directive name */
	(CMD_HAND_TYPE)ap_set_string_slot, /* built in handler */
	(void *)XtOffsetOf(cpphello_dir_config, hellomessage),
	OR_ALL,                          /* where available */
	TAKE1,                           /* arguments */
	"Message to print."
                                         /* directive description */
    },
    {NULL, NULL, NULL, 0, cmd_how(0), NULL}
};

/********************
 * Content Handlers *
 ********************/

/* The main event */
static int cpphello_handler(request_rec *r)
{
    // Get the config for this request.
    cpphello_dir_config *cfg = 
	(cpphello_dir_config *)
	ap_get_module_config(r->per_dir_config, &cpphello_module);

    // Generate the message.
    string messagetosend = 
	string("<P>") + 
	string(cfg->hellomessage) + 
	string("</P>\n");

    // Send the headers.
    r->content_type = "text/html";
    ap_send_http_header(r);

    if (!r->header_only) {
	ap_rputs(messagetosend.c_str(), r);
    }

    return OK;
}

/* Dispatch list of content handlers */
static const handler_rec cpphello_handlers[] = { 
    { "cpphello", cpphello_handler }, 
    { NULL, NULL }
};

/************************
 * Global Dispatch List *
 ************************/

// We have to use C style linkage for the API functions that will be
// linked to apache.
extern "C" {
    // Dispatch list for API hooks 
    module MODULE_VAR_EXPORT cpphello_module = {
	STANDARD_MODULE_STUFF, 
	NULL,                       /* module initializer                  */
	cpphello_create_dir_config, /* create per-dir    config structures */
	NULL,                       /* merge  per-dir    config structures */
	NULL,                       /* create per-server config structures */
	NULL,                       /* merge  per-server config structures */
	cpphello_cmds,              /* table of config file commands       */
	cpphello_handlers,          /* [#8] MIME-typed-dispatched handlers */
	NULL,                       /* [#1] URI to filename translation    */
	NULL,                       /* [#4] validate user id from request  */
	NULL,                       /* [#5] check if the user is ok _here_ */
	NULL,                       /* [#3] check access by host address   */
	NULL,                       /* [#6] determine MIME type            */
	NULL,                       /* [#7] pre-run fixups                 */
	NULL,                       /* [#9] log a transaction              */
	NULL,                       /* [#2] header parser                  */
	NULL,                       /* child_init                          */
	NULL,                       /* child_exit                          */
	NULL                        /* [#0] post read-request              */
#ifdef EAPI
	,NULL,                      /* EAPI: add_module                    */
	NULL,                       /* EAPI: remove_module                 */
	NULL,                       /* EAPI: rewrite_command               */
	NULL,                       /* EAPI: new_connection                */
	NULL                        /* EAPI: close_connection              */
#endif
    };
};
