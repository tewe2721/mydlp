package com.mydlp.backend;

import org.apache.commons.daemon.Daemon;
import org.apache.commons.daemon.DaemonContext;
import org.apache.commons.daemon.DaemonInitException;

public class Bootstrap implements Daemon {
	
	private BackendServer server = null;

	@Override
	public void destroy() {
	}

	@Override
	public void init(DaemonContext arg0) throws DaemonInitException, Exception {
		server = new BackendServer();
	}

	@Override
	public void start() throws Exception {
		server.listen();
	}

	@Override
	public void stop() throws Exception {
		if (server != null)
			server.stop();
	}

}
