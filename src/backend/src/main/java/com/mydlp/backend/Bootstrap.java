package com.mydlp.backend;

import org.apache.commons.daemon.Daemon;
import org.apache.commons.daemon.DaemonContext;

public class Bootstrap implements Daemon {
	
	private BackendServer server = null;

	@Override
	public void destroy() {
	}

	@Override
	public void init(DaemonContext arg0) {
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
