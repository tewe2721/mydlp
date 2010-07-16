import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.thrift.TException;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;

import com.medratech.bayessianzemberek.BayessianAnalyzer;
import com.medratech.bayessianzemberek.BayessianFileUtil;
import com.sun.akuma.Daemon;

/**
 * Hello world!
 * 
 */
public class BayessianZemberekBackend {

	public static class MyDLPZBHandler implements Mydlp_bz.Iface {
		private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();
		private final Lock read = readWriteLock.readLock();
		private final Lock write = readWriteLock.writeLock();

		private BayessianAnalyzer analyzer = null;

		private BayessianFileUtil bayessianFileUtil = null;

		public MyDLPZBHandler() {
			analyzer = new BayessianAnalyzer();

			String datC = "/var/lib/mydlp/bz/confidential.dat";
			if (System.getProperty("bayesdb.confidential") != null)
				datC = System.getProperty("bayesdb.confidential");

			String datP = "/var/lib/mydlp/bz/public.dat";
			if (System.getProperty("bayesdb.public") != null)
				datP = System.getProperty("bayesdb.public");

			bayessianFileUtil = new BayessianFileUtil(datC, datP);
			bayessianFileUtil.read();
			analyzer.setBayessianDB(bayessianFileUtil.getBayessianDB());
		}

		@Override
		public double score(String Text) throws TException {
			read.lock();
			double ret = 0.5;
			try {
				StringBuffer tBuff = new StringBuffer();
				tBuff.append(Text);
				ret = analyzer.score(tBuff);
			} catch (RuntimeException e) {
				e.printStackTrace();
			} finally {
				read.unlock();
			}
			return ret;
		}

		@Override
		public void trainConfidential(String Text) throws TException {
			write.lock();
			try {
				StringBuffer tBuff = new StringBuffer();
				tBuff.append(Text);
				analyzer.trainClassified(tBuff);
				bayessianFileUtil.setBayessianDB(analyzer.getBayessianDB());
				bayessianFileUtil.write();
			} catch (RuntimeException e) {
				e.printStackTrace();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} finally {
				write.unlock();
			}
		}

		@Override
		public void trainPublic(String Text) throws TException {
			write.lock();
			try {
				StringBuffer tBuff = new StringBuffer();
				tBuff.append(Text);
				analyzer.trainNonclassified(tBuff);
				bayessianFileUtil.setBayessianDB(analyzer.getBayessianDB());
				bayessianFileUtil.write();
			} catch (RuntimeException e) {
				e.printStackTrace();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} finally {
				write.unlock();
			}
		}

		@Override
		public void reset() throws TException {
			write.lock();
			try {
				analyzer.resetDB();
				bayessianFileUtil.setBayessianDB(analyzer.getBayessianDB());
				bayessianFileUtil.write();
			} catch (RuntimeException e) {
				e.printStackTrace();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} finally {
				write.unlock();
			}
		}

	}

	public static void main(String[] args) {
		Daemon d = new Daemon();

		if (d.isDaemonized()) {
			String pidFN = "/var/run/mydlp/backend-java.pid";
			if (System.getProperty("daemon.pidfile") != null)
				pidFN = System.getProperty("daemon.pidfile");
			try {
				d.init(pidFN);
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			try {
				d.daemonize();
			} catch (IOException e) {
				e.printStackTrace();
			}
			System.exit(0);
		}

		try {
			MyDLPZBHandler handler = new MyDLPZBHandler();
			Mydlp_bz.Processor processor = new Mydlp_bz.Processor(handler);
			TServerTransport serverTransport = new TServerSocket(9091);
			TServer server = new TThreadPoolServer(processor, serverTransport);

			System.out.println("Starting the server...");
			server.serve();
		} catch (Exception x) {
			x.printStackTrace();
		}
		System.out.println("done.");
	}
}
