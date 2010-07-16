import java.io.File;
import java.io.FileNotFoundException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.thrift.TException;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;

import com.medratech.bayessianzemberek.BayessianAnalyzer;
import com.medratech.bayessianzemberek.BayessianFileUtil;

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
			bayessianFileUtil = new BayessianFileUtil("/var/lib/mydlp/bz/confidential.dat", "/var/lib/mydlp/bz/public.dat");
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
	
	static public File getPidFile ()
	{
		return new File(System.getProperty("daemon.pidfile"));
	}
	
	static public void daemonize()
	{
	   getPidFile().deleteOnExit();
	   System.out.close();
	   System.err.close();
	}

	public static void main(String[] args) {
		try {
			daemonize();
			
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
