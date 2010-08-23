
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.thrift.TException;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;

import com.medratech.bayessianzemberek.BayessianAnalyzer;
import com.medratech.bayessianzemberek.BayessianDBUtil;
import com.sun.akuma.Daemon;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
/**
 * Hello world!
 * 
 */
public class BayessianZemberekBackend {

    private static final Logger LOGGER = LoggerFactory.getLogger(BayessianZemberekBackend.class.getName());

    public static class MyDLPZBHandler implements Mydlp_bz.Iface {

        private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();
        private final Lock read = readWriteLock.readLock();
        private final Lock write = readWriteLock.writeLock();
        private BayessianAnalyzer analyzer = new BayessianAnalyzer();
        private BayessianDBUtil bayessianDBUtil = new BayessianDBUtil();

        @Override
        public double score(String Text) throws TException {
            read.lock();
            double ret = 0.5;
            try {
                StringBuffer tBuff = new StringBuffer();
                tBuff.append(Text);
                ret = analyzer.score(tBuff);
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
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
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
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
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
            } finally {
                write.unlock();
            }
        }

        @Override
        public void reset() throws TException {
            write.lock();
            try {
                analyzer.resetDB();
                bayessianDBUtil.setBayessianDB(analyzer.getBayessianDB());
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
            } finally {
                write.unlock();
            }
        }

        @Override
        public void pushDB(String DBC, String DBNC) throws TException {
            write.lock();
            try {
                bayessianDBUtil.read(new StringBuffer(DBC), new StringBuffer(DBNC));
                analyzer.setBayessianDB(bayessianDBUtil.getBayessianDB());
                LOGGER.info("Loaded new database.");
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
            } finally {
                write.unlock();
            }
        }

        @Override
        public List<String> pullDB() throws TException {
            read.lock();
            List<String> ret = new ArrayList<String>();
            try {
                bayessianDBUtil.setBayessianDB(analyzer.getBayessianDB());
                ret.add(bayessianDBUtil.getClassifiedDB().toString());
                ret.add(bayessianDBUtil.getNonClassifiedDB().toString());
                LOGGER.info("Serialized database.");
            } catch (RuntimeException e) {
                LOGGER.error("Exception: ", e);
            } finally {
                read.unlock();
            }
            return ret;
        }
    }

    public static void main(String[] args) {
        Daemon d = new Daemon();

        if (d.isDaemonized()) {
            String pidFN = "/var/run/mydlp/backend-java.pid";
            if (System.getProperty("daemon.pidfile") != null) {
                pidFN = System.getProperty("daemon.pidfile");
            }
            try {
                d.init(pidFN);
            } catch (Exception e) {
                LOGGER.error("Exception: ", e);
            }
        } else {
            try {
                d.daemonize();
            } catch (IOException e) {
                LOGGER.error("Exception: ", e);
            }
            System.exit(0);
        }

        try {
            MyDLPZBHandler handler = new MyDLPZBHandler();
            Mydlp_bz.Processor processor = new Mydlp_bz.Processor(handler);
            TServerTransport serverTransport = new TServerSocket(9091);
            TServer server = new TThreadPoolServer(processor, serverTransport);

            System.out.println("Starting the server...");
            LOGGER.info("Starting the server...");
            server.serve();
        } catch (Exception x) {
            LOGGER.error("Exception: ", x);
        }
        System.out.println("done.");
    }
}
