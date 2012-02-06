package com.mydlp.backend.thrift;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.fork.ForkParser;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.sax.BodyContentHandler;
import org.xml.sax.SAXException;

public class MydlpImpl implements Mydlp.Iface {

	protected static final String DEFAULT_CHARSET = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_CHARSET)
			.encode(CharBuffer.wrap(""));
	
	protected static final String JAVA_COMMAND;
	
	static {
		String osType = System.getProperty("os.arch");
		System.out.println("Operating system type => " + osType);
		String osName = System.getProperty("os.name");
		System.out.println("Operating system type => " + osName);
		String javaHome = System.getProperty("java.home");
        File f = new File(javaHome);
        f = new File(f, "bin");
        f = new File(f, "java");
        JAVA_COMMAND = f.getAbsolutePath() + " -cp -Xmx128m";
	}

	protected Tika tika = new Tika();
	
	protected InputStream getInputStream(final ByteBuffer buf) {
		return new InputStream() {
			public synchronized int read() throws IOException {
				return buf.hasRemaining() ? buf.get() : -1;
			}

			public synchronized int read(byte[] bytes, int off, int len)
					throws IOException {
				int rv = Math.min(len, buf.remaining());
				buf.get(bytes, off, rv);
				return rv == 0 ? -1 : rv;
			}
		};
	}

	@Override
	public String getMime(ByteBuffer Data) throws TException {
		InputStream inputStream = getInputStream(Data);
		try {
			return tika.detect(inputStream);
		} catch (IOException e) {
			e.printStackTrace();
			return "mydlp-internal/not-found";
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public ByteBuffer getText(ByteBuffer Data) throws TException {
		InputStream inputStream = getInputStream(Data);
		try {
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			BodyContentHandler contentHandler = new BodyContentHandler(os);
			ParseContext context	 = new ParseContext();
			ForkParser parser = new ForkParser();
			Metadata metadata = new Metadata();
			parser.setJavaCommand(JAVA_COMMAND);
			parser.parse(inputStream, contentHandler, metadata, context);
			return ByteBuffer.wrap(os.toByteArray());
		} catch (IOException e) {
			e.printStackTrace();
			return EMPTY;
		} catch (SAXException e) {
			e.printStackTrace();
			return EMPTY;
		} catch (TikaException e) {
			e.printStackTrace();
			return EMPTY;
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

}
