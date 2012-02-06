package com.mydlp.backend.thrift;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.io.IOUtils;
import org.xml.sax.SAXException;

public class MydlpImpl implements Mydlp.Iface {

	protected static final String DEFAULT_CHARSET = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_CHARSET)
			.encode(CharBuffer.wrap(""));
	
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
			Reader reader = tika.parse(inputStream);
			IOUtils.copy(reader, os);
			os.close();
			return ByteBuffer.wrap(os.toByteArray());
		} catch (IOException e) {
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
