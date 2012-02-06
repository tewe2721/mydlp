package com.mydlp.backend.thrift;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;

public class MydlpImpl implements Mydlp.Iface {
	
	protected static final String DEFAULT_CHARSET = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_CHARSET).encode(CharBuffer.wrap(""));
	
	protected Tika tika;
	
		public MydlpImpl(Tika tikaInstsance) {
		super();
		this.tika = tikaInstsance;
	}
		
	protected InputStream getInputStream(final ByteBuffer buf) {
		return new InputStream() {
			public synchronized int read() throws IOException {				
				return buf.hasRemaining() ? buf.get() : -1;
			}
	 
			public synchronized int read(byte[] bytes, int off, int len) throws IOException {			
				int rv = Math.min(len, buf.remaining());				
				buf.get(bytes, off, rv);
				return rv == 0 ? -1 : rv;
			}
		};
	}

	@Override
	public String getMime(ByteBuffer Data) throws TException {
		try {
			return tika.detect(getInputStream(Data));
		} catch (IOException e) {
			e.printStackTrace();
		}
		return "mydlp-internal/not-found";
	}

	@Override
	public ByteBuffer getText(ByteBuffer Data) throws TException {
		try {
			return Charset.forName(DEFAULT_CHARSET).encode(
					CharBuffer.wrap(
							tika.parseToString(getInputStream(Data))));
		} catch (TikaException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return EMPTY;
	}

}
