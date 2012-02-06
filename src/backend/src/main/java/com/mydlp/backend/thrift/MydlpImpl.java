package com.mydlp.backend.thrift;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.Parser;
import org.apache.tika.sax.BodyContentHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

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
			ContentHandler textHandler = new BodyContentHandler();
			Metadata metadata = new Metadata();
			ParseContext parseContext = new ParseContext();
			Parser parser = new AutoDetectParser();
			parser.parse(inputStream, textHandler, metadata, parseContext);
			return Charset.forName(DEFAULT_CHARSET).encode(
					CharBuffer.wrap(
							textHandler.toString()));
		} catch (TikaException e) {
			e.printStackTrace();
			return EMPTY;
		} catch (IOException e) {
			e.printStackTrace();
			return EMPTY;
		} catch (SAXException e) {
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
