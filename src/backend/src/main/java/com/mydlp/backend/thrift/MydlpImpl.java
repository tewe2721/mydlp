package com.mydlp.backend.thrift;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.io.IOUtils;
import org.apache.tika.metadata.Metadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MydlpImpl implements Mydlp.Iface {
	
	private static Logger logger = LoggerFactory.getLogger(MydlpImpl.class);

	protected static final String DEFAULT_ENCODING = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_ENCODING)
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
			logger.error("Can not detect file type", e);
			return "mydlp-internal/not-found";
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				logger.error("Can not close stream", e);
			}
		}
	}
	
	protected Boolean isMemoryError(Throwable t) {
		if (t instanceof OutOfMemoryError)
			return true;
		
		Throwable cause = t.getCause();
		if (cause == null)
			return false;
		return isMemoryError(cause);
	}

	@Override
	public ByteBuffer getText(String FileName, String MimeType, ByteBuffer Data)
			throws TException {
		InputStream inputStream = getInputStream(Data);
		try {
			Metadata metadata = new Metadata();
			if (FileName != null && FileName.length() > 0)
				metadata.add(Metadata.RESOURCE_NAME_KEY, FileName);
			metadata.add(Metadata.CONTENT_TYPE, MimeType);
			Reader reader = tika.parse(inputStream, metadata);
			return ByteBuffer.wrap(IOUtils.toByteArray(reader, DEFAULT_ENCODING));
		} catch (OutOfMemoryError e) {
			logger.error("Can not allocate required memory", e);
			return EMPTY;
		} catch (IOException e) {
			if (isMemoryError(e))
			{
				logger.error("Can not allocate required memory", e);
				return EMPTY;
			}
			else
				throw new TException(e);
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				logger.error("Can not close stream", e);
			}
		}

	}

}
