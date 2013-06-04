package com.mydlp.backend.thrift;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import org.apache.thrift.TException;
import org.apache.tika.Tika;
import org.apache.tika.io.IOUtils;
import org.apache.tika.metadata.Metadata;
import org.htmlcleaner.CleanerProperties;
import org.htmlcleaner.HtmlCleaner;
import org.htmlcleaner.TagNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mydlp.seclore.FileSecureConfigBuilder;
import com.mydlp.seclore.FileSecureException;
import com.mydlp.seclore.FileSecureProtect;
import com.mydlp.seclore.MyDLPCertificateException;

public class MydlpImpl implements Mydlp.Iface {

	private static Logger logger = LoggerFactory.getLogger(MydlpImpl.class);

	protected static final String DEFAULT_ENCODING = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_ENCODING)
			.encode(CharBuffer.wrap(""));
	protected static final ByteBuffer ERROR = Charset.forName(DEFAULT_ENCODING)
			.encode(CharBuffer.wrap("mydlp-internal/error"));
	protected static final String MIME_NOT_FOUND = "mydlp-internal/not-found";

	protected Tika tika = null;

	protected HtmlCleaner cleaner = null;

	protected FileSecureConfigBuilder secloreConfig = null;

	protected FileSecureProtect secloreProtect = null;

	protected Tika getTika() {
		if (tika == null) {
			tika = new Tika();
		}
		return tika;
	}

	protected HtmlCleaner getCleaner() {
		if (cleaner == null) {
			cleaner = new HtmlCleaner();
			CleanerProperties props = cleaner.getProperties();
			props.setAllowHtmlInsideAttributes(true);
			props.setAllowMultiWordAttributes(true);
			props.setRecognizeUnicodeChars(true);
			props.setOmitComments(true);
		}
		return cleaner;
	}

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
	public String getMime(String FileName, ByteBuffer Data) throws TException {
		Metadata metadata = new Metadata();
		if (FileName != null && FileName.length() > 0)
			metadata.add(Metadata.RESOURCE_NAME_KEY, FileName);
		InputStream inputStream = getInputStream(Data);
		try {
			return getTika().detect(inputStream, metadata);
		} catch (IOException e) {
			logger.error("Can not detect file type", e);
			return MIME_NOT_FOUND;
		} catch (Throwable e) {
			logger.error("Can not detect file type", e);
			return MIME_NOT_FOUND;
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

	protected Reader getMeta(Metadata metadata) {
		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < metadata.names().length; i++) {
			String name = metadata.names()[i];
			if (name.equals(Metadata.CONTENT_TYPE))
				continue;
			if (name.equals(Metadata.CONTENT_TYPE))
				continue;
			buffer.append(name + " : " + metadata.get(name) + "\n");
		}
		return new StringReader(buffer.toString());
	}
	
	protected ByteBuffer getFinalBuffer(Reader metaReader, Reader textReader) 
			throws IOException {
		byte[] metaBytes = IOUtils.toByteArray(metaReader, DEFAULT_ENCODING);
		byte[] textBytes = IOUtils.toByteArray(textReader, DEFAULT_ENCODING);
		byte[] finalBytes = new byte[metaBytes.length + textBytes.length];

		System.arraycopy(metaBytes,	0,	finalBytes,	0,					metaBytes.length);
		System.arraycopy(textBytes,	0,	finalBytes,	metaBytes.length,	textBytes.length);
		
		return ByteBuffer.wrap(finalBytes);
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
			Reader textReader = getTika().parse(inputStream, metadata);
			Reader metaReader = getMeta(metadata);
			return getFinalBuffer(metaReader, textReader);
		} catch (Throwable e) {
			if (isMemoryError(e)) {
				logger.error("Can not allocate required memory", e);
				return EMPTY;
			} else {
				logger.error("Can not read text", e);
				return ERROR;
			}
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				logger.error("Can not close stream", e);
			}
		}
	}

	@Override
	public ByteBuffer getUnicodeText(String Encoding, ByteBuffer Data)
			throws TException {
		InputStream inputStream = getInputStream(Data);
		try {
			Metadata metadata = new Metadata();
			metadata.add(Metadata.RESOURCE_NAME_KEY, "sample.txt");
			metadata.add(Metadata.CONTENT_TYPE, "text/plain");
			if (Encoding != null && Encoding.length() > 0
					&& !Encoding.equals("unknown"))
				;
			metadata.add(Metadata.CONTENT_ENCODING, Encoding);
			Reader reader = getTika().parse(inputStream, metadata);
			return ByteBuffer.wrap(IOUtils
					.toByteArray(reader, DEFAULT_ENCODING));
		} catch (Throwable e) {
			if (isMemoryError(e)) {
				logger.error("Can not allocate required memory", e);
				return EMPTY;
			} else {
				logger.error("Can not read unicode text", e);
				return ERROR;
			}
		} finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				logger.error("Can not close stream", e);
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> extractLinks(ByteBuffer HTMLData) throws TException {
		List<String> links = new ArrayList<String>();
		try {
			TagNode rootNode = getCleaner().clean(getInputStream(HTMLData));
			for (TagNode aTag : (List<TagNode>) rootNode.getElementListByName(
					"a", true)) {
				try {
					String link = aTag.getAttributeByName("href");
					if (link != null && link.length() > 0)
						links.add(link);
				} catch (Throwable t) {
					logger.error("Can not extract link", t);
				}
			}
		} catch (IOException e) {
			logger.error("Can not parse HTML", e);
		}
		return links;
	}

	@Override
	public String secloreInitialize(String SecloreAppPath,
			String SecloreAddress, int SeclorePort, String SecloreAppName,
			int SecloreHotFolderCabinetId,
			String SecloreHotFolderCabinetPassphrase, int SeclorePoolSize)
			throws TException {
		try {
			if (secloreProtect != null || secloreConfig != null)
				secloreTerminate();

			secloreConfig = new FileSecureConfigBuilder(SecloreAppPath,
					SecloreAddress, SeclorePort, SecloreAppName,
					SecloreHotFolderCabinetId,
					SecloreHotFolderCabinetPassphrase, SeclorePoolSize);
			try {
				secloreConfig.installCertificateIfNotExists();
			} catch (MyDLPCertificateException e) {
				logger.error(
						"An error occurred when install seclore remote certificate",
						e);
			}

			secloreProtect = new FileSecureProtect(secloreConfig);

			try {
				secloreProtect.initialize();
				return "ok";
			} catch (FileSecureException e) {
				logger.error(
						"An error occurred when initializing seclore configuration",
						e);
				return e.getMessage();
			}
		} catch (Throwable e) {
			logger.error(
					"An error unexpected occurred when initializing seclore configuration",
					e);
			return "mydlp.backend.seclore.initialize.unexpectedException";
		}
	}

	@Override
	public String secloreProtect(String FilePath, int HotFolderId,
			String ActivityComments) throws TException {
		if (secloreProtect == null) {
			return "mydlp.backend.seclore.protect.notInitialized";
		}
		try {
			File file = new File(FilePath);
			if (!file.exists())
				return "mydlp.backend.seclore.protect.fileNotFound";
			if (!file.isFile())
				return "mydlp.backend.seclore.protect.notRegularFile";
			if (!file.canRead())
				return "mydlp.backend.seclore.protect.canNotRead";
			if (!file.canWrite())
				return "mydlp.backend.seclore.protect.canNotWrite";
			String fileId = secloreProtect.protect(file.getAbsolutePath(),
					HotFolderId, ActivityComments);
			return "ok " + fileId;
		} catch (FileSecureException e) {
			logger.error(
					"An error occurred when protecting document ( " + FilePath
							+ " ) woth hot folder id (" + HotFolderId + " )", e);
			return e.getMessage();
		} catch (Throwable e) {
			logger.error(
					"An error unexpected occurred when protecting document ( "
							+ FilePath + " ) woth hot folder id ("
							+ HotFolderId + " )", e);
			return "mydlp.backend.seclore.protect.unexpectedException";
		}
	}

	@Override
	public String secloreTerminate() throws TException {
		if (secloreProtect == null) {
			return "mydlp.backend.seclore.terminate.notInitialized";
		}
		try {
			secloreProtect.terminate();
			secloreConfig = null;
			secloreProtect = null;
			return "ok";
		} catch (FileSecureException e) {
			logger.error(
					"An error occurred when terminating seclore connection", e);
			return e.getMessage();
		} catch (Throwable e) {
			logger.error(
					"An error unexpected occurred when terminating seclore connection",
					e);
			return "mydlp.backend.seclore.protect.unexpectedException";
		}
	}

}
