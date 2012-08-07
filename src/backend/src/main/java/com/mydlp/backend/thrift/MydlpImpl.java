package com.mydlp.backend.thrift;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.thrift.TException;
import org.apache.tika.detect.DefaultDetector;
import org.apache.tika.detect.Detector;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.mime.MediaType;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.CompositeParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.Parser;
import org.apache.tika.parser.ParserDecorator;
import org.apache.tika.sax.BodyContentHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.ContentHandler;

public class MydlpImpl implements Mydlp.Iface {
	
	private static Logger logger = LoggerFactory.getLogger(MydlpImpl.class);

	protected static final String DEFAULT_ENCODING = "UTF-8";
	protected static final ByteBuffer EMPTY = Charset.forName(DEFAULT_ENCODING)
			.encode(CharBuffer.wrap(""));
	protected static final ByteBuffer ERROR = Charset.forName(DEFAULT_ENCODING)
			.encode(CharBuffer.wrap("mydlp-internal/error"));
	protected static final String MIME_NOT_FOUND = "mydlp-internal/not-found";
	
	protected Parser parser = null;
	protected Detector detector = null;
	protected ParseContext context = null;
	
	public MydlpImpl() {
		context = new ParseContext();
        detector = new DefaultDetector();
        parser = new AutoDetectParser(detector);
        context.set(Parser.class, parser);
        displayParsers(true);
	}
	
	/*
     * Displays loaded parsers and their mime types
     * If a parser is a composite parser, it will list the
     * sub parsers and their mime-types.
     */
    protected void displayParsers(boolean includeMimeTypes) {
    	StringBuffer toDisplay = new StringBuffer();
        displayParser(parser, includeMimeTypes, 0, toDisplay);
        logger.info("\n" + toDisplay.toString());
    }
     
    private void displayParser(Parser p, boolean includeMimeTypes, int i, StringBuffer toDisplay) {
    	
        boolean isComposite = (p instanceof CompositeParser);
        String name = (p instanceof ParserDecorator) ?
                      ((ParserDecorator) p).getWrappedParser().getClass().getName() :
                      p.getClass().getName();
        toDisplay.append(indent(i) + name + (isComposite ? " (Composite Parser):" : "") + "\n");
        if (includeMimeTypes && !isComposite) {
            for (MediaType mt : p.getSupportedTypes(context)) {
                toDisplay.append(indent(i+2) + mt + "\n");
            }
        }
        
        if (isComposite) {
            Parser[] subParsers = sortParsers(invertMediaTypeMap(((CompositeParser) p).getParsers()));
            for(Parser sp : subParsers) {
                displayParser(sp, includeMimeTypes, i+2, toDisplay);
            }
        }
    }
    
    private Parser[] sortParsers(Map<Parser, Set<MediaType>> parsers) {
        // Get a nicely sorted list of the parsers
        Parser[] sortedParsers = parsers.keySet().toArray(new Parser[parsers.size()]);
        Arrays.sort(sortedParsers, new Comparator<Parser>() {
            public int compare(Parser p1, Parser p2) {
                String name1 = p1.getClass().getName();
                String name2 = p2.getClass().getName();
                return name1.compareTo(name2);
            }
        });
        return sortedParsers;
    }
    
    private Map<Parser, Set<MediaType>> invertMediaTypeMap(Map<MediaType, Parser> supported) {
        Map<Parser,Set<MediaType>> parsers = new HashMap<Parser, Set<MediaType>>();
        for(Entry<MediaType, Parser> e : supported.entrySet()) {
            if (!parsers.containsKey(e.getValue())) {
                parsers.put(e.getValue(), new HashSet<MediaType>());
            }
            parsers.get(e.getValue()).add(e.getKey());
        }
        return parsers;
    }
    
    private String indent(int indent) {
        return "                     ".substring(0, indent);
    }
	
	protected TikaInputStream getInputStream(final ByteBuffer buf) {
		return TikaInputStream.get(new InputStream() {
			public synchronized int read() throws IOException {
				return buf.hasRemaining() ? buf.get() : -1;
			}

			public synchronized int read(byte[] bytes, int off, int len)
					throws IOException {
				int rv = Math.min(len, buf.remaining());
				buf.get(bytes, off, rv);
				return rv == 0 ? -1 : rv;
			}
		});
	}

	@Override
	public String getMime(String FileName, ByteBuffer Data) throws TException {
		Metadata metadata = new Metadata();
		if (FileName != null && FileName.length() > 0)
			metadata.add(Metadata.RESOURCE_NAME_KEY, FileName);
		TikaInputStream inputStream = getInputStream(Data);
		try {
			return detector.detect(inputStream, metadata).toString();
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

	@Override
	public ByteBuffer getText(String FileName, String MimeType, ByteBuffer Data)
			throws TException {
		InputStream inputStream = getInputStream(Data);
		try {
			Metadata metadata = new Metadata();
			if (FileName != null && FileName.length() > 0)
				metadata.add(Metadata.RESOURCE_NAME_KEY, FileName);
			metadata.add(Metadata.CONTENT_TYPE, MimeType);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ContentHandler handler = new BodyContentHandler(bos);
			parser.parse(inputStream, handler, metadata, context);
			bos.close();
			return ByteBuffer.wrap(bos.toByteArray());
		} catch (Throwable e) {
			if (isMemoryError(e))
			{
				logger.error("Can not allocate required memory", e);
				return EMPTY;
			}
			else
			{
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

}
