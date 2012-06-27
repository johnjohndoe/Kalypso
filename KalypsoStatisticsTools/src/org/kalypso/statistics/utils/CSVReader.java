package org.kalypso.statistics.utils;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class CSVReader {

	private BufferedReader m_reader;
	private final char m_separator;
	private static final int EOL = 0;

	/**
	 * category of ordinary character
	 */
	private static final int ORDINARY = 1;

	/**
	 * categotory of the quote mark "
	 */
	private static final int QUOTE = 2;

	/**
	 * category of the separator, e.g. comma, semicolon or tab.
	 */
	private static final int SEPARATOR = 3;

	/**
	 * category of characters treated as white space.
	 */
	private static final int WHITESPACE = 4;

	/**
	 * parser: We are in blanks before the field.
	 */
	private static final int SEEKINGSTART = 0;

	/**
	 * parser: We are in the middle of an ordinary field.
	 */
	private static final int INPLAIN = 1;

	/**
	 * parser: e are in middle of field surrounded in quotes.
	 */
	private static final int INQUOTED = 2;

	/**
	 * parser: We have just hit a quote, might be doubled or might be last one.
	 */
	private static final int AFTERENDQUOTE = 3;

	/**
	 * parser: We are in blanks after the field looking for the separator
	 */
	private static final int SKIPPINGTAIL = 4;

	/**
	 * state of the parser's finite state automaton.
	 */

	/**
	 * The line we are parsing. null means none read yet. Line contains
	 * unprocessed chars. Processed ones are removed.
	 */
	private String line = null;

	/**
	 * How many lines we have read so far. Used in error messages.
	 */
	private int lineCount = 0;

	public CSVReader(final String filePath, final char separator, final Charset charset) throws FileNotFoundException {
		final InputStreamReader reader = new InputStreamReader(new FileInputStream(filePath), charset);
		m_reader = new BufferedReader(reader);
		m_separator = separator;
	}

	public CSVReader(final InputStream stream, final char separator, final Charset charset) throws FileNotFoundException {
		final InputStreamReader reader = new InputStreamReader(stream, charset);
		m_reader = new BufferedReader(reader);
		m_separator = separator;
	}

	private int categorize(final char c) {
		switch (c) {
		case ' ':
		case '\r':
		case 0xff:
			return WHITESPACE;
			// case ';':
			// case '!':
		case '#':
			// return EOL;
		case '\n':
			return EOL; /* artificially applied to end of line */
		case '\"':
			return QUOTE;
		default:
			if (c == m_separator) {
				/* dynamically determined so can't use as case label */
				return SEPARATOR;
			} else if ('!' <= c && c <= '~') {
				/* do our tests in crafted order, hoping for an early return */
				return ORDINARY;
			} else if (0x00 <= c && c <= 0x20) {
				return WHITESPACE;
			} else if (Character.isWhitespace(c)) {
				return WHITESPACE;
			} else {
				return ORDINARY;
			}
		}
	}

	public String[][] readFile() {
		final List<String[]> lines = new ArrayList<String[]>();
		String[] line = null;
		while ((line = getLine()) != null) {
			lines.add(line);
		}
		return lines.toArray(new String[0][0]);
	}

	public String[] getLine() {
		final List<String> line = new ArrayList<String>();
		String token = null;
		try {
			while (line.size() == 0) {
				while ((token = get()) != null) {
					line.add(token);
				}
			}
		} catch (final EOFException e) {
			return null;
		} catch (final IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
		return line.toArray(new String[line.size()]);
	}

	/**
	 * Read one field from the CSV file
	 * 
	 * @return String value, even if the field is numeric. Surrounded and
	 *         embedded double quotes are stripped. possibly "". null means end
	 *         of line.
	 * 
	 * @exception EOFException
	 *                at end of file after all the fields have been read.
	 * 
	 * @exception IOException
	 *                Some problem reading the file, possibly malformed data.
	 */
	private String get() throws EOFException, IOException {
		final StringBuffer field = new StringBuffer(50);
		/* we implement the parser as a finite state automaton with five states. */
		readLine();

		int state = SEEKINGSTART; /*
								 * start seeking, even if partway through a line
								 */
		/* don't need to maintain state between fields. */

		/* loop for each char in the line to find a field */
		/* guaranteed to leave early by hitting EOL */
		for (int i = 0; i < line.length(); i++) {
			final char c = line.charAt(i);
			final int category = categorize(c);
			switch (state) {
			case SEEKINGSTART: {
				/* in blanks before field */
				switch (category) {
				case WHITESPACE:
					/* ignore */
					break;
				case QUOTE:
					state = INQUOTED;
					break;
				case SEPARATOR:
					/* end of empty field */
					line = line.substring(i + 1);
					return "";
				case EOL:
					/* end of line */
					line = null;
					return null;
				case ORDINARY:
					field.append(c);
					state = INPLAIN;
					break;
				}
				break;
			} // end of SEEKINGSTART
			case INPLAIN: {
				/* in middle of ordinary field */
				switch (category) {
				case QUOTE:
					throw new IOException("Malformed CSV stream. Missing quote at start of field on line " + lineCount);
				case SEPARATOR:
					/* done */
					line = line.substring(i + 1);
					return field.toString().trim();
				case EOL:
					line = line.substring(i); /* push EOL back */
					return field.toString().trim();
				case WHITESPACE:
					field.append(' ');
					break;
				case ORDINARY:
					field.append(c);
					break;
				}
				break;
			} // end of INPLAIN
			case INQUOTED: {
				/* in middle of field surrounded in quotes */
				switch (category) {
				case QUOTE:
					state = AFTERENDQUOTE;
					break;
				case EOL:
					throw new IOException("Malformed CSV stream. Missing quote after field on line " + lineCount);
				case WHITESPACE:
					field.append(' ');
					break;
				case SEPARATOR:
				case ORDINARY:
					field.append(c);
					break;
				}
				break;
			} // end of INQUOTED
			case AFTERENDQUOTE: {
				/*
				 * In situation like this "xxx" which may turn out to be
				 * xxx""xxx" or "xxx", We find out here.
				 */
				switch (category) {
				case QUOTE:
					field.append(c);
					state = INQUOTED;
					break;
				case SEPARATOR:
					/* we are done. */
					line = line.substring(i + 1);
					return field.toString().trim();
				case EOL:
					line = line.substring(i); /* push back eol */
					return field.toString().trim();
				case WHITESPACE:
					/* ignore trailing spaces up to separator */
					state = SKIPPINGTAIL;
					break;
				case ORDINARY:
					throw new IOException("Malformed CSV stream, missing separator after field on line " + lineCount);
				}
				break;
			} // end of AFTERENDQUOTE
			case SKIPPINGTAIL: {
				/* in spaces after field seeking separator */
				switch (category) {
				case SEPARATOR:
					/* we are done. */
					line = line.substring(i + 1);
					return field.toString().trim();
				case EOL:
					line = line.substring(i); /* push back eol */
					return field.toString().trim();
				case WHITESPACE:
					/* ignore trailing spaces up to separator */
					break;
				case QUOTE:
				case ORDINARY:
					throw new IOException("Malformed CSV stream, missing separator after field on line " + lineCount);
				} // end of switch
				break;
			} // end of SKIPPINGTAIL
			} // end switch(state)
		} // end for
		throw new IOException("Program logic bug. Should not reach here. Processing line " + lineCount);
	} // end get

	/**
	 * Make sure a line is available for parsing. Does nothing if there already
	 * is one.
	 * 
	 * @exception EOFException
	 */
	private void readLine() throws EOFException, IOException {
		if (line == null) {
			line = m_reader.readLine(); /*
										 * this strips platform specific line
										 * ending
										 */
			if (line == null) {
				/* null means EOF, yet another inconsistent Java convention. */
				throw new EOFException();
			} else {
				line += '\n'; /* apply standard line end for parser to find */
				lineCount++;
			}
		}
	} // end of readLine

	/**
	 * Skip over fields you don't want to process.
	 * 
	 * @param fields
	 *            How many field you want to bypass reading. The newline counts
	 *            as one field.
	 * @exception EOFException
	 *                at end of file after all the fields have been read.
	 * @exception IOException
	 *                Some problem reading the file, possibly malformed data.
	 */
	public void skip(final int fields) throws EOFException, IOException {
		if (fields <= 0) {
			return;
		}
		for (int i = 0; i < fields; i++) {
			// throw results away
			get();
		}
	} // end of skip

	/**
	 * Skip over remaining fields on this line you don't want to process.
	 * 
	 * @exception EOFException
	 *                at end of file after all the fields have been read.
	 * @exception IOException
	 *                Some problem reading the file, possibly malformed data.
	 */
	public void skipToNextLine() throws EOFException, IOException {
		if (line == null) {
			readLine();
		}
		line = null;
	} // end of skipToNextLine

	/**
	 * Close the Reader.
	 */
	public void close() throws IOException {
		if (m_reader != null) {
			m_reader.close();
			m_reader = null;
		}
	}
}
