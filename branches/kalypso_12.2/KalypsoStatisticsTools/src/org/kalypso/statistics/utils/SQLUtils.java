package org.kalypso.statistics.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.LinkedHashMap;
import java.util.List;

import org.kalypso.statistics.db.datasource.DataSourceCommand;
import org.kalypso.statistics.types.ECodePage;

public class SQLUtils {

	public static SimpleDateFormat SQL_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd"); //$NON-NLS-1$

	public static final char COMMA = ',';
	public static final char EQUAL_SIGN = '=';

	private static final CharSequence ESCAPE = "\'"; //$NON-NLS-1$
	private static final CharSequence REPLACEMENT = "\'\'"; //$NON-NLS-1$

	private SQLUtils() {
		// do not instantiate
	}

	public static final String escapeArgument(final Object object) {
		if (object == null)
			return ""; //$NON-NLS-1$
		if (object instanceof Integer) {
			return Integer.toString((Integer) object);
		}
		if (object instanceof Double) {
			return Double.toString((Double) object);
		}
		if (object instanceof Boolean) {
			return ((Boolean) object).booleanValue() ? "1" : "0"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		// if (object instanceof Calendar) {
		//			return ((Calendar) object).booleanValue() ? "1" : "0"; //$NON-NLS-1$ //$NON-NLS-2$
		// }
		return escapeString(object.toString());
	}

	public static final String escapeString(final String string) {
		final StringBuffer buffer = new StringBuffer(ESCAPE);
		buffer.append(string.replace(ESCAPE, REPLACEMENT));
		buffer.append(ESCAPE);
		return buffer.toString();
	}

	public static PreparedStatement prepareInsertStatement(final Connection connection, final String tableName, final LinkedHashMap<String, Object> paramsMap)
			throws SQLException {
		final StringBuffer paramsBuffer = new StringBuffer();
		final StringBuffer valuesBuffer = new StringBuffer();
		for (final String key : paramsMap.keySet()) {
			paramsBuffer.append(key).append(COMMA);
			valuesBuffer.append("?").append(COMMA);
		}
		paramsBuffer.setLength(paramsBuffer.length() - 1);
		valuesBuffer.setLength(valuesBuffer.length() - 1);
		final PreparedStatement statement = connection
				.prepareStatement(String.format("INSERT INTO %s (%s) VALUES (%s) ", tableName, paramsBuffer.toString(), valuesBuffer.toString()),
						Statement.RETURN_GENERATED_KEYS);
		fillParams(statement, paramsMap);
		return statement;
	}

	public static PreparedStatement prepareUpdateStatement(final Connection connection, final String tableName, final String UIDfieldName, final int UIDvalue,
			final LinkedHashMap<String, Object> paramsMap) throws SQLException {
		// final StringBuffer paramsBuffer = new StringBuffer();
		// for (final String key : paramsMap.keySet()) {
		// paramsBuffer.append(key).append(EQUAL_SIGN).append(escapeArgument(paramsMap.get(key))).append(COMMA);
		// }
		// paramsBuffer.setLength(paramsBuffer.length() - 1);
		//		return String.format("UPDATE %s SET %s WHERE %s=%d", tableName, paramsBuffer.toString(), UIDfieldName, UIDvalue); //$NON-NLS-1$
		final StringBuffer paramsBuffer = new StringBuffer();
		for (final String key : paramsMap.keySet()) {
			paramsBuffer.append(key).append(EQUAL_SIGN).append("?").append(COMMA);
		}
		paramsBuffer.setLength(paramsBuffer.length() - 1);
		final PreparedStatement statement = connection.prepareStatement(String.format("UPDATE %s SET %s WHERE %s = %d", tableName, paramsBuffer.toString(),
				UIDfieldName, UIDvalue));
		fillParams(statement, paramsMap);
		return statement;
	}

	private static void fillParams(final PreparedStatement statement, final LinkedHashMap<String, Object> paramsMap) throws SQLException {
		int index = 0;
		for (final String key : paramsMap.keySet()) {
			final Object object = paramsMap.get(key);
			if (object == null)
				throw new SQLException("Param is null");
			if (object instanceof Integer) {
				statement.setInt(++index, (Integer) object);
			} else if (object instanceof Long) {
				statement.setLong(++index, (Long) object);
			} else if (object instanceof Double) {
				statement.setDouble(++index, (Double) object);
			} else if (object instanceof Boolean) {
				statement.setBoolean(++index, ((Boolean) object).booleanValue());
				// statement.setInt(++index, ((Boolean) object).booleanValue() ?
				// 1 : 0);
			} else if (object instanceof Calendar) {
				// do not use Calendar.getTimeInMillis() to create Timestamp!
				// Timestamp count millis from 01.01.1970, and Calendar counts
				// from epoch start!
				// statement.setTimestamp(++index, new Timestamp(((Calendar)
				// object).getTimeInMillis()));
				final Calendar calendar = (Calendar) object;
				statement.setTimestamp(++index, new Timestamp(calendar.getTime().getTime()));
			} else
				statement.setString(++index, object.toString());
		}
	}

	public static Calendar createCalendar(final Timestamp timestamp) {
		return createCalendar(timestamp.getTime());
	}

	public static Calendar createCalendar(final long timestampMillis) {
		// final Calendar calendar = Calendar.getInstance();
		final Calendar calendar = new GregorianCalendar();
		calendar.setTimeInMillis(timestampMillis);
		return calendar;
	}

	public static Calendar createCalendar() {
		return createCalendar(System.currentTimeMillis());
	}

	public static void rollbackAndCloseQuietly(final Connection connection) {
		if (connection != null)
			try {
				if (!connection.isClosed()) {
					if (!connection.getAutoCommit()) {
						connection.rollback();
						connection.setAutoCommit(true);
					}
					connection.close();
				}
			} catch (final SQLException e) {
				// ignore
			}
	}

	public static void closeQuietly(final Connection connection) {
		if (connection != null)
			try {
				if (!connection.isClosed())
					connection.close();
			} catch (final SQLException e) {
				// ignore
				System.out.println("CAUNTION: CANNOT CLOSE CONNECTION");
			}
	}

	/**
	 * Returns the list of SQL and/or EXPEDIT commands
	 * 
	 * @param resourceStream
	 * @return
	 * @throws IOException
	 */
	public static List<DataSourceCommand> readSqlFile(final InputStream resourceStream) throws IOException {
		final List<DataSourceCommand> commands = new ArrayList<DataSourceCommand>();
		final StringBuffer sb = new StringBuffer();
		if (resourceStream == null)
			return commands;
		String s = new String();
		final BufferedReader br = new BufferedReader(new InputStreamReader(resourceStream, ECodePage.UTF8.getCharset()));
		while ((s = br.readLine()) != null) {
			s = s.trim();
			if (s.length() == 0)
				continue;
			if (s.startsWith(DataSourceCommand.COMMENT))
				continue;
			if (s.startsWith(DataSourceCommand.TYPE.STATISTICS.name())) {
				final String cmd = s.substring(DataSourceCommand.TYPE.STATISTICS.name().length()).trim();
				commands.add(new DataSourceCommand(DataSourceCommand.TYPE.STATISTICS, cmd));
				continue;
			}
			sb.append(s);
			if (s.endsWith(DataSourceCommand.SQL_SEPARATOR)) {
				// exclude separator
				sb.setLength(sb.length() - DataSourceCommand.SQL_SEPARATOR.length());
				final String sql = sb.toString().trim();
				if (sql.length() > 0) {
					commands.add(new DataSourceCommand(DataSourceCommand.TYPE.SQL, sql));
				}
				sb.setLength(0);
				continue;
			}
		}
		br.close();
		return commands;
	}

}
