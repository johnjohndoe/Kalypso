package org.kalypso.statistics.db.datasource;

public class DataSourceCommand {

	private final TYPE m_type;
	private final String m_command;

	public static String COMMENT = "/"; //$NON-NLS-1$
	public static String SQL_SEPARATOR = ";"; //$NON-NLS-1$

	public static enum TYPE {
		SQL, STATISTICS
	}

	public DataSourceCommand(final TYPE type, final String command) {
		m_type = type;
		m_command = command;
	}

	public TYPE getType() {
		return m_type;
	}

	public String getCommand() {
		return m_command;
	}
}
