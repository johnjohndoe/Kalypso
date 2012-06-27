package org.kalypso.statistics.logger;

import org.kalypso.statistics.i18n.Messages;

public interface ISysLogger {

	public static final String FORMAT = Messages.ISysLogger_MESSAGE_FORMAT;

	public static final String END_OF_LINE = System.getProperty("line.separator");

	public void logInfo(final String message);

	public void logWarning(final String message);

	public void logError(final String message);

	public void logException(final Throwable exception);
}
