package org.kalypso.statistics.logger;

public class ConsoleLogger implements ISysLogger {

	@Override
	public void logInfo(final String message) {
		System.out.println(message);
	}

	@Override
	public void logWarning(final String message) {
		System.err.println(message);
	}

	@Override
	public void logError(final String message) {
		System.err.println(message);
	}

	@Override
	public void logException(final Throwable exception) {
		System.err.println(exception.getMessage());
		exception.printStackTrace();
	}

}
