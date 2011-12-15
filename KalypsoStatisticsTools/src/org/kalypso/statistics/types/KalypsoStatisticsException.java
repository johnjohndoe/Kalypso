package org.kalypso.statistics.types;

public class KalypsoStatisticsException extends Exception {

	private static final long serialVersionUID = 3278606816891980652L;
	private final String m_message;

	public KalypsoStatisticsException(final String message) {
		m_message = message;
	}

	@Override
	public String getMessage() {
		return m_message;
	}

}
