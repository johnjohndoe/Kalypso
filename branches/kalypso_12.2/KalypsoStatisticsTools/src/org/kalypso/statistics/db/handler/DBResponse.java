package org.kalypso.statistics.db.handler;

public class DBResponse {

	public static enum EDBResponseType {
		OK, WARNING, ERROR
	}

	private final EDBResponseType m_responseType;
	private final String m_message;
	private final Integer m_recordUID;

	public DBResponse(final EDBResponseType responseType, final String message) {
		m_responseType = responseType;
		m_message = message;
		m_recordUID = null;
	}

	public DBResponse(final EDBResponseType responseType, final String message, final Integer recordUID) {
		m_responseType = responseType;
		m_message = message;
		m_recordUID = recordUID;
	}

	public EDBResponseType getResponseType() {
		return m_responseType;
	}

	public String getMessage() {
		return hasMessage() ? m_message : "";
	}

	public boolean hasMessage() {
		return m_message != null;
	}

	public boolean isOK() {
		return m_responseType == EDBResponseType.OK;
	}

	public static DBResponse OK() {
		return new DBResponse(EDBResponseType.OK, null, null);
	}

	public static DBResponse OK(final Integer recordUID) {
		return new DBResponse(EDBResponseType.OK, null, recordUID);
	}

	public Integer getRecordUID() {
		return m_recordUID;
	}
}
