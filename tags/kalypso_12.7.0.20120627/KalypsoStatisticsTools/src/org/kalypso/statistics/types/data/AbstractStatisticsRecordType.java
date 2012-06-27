package org.kalypso.statistics.types.data;

public abstract class AbstractStatisticsRecordType {

	public static final String EMPTY_STRING = "";

	private int m_UID = 0;

	/**
	 * @return the uID
	 */
	public int getUID() {
		return m_UID;
	}

	/**
	 * @param uID
	 *            the uID to set
	 */
	public void setUID(int uID) {
		m_UID = uID;
	}

}
