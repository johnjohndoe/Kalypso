package org.kalypso.statistics.types.data;

import org.kalypso.statistics.types.ENodeProfileType;

public class NodeProfile extends AbstractStatisticsRecordType {

	private String m_name = EMPTY_STRING;
	private String m_description = EMPTY_STRING;
	private int m_numberOfTimeseries = 0;
	private ENodeProfileType m_nodeProfileType = ENodeProfileType.HYDROLOGICAL_NODE;

	public NodeProfile(int UID, String name) {
		this(UID, name, EMPTY_STRING, ENodeProfileType.UNKNOWN);
	}

	public NodeProfile(int UID, String name, ENodeProfileType type) {
		this(UID, name, EMPTY_STRING, type);
	}

	public NodeProfile(int UID, String name, String description) {
		this(UID, name, description, ENodeProfileType.UNKNOWN);
	}

	public NodeProfile(int UID, String name, String description, ENodeProfileType type) {
		super();
		setUID(UID);
		setName(name);
		setDescription(description);
		setNodeProfileType(type);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return m_name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		if (name == null || name.trim().length() == 0) {
			m_name = EMPTY_STRING;
		} else {
			m_name = name;
		}
	}

	/**
	 * @return the nodeProfileType
	 */
	public ENodeProfileType getNodeProfileType() {
		return m_nodeProfileType;
	}

	/**
	 * @param nodeProfileType
	 *            the nodeProfileType to set
	 */
	public void setNodeProfileType(ENodeProfileType nodeProfileType) {
		m_nodeProfileType = nodeProfileType;
	}

	/**
	 * @return the description
	 */
	public String getDescription() {
		return m_description;
	}

	/**
	 * @param description
	 *            the description to set
	 */
	public void setDescription(String description) {
		if (description == null || description.trim().length() == 0) {
			m_description = EMPTY_STRING;
		} else {
			m_description = description;
		}
	}

	/**
	 * @return the numberOfTimeseries
	 */
	public int getNumberOfTimeseries() {
		return m_numberOfTimeseries;
	}

	/**
	 * @param numberOfTimeseries the numberOfTimeseries to set
	 */
	public void setNumberOfTimeseries(int numberOfTimeseries) {
		m_numberOfTimeseries = numberOfTimeseries;
	}

}
