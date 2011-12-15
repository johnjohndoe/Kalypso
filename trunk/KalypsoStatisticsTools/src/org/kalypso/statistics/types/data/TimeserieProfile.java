package org.kalypso.statistics.types.data;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.ETimeseriesType;

public class TimeserieProfile extends AbstractStatisticsRecordType {

	private String m_name = EMPTY_STRING;
	private String m_description = EMPTY_STRING;
	private Calendar m_timeFrom = Calendar.getInstance();
	private Calendar m_timeTo = Calendar.getInstance();
	private ETimeseriesType m_timeseriesType;
	private int m_nodeProfileUID = 0;
	private long m_timestepMillis = 0;
	private int m_numberOfEntries = 0;
	private boolean m_preprocessed = false;

	private final List<TimeserieProfileEntry> m_entries = new ArrayList<TimeserieProfileEntry>();

	public TimeserieProfile(ETimeseriesType timeseriesType) {
		this(0, null, timeseriesType);
	}

	public TimeserieProfile(int UID, String name, ETimeseriesType timeseriesType) {
		super();
		setUID(UID);
		setName(name);
		setTimeseriesType(timeseriesType);
	}

	public static TimeserieProfile clone(TimeserieProfile profileToClone, boolean withUID, boolean withEntries) {
		TimeserieProfile profile = new TimeserieProfile(profileToClone.getTimeseriesType());
		profile.setName(profileToClone.getName());
		profile.setDescription(profileToClone.getDescription());
		profile.setTimeFrom(profileToClone.getTimeFrom());
		profile.setTimeTo(profileToClone.getTimeTo());
		profile.setNodeProfileUID(profileToClone.getNodeProfileUID());
		profile.setNumberOfEntries(profileToClone.getNumberOfEntries());
		profile.setTimestepMillis(profileToClone.getTimestepMillis());
		profile.setPreprocessed(profileToClone.isPreprocessed());
		profile.setUID(withUID ? profileToClone.getUID() : 0);
		if (withEntries) {
			profile.getEntries().addAll(profileToClone.getEntries());
		}
		return profile;
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
	 * @return the entries
	 */
	public List<TimeserieProfileEntry> getEntries() {
		return m_entries;
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
		m_description = description;
	}

	/**
	 * @return the timeFrom
	 */
	public Calendar getTimeFrom() {
		return m_timeFrom;
	}

	/**
	 * @param timeFrom
	 *            the timeFrom to set
	 */
	public void setTimeFrom(Calendar timeFrom) {
		m_timeFrom = timeFrom == null ? Calendar.getInstance() : timeFrom;
	}

	/**
	 * @return the timeTo
	 */
	public Calendar getTimeTo() {
		return m_timeTo;
	}

	/**
	 * @param timeTo
	 *            the timeTo to set
	 */
	public void setTimeTo(Calendar timeTo) {
		m_timeTo = timeTo == null ? Calendar.getInstance() : timeTo;
	}

	/**
	 * @return the timeseriesType
	 */
	public ETimeseriesType getTimeseriesType() {
		return m_timeseriesType;
	}

	/**
	 * @param timeseriesType
	 *            the timeseriesType to set
	 */
	public void setTimeseriesType(ETimeseriesType timeseriesType) {
		if (timeseriesType == null) {
			m_timeseriesType = ETimeseriesType.UNKNOWN;
		} else {
			m_timeseriesType = timeseriesType;
		}
	}

	/**
	 * @return the nodeProfileUID
	 */
	public int getNodeProfileUID() {
		return m_nodeProfileUID;
	}

	/**
	 * @param nodeProfileUID
	 *            the nodeProfileUID to set
	 */
	public void setNodeProfileUID(int nodeProfileUID) {
		m_nodeProfileUID = 0;
		for (NodeProfile node : SessionDataProvider.getInstance().getDataProvider().getNodes()) {
			if (nodeProfileUID == node.getUID()) {
				m_nodeProfileUID = nodeProfileUID;
				break;
			}
		}
	}

	/**
	 * @return the nodeProfileName
	 */
	public String getNodeProfileName() {
		if (getNodeProfileUID() <= 0) {
			return EMPTY_STRING;
		}
		for (NodeProfile node : SessionDataProvider.getInstance().getDataProvider().getNodes()) {
			if (getNodeProfileUID() == node.getUID()) {
				return node.getName();
			}
		}
		return EMPTY_STRING;
	}

	/**
	 * @return the timestepMillis
	 */
	public long getTimestepMillis() {
		return m_timestepMillis;
	}

	/**
	 * @param timestepMillis
	 *            the timestepMillis to set
	 */
	public void setTimestepMillis(long timestepMillis) {
		m_timestepMillis = timestepMillis;
	}

	/**
	 * @return the numberOfEntries
	 */
	public int getNumberOfEntries() {
		return m_numberOfEntries;
	}

	/**
	 * @param numberOfEntries
	 *            the numberOfEntries to set
	 */
	public void setNumberOfEntries(int numberOfEntries) {
		m_numberOfEntries = numberOfEntries;
	}

	/**
	 * @return the preprocessed
	 */
	public boolean isPreprocessed() {
		return m_preprocessed;
	}

	/**
	 * @param preprocessed
	 *            the preprocessed to set
	 */
	public void setPreprocessed(boolean preprocessed) {
		m_preprocessed = preprocessed;
	}

}
