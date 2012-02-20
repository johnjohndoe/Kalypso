package org.kalypso.statistics.db;

import java.util.List;

import org.kalypso.statistics.db.handler.AbstractRecordDBHandler;
import org.kalypso.statistics.db.handler.DBHandlerNodeProfile;
import org.kalypso.statistics.db.handler.DBHandlerTimeseriesProfile;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;

public class DataProvider {

	private DBHandlerNodeProfile m_dbHandlerNodeProfile = null;
	private DBHandlerTimeseriesProfile m_dbHandlerTimeseriesProfile = null;

	private static DataProvider INSTANCE = null;

	private DataProvider() {
	}

	public static DataProvider getInstance() {
		if (INSTANCE == null) {
			synchronized (DataProvider.class) {
				INSTANCE = new DataProvider();
			}
		}
		return INSTANCE;
	}

	public void loadHandlers() {
		setDbHandlerNodeProfile(new DBHandlerNodeProfile());
		setDbHandlerTimeseriesProfile(new DBHandlerTimeseriesProfile());
	}

	public final AbstractRecordDBHandler<?> getRecordDBHandler(final AbstractStatisticsRecordType instance) {
		if (instance instanceof NodeProfile) {
			return getDbHandlerNodeProfile();
		}
		if (instance instanceof TimeserieProfile) {
			return getDbHandlerTimeseriesProfile();
		}
		return null;
	}

	public AbstractRecordDBHandler<? extends AbstractStatisticsRecordType> getRecordDBHandler(final Class<? extends AbstractStatisticsRecordType> recordType) {
		final String className = recordType.getSimpleName();
		if (className.equals(NodeProfile.class.getSimpleName())) {
			return getDbHandlerNodeProfile();
		}
		if (className.equals(TimeserieProfile.class.getSimpleName())) {
			return getDbHandlerTimeseriesProfile();
		}
		return null;
	}

	public List<TimeserieProfile> getTimeseries() {
		return getDbHandlerTimeseriesProfile().getRecords();
//		// TODO Auto-generated method stub
//		ArrayList<TimeserieProfile> list = new ArrayList<TimeserieProfile>();
//		list.add(new TimeserieProfile(0, "Test 1 - Rainfall", ETimeseriesType.PRECIPITATION));
//		list.add(new TimeserieProfile(0, "Test 2 - Runoff", ETimeseriesType.RUNOFF));
//		list.add(new TimeserieProfile(0, "Test 3 - Waterlevel", ETimeseriesType.WATERLEVEL));
//		list.add(new TimeserieProfile(0, "Test 4 - Something else", null));
//		return list;
	}

	public List<NodeProfile> getNodes() {
		return getDbHandlerNodeProfile().getRecords();
	}
	
	/**
	 * @return the dbHandlerNodeProfile
	 */
	public DBHandlerNodeProfile getDbHandlerNodeProfile() {
		return m_dbHandlerNodeProfile;
	}

	/**
	 * @param dbHandlerNodeProfile
	 *            the dbHandlerNodeProfile to set
	 */
	private void setDbHandlerNodeProfile(DBHandlerNodeProfile dbHandlerNodeProfile) {
		m_dbHandlerNodeProfile = dbHandlerNodeProfile;
	}

	/**
	 * @return the dbHandlerTimeseriesProfile
	 */
	public DBHandlerTimeseriesProfile getDbHandlerTimeseriesProfile() {
		return m_dbHandlerTimeseriesProfile;
	}

	/**
	 * @param dbHandlerTimeseriesProfile the dbHandlerTimeseriesProfile to set
	 */
	private void setDbHandlerTimeseriesProfile(DBHandlerTimeseriesProfile dbHandlerTimeseriesProfile) {
		m_dbHandlerTimeseriesProfile = dbHandlerTimeseriesProfile;
	}

}
