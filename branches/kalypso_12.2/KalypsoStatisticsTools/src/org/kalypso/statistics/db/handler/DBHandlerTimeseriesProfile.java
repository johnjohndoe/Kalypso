package org.kalypso.statistics.db.handler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.LinkedHashMap;

import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.SQLUtils;

/**
 * This class will provide the collection of scenarios.
 * 
 */
public class DBHandlerTimeseriesProfile extends AbstractRecordDBHandler<TimeserieProfile> {

	public DBHandlerTimeseriesProfile() {
		super("TimeSerieProfile"); //$NON-NLS-1$
		loadRecords();
	}

	@Override
	protected LinkedHashMap<String, Object> getParamsMap(final TimeserieProfile p) {
		final LinkedHashMap<String, Object> paramsMap = new LinkedHashMap<String, Object>();
		paramsMap.put("name", p.getName()); //$NON-NLS-1$
		paramsMap.put("description", p.getDescription()); //$NON-NLS-1$
		paramsMap.put("profileType", p.getTimeseriesType().name()); //$NON-NLS-1$
		paramsMap.put("nodeProfile_UID", p.getNodeProfileUID()); //$NON-NLS-1$
		paramsMap.put("beginStamp", p.getTimeFrom()); //$NON-NLS-1$
		paramsMap.put("endStamp", p.getTimeTo()); //$NON-NLS-1$
		paramsMap.put("timeStepMillis", p.getTimestepMillis()); //$NON-NLS-1$
		if (p.getEntries().size() == 0)
			paramsMap.put("numberOfEntries", p.getNumberOfEntries()); //$NON-NLS-1$
		else
			paramsMap.put("numberOfEntries", p.getEntries().size()); //$NON-NLS-1$
		return paramsMap;
	}

	@Override
	protected void loadRecords() {
		getRecords().clear();
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final PreparedStatement stmt = connection.prepareStatement("SELECT * FROM TimeSerieProfile ORDER BY name ASC");
			final ResultSet rs = stmt.executeQuery();
			_internalLoadRecords(rs);
			stmt.close();
		} catch (final SQLException e) {
			DataSourceManager.getDataSource().setDataSourceErrorOccured();
			AppUtils.getLogger().logException(e);
		} finally {
			if (connection != null)
				try {
					connection.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
	}

	public void loadRecordsForNode(NodeProfile node) {
		getRecords().clear();
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			// final PreparedStatement stmt =
			// connection.prepareStatement("SELECT * FROM TimeSerieProfile WHERE node taj i taj = ? ORDER BY name ASC");
			// stmt.setBoolean(1, false);
			final PreparedStatement stmt = connection.prepareStatement("SELECT * FROM TimeSerieProfile WHERE nodeProfile_UID = ? ORDER BY name ASC");
			stmt.setInt(1, node.getUID());
			final ResultSet rs = stmt.executeQuery();
			_internalLoadRecords(rs);
			stmt.close();
		} catch (final SQLException e) {
			DataSourceManager.getDataSource().setDataSourceErrorOccured();
			AppUtils.getLogger().logException(e);
		} finally {
			if (connection != null)
				try {
					connection.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
	}

	private void _internalLoadRecords(final ResultSet rs) throws SQLException {
		while (rs.next()) {
			int UID = rs.getInt("UID");
			int nodeProfile_UID = rs.getInt("nodeProfile_UID");
			int timeStepMillis = rs.getInt("timeStepMillis");
			int numberOfEntries = rs.getInt("numberOfEntries");
			String name = rs.getString("name");
			String description = rs.getString("description");
			ETimeseriesType type = ETimeseriesType.valueOf(rs.getString("profileType"));
			Calendar calendarFrom = SQLUtils.createCalendar(rs.getTimestamp("beginStamp"));
			Calendar calendarTo = SQLUtils.createCalendar(rs.getTimestamp("endStamp"));
			final TimeserieProfile p = new TimeserieProfile(UID, name, type);
			p.setDescription(description);
			p.setTimeFrom(calendarFrom);
			p.setTimeTo(calendarTo);
			p.setNodeProfileUID(nodeProfile_UID);
			p.setTimestepMillis(timeStepMillis);
			p.setNumberOfEntries(numberOfEntries);
			getRecords().add(p);
		}
	}

	@Override
	protected DBResponse postSaveRecord(Connection connection, TimeserieProfile copy) throws SQLException {
		// save timeserie entries as well
		int UID = copy.getUID();
		PreparedStatement deleteStmt = connection.prepareStatement("DELETE FROM TimeSerieEntry WHERE timeserieProfile_UID = ?");
		deleteStmt.setInt(1, copy.getUID());
		deleteStmt.executeUpdate();
		final PreparedStatement stmt = connection.prepareStatement("INSERT INTO TimeSerieEntry (timeserieProfile_UID, entryDate, entryValue) VALUES (?, ?, ?)");
		for (TimeserieProfileEntry e : copy.getEntries()) {
			stmt.clearParameters();
			stmt.setInt(1, UID);
			stmt.setLong(2, e.getTime().getTimeInMillis());
			stmt.setDouble(3, e.getValue());
			stmt.executeUpdate();
		}
		stmt.close();
		copy.getEntries().clear();
		// re-sort entries, by reloading (and get their UIDs)
		// loadEntries(connection, copy);
		return DBResponse.OK();
	}

	public void loadEntries(Connection connection, TimeserieProfile timeserieProfile) throws SQLException {
		timeserieProfile.getEntries().clear();
		final PreparedStatement stmt = connection
				.prepareStatement("SELECT UID, entryDate, entryValue FROM TimeSerieEntry WHERE timeserieProfile_UID = ? ORDER BY entryDate ASC");
		stmt.setInt(1, timeserieProfile.getUID());
		final ResultSet rs = stmt.executeQuery();
		while (rs.next()) {
			int UID = rs.getInt("UID");
			Calendar entryDate = Calendar.getInstance();
			entryDate.setTimeInMillis(rs.getLong("entryDate"));
			double entryValue = rs.getDouble("entryValue");
			TimeserieProfileEntry entry = new TimeserieProfileEntry(UID, entryDate, entryValue);
			timeserieProfile.getEntries().add(entry);
		}
		stmt.close();
	}

	public static void loadEntries(TimeserieProfile timeserieProfile) {
		timeserieProfile.getEntries().clear();
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final PreparedStatement stmt = connection
					.prepareStatement("SELECT UID, entryDate, entryValue FROM TimeSerieEntry WHERE timeserieProfile_UID = ? ORDER BY entryDate ASC");
			stmt.setInt(1, timeserieProfile.getUID());
			final ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				int UID = rs.getInt("UID");
				Calendar entryDate = Calendar.getInstance();
				entryDate.setTimeInMillis(rs.getLong("entryDate"));
				double entryValue = rs.getDouble("entryValue");
				TimeserieProfileEntry entry = new TimeserieProfileEntry(UID, entryDate, entryValue);
				timeserieProfile.getEntries().add(entry);
			}
			stmt.close();
		} catch (final SQLException e) {
			DataSourceManager.getDataSource().setDataSourceErrorOccured();
			AppUtils.getLogger().logException(e);
		} finally {
			if (connection != null)
				try {
					connection.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
	}

}
