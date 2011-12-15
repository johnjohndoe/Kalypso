package org.kalypso.statistics.db.handler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;

import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.types.ENodeProfileType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.utils.AppUtils;

/**
 * This class will provide the collection of scenarios.
 * 
 */
public class DBHandlerNodeProfile extends AbstractRecordDBHandler<NodeProfile> {

	public DBHandlerNodeProfile() {
		super("NodeProfile"); //$NON-NLS-1$
		loadRecords();
	}

	@Override
	protected LinkedHashMap<String, Object> getParamsMap(final NodeProfile p) {
		final LinkedHashMap<String, Object> paramsMap = new LinkedHashMap<String, Object>();
		paramsMap.put("name", p.getName()); //$NON-NLS-1$
		paramsMap.put("description", p.getDescription()); //$NON-NLS-1$
		paramsMap.put("profileType", p.getNodeProfileType().name()); //$NON-NLS-1$
		return paramsMap;
	}

	public void reloadProfiles() {
		loadRecords();
	}

	@Override
	protected void loadRecords() {
		getRecords().clear();
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final PreparedStatement stmt = connection.prepareStatement("SELECT * FROM NodeProfile ORDER BY name ASC");
			final ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				int UID = rs.getInt("UID");
				String name = rs.getString("name");
				String description = rs.getString("description");
				ENodeProfileType profileType = ENodeProfileType.valueOf(rs.getString("profileType"));
				final NodeProfile p = new NodeProfile(UID, name, description, profileType);
				getRecords().add(p);
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
