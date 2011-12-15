package org.kalypso.statistics.db.handler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.utils.AppUtils;

/**
 * This class will be providing the collection of system variables.
 * 
 * @author Dejan Antanaskovic, AntSoft
 * 
 */
public class DBHandlerSysVars {

	private DBHandlerSysVars() {
	}

	public static void loadVars(final Map<String, String> varsMap) {
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final Statement statement = connection.createStatement();
			final String sql = "SELECT varname, varval FROM SysVars";
			final ResultSet rs = statement.executeQuery(sql);
			while (rs.next()) {
				varsMap.put(rs.getString(1), rs.getString(2));
			}
			statement.close();
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

	public static void saveVar(final String key, final String value) {
		final Map<String, String> map = new HashMap<String, String>();
		map.put(key, value);
		saveVars(map);
	}

	public static void saveVars(final Map<String, String> varsMap) {
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final PreparedStatement updateStmt = connection.prepareStatement("UPDATE SysVars SET varval = ? WHERE varname = ?");
			final PreparedStatement insertStmt = connection.prepareStatement("INSERT INTO SysVars (varname, varval) VALUES (?, ?)");
			for (final String key : varsMap.keySet()) {
				updateStmt.clearParameters();
				updateStmt.setString(1, varsMap.get(key));
				updateStmt.setString(2, key);
				final int cnt = updateStmt.executeUpdate();
				if (cnt == 0) {
					insertStmt.clearParameters();
					insertStmt.setString(1, key);
					insertStmt.setString(2, varsMap.get(key));
					insertStmt.executeUpdate();
				}
			}
			insertStmt.close();
			updateStmt.close();
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
