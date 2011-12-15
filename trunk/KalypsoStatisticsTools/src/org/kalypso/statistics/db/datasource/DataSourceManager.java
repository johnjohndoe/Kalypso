package org.kalypso.statistics.db.datasource;

import java.sql.Connection;
import java.sql.SQLException;

import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.WorkspaceUtils;

/**
 * DataSource manager.
 * 
 * @author Dejan Antanaskovic, AntSoft
 * 
 */
public class DataSourceManager {

	private static DataSourceType DATA_SOURCE_TYPE = DataSourceType.DERBY_EMBEDDED;

	public static enum DataSourceType {
		DERBY_EMBEDDED, //
		DERBY_NETWORK_SERVER
	}

	private DataSourceManager() {
		// Do not instantiate
	}

	public static void setDataSourceType(final DataSourceType dataSourceType) {
		// call only once, at the beginning!
		DATA_SOURCE_TYPE = dataSourceType;
	}

	public static DataSourceType getDataSourceType() {
		return DATA_SOURCE_TYPE;
	}

	public static IDataSource getDataSource() {
		switch (DATA_SOURCE_TYPE) {
		case DERBY_NETWORK_SERVER:
//			return DataSourceDerbyNetworkServer.getInstance();
		case DERBY_EMBEDDED:
		default:
			return DataSourceDerbyEmbedded.getInstance();
		}
	}

	public static Connection getConnection() throws SQLException {
		return getDataSource().getConnection();
	}

//	public static boolean backupDatabase() {
//		try {
//			WorkspaceUtils.backUpDatabase(getConnection());
//			return true;
//		} catch (final SQLException e) {
//			AppUtils.getLogger().logException(e);
//			return false;
//		}
//	}

}