package org.kalypso.statistics.db.datasource;

import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLNonTransientConnectionException;

import org.apache.derby.jdbc.EmbeddedConnectionPoolDataSource;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.statistics.utils.AppUtils;

import de.renew.workflow.connector.cases.IScenario;

/**
 * Derby DataSource. Singleton!
 * 
 * @author Dejan Antanaskovic, AntSoft
 * 
 */
public class DataSourceDerbyEmbedded extends AbstractDataSourceDerby {

	private static DataSourceDerbyEmbedded INSTANCE = null;

	private final static String DB_NAME = "database"; //$NON-NLS-1$
	private final static String DB_USER = "stats"; //$NON-NLS-1$
	private final static String DB_PASSWORD = "kalypsoStats"; //$NON-NLS-1$

	private final static String DB_CREATE_PATH = "/resources/datasource/derby"; //$NON-NLS-1$
	private final static String DB_INITSQL_PATH = "/resources/datasource/derby"; //$NON-NLS-1$

	private EmbeddedConnectionPoolDataSource m_dataSource;

	private IScenario m_scenario = null;

	private DataSourceDerbyEmbedded() {
		// singleton!
	}

	protected static synchronized IDataSource getInstance() {
		if (INSTANCE == null)
			INSTANCE = new DataSourceDerbyEmbedded();
		return INSTANCE;
	}

	@Override
	public void startDataSource(final IScenario scenario) {
		m_scenario = scenario;
		_internalStartDataSource();
	}

	private void _internalStartDataSource() {
		if (m_scenario == null) {
			setDataSourceErrorOccured();
			setDataSourceRunning(false);
			return;
		}
		if (isDataSourceErrorOccured() || isDataSourceRunning())
			return;
		m_dataSource = new EmbeddedConnectionPoolDataSource();
		try {
			m_dataSource.setDatabaseName(getDataSourcePath());
			m_dataSource.setUser(DB_USER);
			m_dataSource.setPassword(DB_PASSWORD);
			m_dataSource.setCreateDatabase("create"); //$NON-NLS-1$
			m_dataSource.getPooledConnection();
			setDataSourceRunning(true);
		} catch (final SQLException e) {
			final SQLException nextException = e.getNextException();
			if (nextException != null) {
				if (nextException.getSQLState().equals("XSDB6")) { //$NON-NLS-1$
					setAnotherDataSourceInstanceIsRunning();
				}
			}
			setDataSourceErrorOccured();
			setDataSourceRunning(false);
			AppUtils.getLogger().logException(e);
		} catch (Exception e) {
			setDataSourceErrorOccured();
			setDataSourceRunning(false);
			AppUtils.getLogger().logException(e);
		}
	}

	@Override
	public void stopDataSource() {
		if (!isDataSourceRunning())
			return;
		// try {
		// final Statement statement = getRawConnection().createStatement();
		// statement.execute("CALL SYSCS_UTIL.SYSCS_COMPRESS_TABLE()");
		// statement.execute("CALL SYSCS_UTIL.SYSCS_UPDATE_STATISTICS('EXPEDIT', 'SHIPMENT', null)");
		// do NOT freeze database before backup, it will block it!
		// statement.execute("CALL SYSCS_UTIL.SYSCS_UNFREEZE_DATABASE()");
		// statement.execute("CALL SYSCS_UTIL.SYSCS_BACKUP_DATABASE('D:\\Private\\Expedit')");
		// statement.execute("CALL SYSCS_UTIL.SYSCS_UNFREEZE_DATABASE()");
		// statement.close();
		// } catch (final SQLException e1) {
		// e1.printStackTrace();
		// }

		m_dataSource.setShutdownDatabase("shutdown"); //$NON-NLS-1$
		try {
			// m_dataSource.setShutdownDatabase(""); //$NON-NLS-1$
			m_dataSource.getConnection();
		} catch (final SQLNonTransientConnectionException e) {
			// e.printStackTrace();
		} catch (final SQLException e) {
			setDataSourceErrorOccured();
			AppUtils.getLogger().logException(e);
		} finally {
			setDataSourceRunning(false);
		}
	}

	@Override
	protected Connection getRawConnection() throws SQLException {
		if (!isDataSourceRunning()) {
			_internalStartDataSource();
		}
		return m_dataSource.getPooledConnection().getConnection();
	}

	private final String getDataSourcePath() throws CoreException {
		final File folder = new File(m_scenario.getFolder().getFile(DB_NAME).getLocationURI());
		return folder.getAbsolutePath();
	}

	@Override
	protected String getFolderPathCreateDB() {
		return DB_CREATE_PATH;
	}

	@Override
	protected String getFolderPathInitDB() {
		return DB_INITSQL_PATH;
	}

	@Override
	public String getDatabaseName() {
		return DB_NAME;
	}

}
