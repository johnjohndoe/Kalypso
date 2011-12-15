package org.kalypso.statistics.db.datasource;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.statistics.db.handler.DBHandlerSysVars;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.SQLUtils;
import org.kalypso.statistics.utils.WorkspaceUtils;
import org.kalypso.statistics.utils.WorkspaceUtils.SYSVARS;

public abstract class AbstractDataSourceDerby implements IDataSource {

	private static final String DB_SCHEMA = "STATISTICS"; //$NON-NLS-1$

	private boolean GENERAL_ERROR = false;
	private boolean IS_DATASOURCE_INITIALIZED = false;
	private boolean IS_DATASOURCE_RUNNING = false;
	private boolean ANOTHER_INSTANCE_IS_RUNNING = false;

//	private final boolean m_dbStateReparationJobScheduled = false;

	private final List<String> m_sqlUpdates = new ArrayList<String>();

	protected AbstractDataSourceDerby() {
		// singleton!
	}

	@Override
	public abstract void startDataSource(final IScenario scenario);

	@Override
	public abstract void stopDataSource();

	protected abstract Connection getRawConnection() throws SQLException;

	protected abstract String getFolderPathCreateDB();

	protected abstract String getFolderPathInitDB();

	@Override
	public void resetDataSourceErrorFlag() {
		GENERAL_ERROR = false;
	}

	@Override
	public Connection getConnection() throws SQLException {
		if (GENERAL_ERROR) {
			throw new SQLException("General database error");
		}
		final Connection connection = getRawConnection();
		final Statement statement = connection.createStatement();
		statement.execute(String.format("SET SCHEMA '%s'", DB_SCHEMA)); //$NON-NLS-1$
		statement.close();
		return connection;
	}

	@Override
	public final boolean isDataSourceInitialized() {
		if (GENERAL_ERROR)
			return false;
		if (IS_DATASOURCE_INITIALIZED)
			return true;
		Connection conn = null;
		Statement stmt = null;

		try {
			conn = getRawConnection();

			stmt = conn.createStatement();
			final String sql = String.format("SELECT * FROM SYS.SYSSCHEMAS WHERE SCHEMANAME = '%s'", DB_SCHEMA); //$NON-NLS-1$
			final ResultSet query = stmt.executeQuery(sql);
			if (query.next()) {
				IS_DATASOURCE_INITIALIZED = true;
			}
			stmt.close();
		} catch (final SQLException e) {
			IS_DATASOURCE_INITIALIZED = false;
			GENERAL_ERROR = true;
			AppUtils.getLogger().logException(e);
		} finally {
			if (conn != null)
				try {
					conn.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
		return IS_DATASOURCE_INITIALIZED;
	}

	@Override
	public final void initDataSource() {
		initDataSource(new NullProgressMonitor());
	}

	@Override
	public final void initDataSource(final IProgressMonitor monitor) {
		if (GENERAL_ERROR || IS_DATASOURCE_INITIALIZED)
			return;
		final List<String[]> sqlFilesList = new ArrayList<String[]>();
		sqlFilesList.add(new String[] { "Creating data structure...", getFolderPathCreateDB() + "/createDatabase.sql" }); //$NON-NLS-1$ 
		sqlFilesList.add(new String[] { "Initializing database...", getFolderPathInitDB() + "/initDatabase.sql" }); //$NON-NLS-1$ 

		Connection conn = null;

		try {
			conn = getRawConnection();

			final Statement stmt = conn.createStatement();
			stmt.execute(String.format("CREATE SCHEMA \"%s\"", DB_SCHEMA)); //$NON-NLS-1$
			stmt.execute(String.format("SET SCHEMA \"%s\"", DB_SCHEMA)); //$NON-NLS-1$

			for (final String[] entry : sqlFilesList) {
				monitor.subTask(entry[0]);
				final InputStream inputStream = WorkspaceUtils.getResourceStream(entry[1]);
				final List<DataSourceCommand> sqlCommands = SQLUtils.readSqlFile(inputStream);
				for (final DataSourceCommand command : sqlCommands) {
					if (!DataSourceCommand.TYPE.SQL.equals(command.getType())) {
						throw new SQLException(String.format("Initialization file '%s' contains non-SQL command: '%s %s'.", entry, command.getType().name(), command.getCommand()));
					}
					// System.out.println(command.getCommand());
					stmt.execute(command.getCommand());
					monitor.worked(10);
				}
			}
			stmt.close();
			conn.close();
			WorkspaceUtils.initLocation();
		} catch (final SQLException e) {
			GENERAL_ERROR = true;
			AppUtils.getLogger().logException(e);
		} catch (final Exception e) {
			// OBJAVI GRESKU, nepoznat uzrok, ne moze da inicijalizuje bazu
			GENERAL_ERROR = true;
			AppUtils.getLogger().logException(e);
		} finally {
			if (conn != null)
				try {
					conn.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
		// set update info
		final Map<String, String> sysVars = new HashMap<String, String>();
		final String lastUpdate = String.format("%1$tY%1$tm%1$td%1$tH%1$tM", Calendar.getInstance()); //$NON-NLS-1$
		sysVars.put(SYSVARS.SYSSQLUPDATETIME.name(), lastUpdate);
		DBHandlerSysVars.saveVars(sysVars);

		monitor.done();
	}

	@Override
	public boolean isDataSourceUpdateAvailable() {
		if (GENERAL_ERROR || !IS_DATASOURCE_INITIALIZED) {
			m_sqlUpdates.clear();
			return false;
		}
		final String updateFile = getFolderPathInitDB() + "/update.sql"; //$NON-NLS-1$
		final Map<String, String> sysVars = new HashMap<String, String>();
		DBHandlerSysVars.loadVars(sysVars);
		final String val = sysVars.get(SYSVARS.SYSSQLUPDATETIME.name());
		AppUtils.getLogger().logInfo("Last DB update stamp: " + val);
		final long sysLastUpdate = val == null ? 0 : Long.parseLong(val);

		final InputStream resourceStream = WorkspaceUtils.getResourceStream(updateFile);

		try {
			final List<DataSourceCommand> rawCommands = SQLUtils.readSqlFile(resourceStream);
			if (rawCommands.size() == 0) {
				m_sqlUpdates.clear();
				return false;
			}
			boolean beginningDateFound = false;
			for (final DataSourceCommand dataSourceCommand : rawCommands) {
				if (beginningDateFound) {
					if (DataSourceCommand.TYPE.SQL.equals(dataSourceCommand.getType())) {
						m_sqlUpdates.add(dataSourceCommand.getCommand());
					}
				} else {
					if (DataSourceCommand.TYPE.STATISTICS.equals(dataSourceCommand.getType())) {
						final long updateDate = Long.parseLong(dataSourceCommand.getCommand());
						beginningDateFound = sysLastUpdate < updateDate;
						if (beginningDateFound) {
							sysVars.clear();
							sysVars.put(SYSVARS.SYSSQLUPDATETIME.name(), dataSourceCommand.getCommand());
							DBHandlerSysVars.saveVars(sysVars);
						}
					}
				}
			}
			return m_sqlUpdates.size() > 0;
		} catch (final NumberFormatException e) {
			Logger.getAnonymousLogger().log(Level.WARNING, String.format("'%s' improper content - unrecognisable timestep.", "update.sql")); //$NON-NLS-1$
			m_sqlUpdates.clear();
		} catch (final Exception e) {
			AppUtils.getLogger().logException(e);
			m_sqlUpdates.clear();
		}
		return false;
	}

	@Override
	public final void updateDataSource() {
		updateDataSource(new NullProgressMonitor());
	}

	@Override
	public final void updateDataSource(final IProgressMonitor monitor) {
		if (GENERAL_ERROR || !IS_DATASOURCE_INITIALIZED)
			return;
		if (m_sqlUpdates.size() == 0)
			return;
		Connection conn = null;
		Statement stmt = null;

		try {
			monitor.beginTask("Updating database...", IProgressMonitor.UNKNOWN);
			conn = getConnection();
			stmt = conn.createStatement();
			for (final String sql : m_sqlUpdates) {
				try {
					stmt.execute(sql);
				} catch (SQLException e) {
					AppUtils.getLogger().logError(sql);
					AppUtils.getLogger().logError(e.getMessage());
					AppUtils.getLogger().logException(e);
				}
			}
			stmt.close();
			conn.close();
		} catch (final SQLException e) {
			// GENERAL_ERROR = true;
			AppUtils.getLogger().logException(e);
		} finally {
			if (conn != null)
				try {
					conn.close();
				} catch (final SQLException e) {
					// ignore
				}
		}
	}

	@Override
	public void setDataSourceErrorOccured() {
		GENERAL_ERROR = true;
		// // try to repair...
		// if (!m_dbStateReparationJobScheduled) {
		// m_dbStateReparationJobScheduled = true;
		// Job job = new Job("") {
		// @Override
		// protected IStatus run(IProgressMonitor monitor) {
		// GENERAL_ERROR = false;
		// m_dbStateReparationJobScheduled = false;
		// return Status.OK_STATUS;
		// }
		// };
		// job.setSystem(true);
		// job.setPriority(Job.INTERACTIVE);
		// job.schedule(1000);
		// }
	}

	@Override
	public boolean isDataSourceErrorOccured() {
		return GENERAL_ERROR;
	}

	@Override
	public boolean isDataSourceRunning() {
		return IS_DATASOURCE_RUNNING;
	}

	protected void setAnotherDataSourceInstanceIsRunning() {
		ANOTHER_INSTANCE_IS_RUNNING = true;
	}

	public boolean isAnotherDataSourceInstanceIsRunning() {
		return ANOTHER_INSTANCE_IS_RUNNING;
	}

	protected void setDataSourceRunning(final boolean isDataSourceRunning) {
		IS_DATASOURCE_RUNNING = isDataSourceRunning;
	}

}
