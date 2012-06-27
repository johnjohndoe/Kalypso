package org.kalypso.statistics.db.datasource;

import java.sql.Connection;
import java.sql.SQLException;

import org.eclipse.core.runtime.IProgressMonitor;

import de.renew.workflow.connector.cases.IScenario;

public interface IDataSource {

	public void startDataSource(final IScenario scenario);

	public void stopDataSource();

	public Connection getConnection() throws SQLException;

	public boolean isDataSourceInitialized();

	public boolean isDataSourceUpdateAvailable();

	public void initDataSource();

	public void initDataSource(final IProgressMonitor monitor);

	public void updateDataSource();

	public void updateDataSource(final IProgressMonitor monitor);

	public void setDataSourceErrorOccured();

	public void resetDataSourceErrorFlag();

	public boolean isDataSourceErrorOccured();

	public boolean isDataSourceRunning();

	public String getDatabaseName();

}
