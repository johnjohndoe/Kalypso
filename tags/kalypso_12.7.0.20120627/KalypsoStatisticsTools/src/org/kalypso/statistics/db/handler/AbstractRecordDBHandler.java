package org.kalypso.statistics.db.handler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.db.handler.DBResponse.EDBResponseType;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.SQLUtils;

public abstract class AbstractRecordDBHandler<T extends AbstractStatisticsRecordType> {

	private static final String DB_TABLES_UID_FIELD = "UID"; //$NON-NLS-1$

	private final List<T> m_records = new ArrayList<T>();

	private final String m_dbTableName;

	private final boolean m_isModifiable;

	private T m_lastSavedRecord = null;

	public List<T> getRecords() {
		return m_records;
	}

	public AbstractRecordDBHandler(final String dbTableName) {
		m_dbTableName = dbTableName;
		m_isModifiable = getDbTableName() != null;
	}

	protected abstract LinkedHashMap<String, Object> getParamsMap(final T record);

	protected abstract void loadRecords();

	public final void reload() {
		loadRecords();
	}

	public T adapt(final AbstractStatisticsRecordType record) {
		return (T) record;
	}

	public DBResponse saveRecord(final AbstractStatisticsRecordType record) {
		Connection connection = null;
		try {
			final T copy = adapt(record);
			connection = DataSourceManager.getConnection();
			connection.setAutoCommit(false);
			DBResponse dbResponse = preSaveRecord(connection, copy);
			if (dbResponse.isOK()) {
				dbResponse = _internalSaveRecord(connection, copy);
				final int recordUID = dbResponse.getRecordUID();
				if (dbResponse.isOK()) {
					dbResponse = postSaveRecord(connection, copy);
					if (dbResponse.isOK()) {
						connection.commit();
						connection.setAutoCommit(true);
						m_lastSavedRecord = copy;
						return DBResponse.OK(recordUID);
					}
				}
			}
			connection.rollback();
			connection.setAutoCommit(true);
			return dbResponse;
		} catch (final Exception e) {
			SQLUtils.rollbackAndCloseQuietly(connection);
			AppUtils.getLogger().logException(e);
			// PartManager.getInstance().getLogger().logError(e.getMessage());
		} finally {
			SQLUtils.closeQuietly(connection);
		}
		return new DBResponse(EDBResponseType.ERROR, String.format("Data saving error [%s]", "SQLAB01"));
	}

	public DBResponse saveRecords(final List<? extends AbstractStatisticsRecordType> records) {
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			connection.setAutoCommit(false);
			for (final AbstractStatisticsRecordType record : records) {
				final T copy = adapt(record);
				DBResponse dbResponse = preSaveRecord(connection, copy);
				if (dbResponse.isOK()) {
					dbResponse = _internalSaveRecord(connection, copy);
					if (dbResponse.isOK()) {
						dbResponse = postSaveRecord(connection, copy);
						m_lastSavedRecord = copy;
						if (!dbResponse.isOK()) {
							connection.rollback();
							connection.setAutoCommit(true);
							return dbResponse;
						}
					} else {
						connection.rollback();
						connection.setAutoCommit(true);
						return dbResponse;
					}
				}
			}
			connection.commit();
			connection.setAutoCommit(true);
			return DBResponse.OK();
		} catch (final Exception e) {
			SQLUtils.rollbackAndCloseQuietly(connection);
			AppUtils.getLogger().logException(e);
		} finally {
			SQLUtils.closeQuietly(connection);
		}
		return new DBResponse(EDBResponseType.ERROR, String.format("Data saving error [%s]", "SQL002"));
	}

	protected DBResponse preSaveRecord(final Connection connection, final T copy) throws SQLException {
		return DBResponse.OK(null);
	}

	protected DBResponse postSaveRecord(final Connection connection, final T copy) throws SQLException {
		return DBResponse.OK();
	}

	protected DBResponse _internalSaveRecord(final Connection connection, final T copy) throws SQLException {
		if (!m_isModifiable)
			return new DBResponse(EDBResponseType.WARNING, String.format("Attempt to change non-modifiable record of type '%s'", copy.getClass()
					.getSimpleName()));
		DBResponse response;
		final LinkedHashMap<String, Object> paramsMap = getParamsMap(copy);
		if (!isExistingEntry(copy)) {
			final PreparedStatement statement = SQLUtils.prepareInsertStatement(connection, getDbTableName(), paramsMap);
			statement.executeUpdate();
			final ResultSet resultSet = statement.getGeneratedKeys();
			if (resultSet.next()) {
				copy.setUID(resultSet.getInt(1));
				response = DBResponse.OK(copy.getUID());
			} else {
				response = new DBResponse(EDBResponseType.ERROR, String.format("Data saving error [%s]", "SQL003"));
			}
			statement.close();
		} else {
			final PreparedStatement statement = SQLUtils.prepareUpdateStatement(connection, getDbTableName(), DB_TABLES_UID_FIELD, copy.getUID(), paramsMap);
			statement.executeUpdate();
			statement.close();
			response = DBResponse.OK(copy.getUID());
		}
		if (response.isOK())
			updateEntry(copy);
		return response;
	}

	protected DBResponse customDeleteRecord(final AbstractStatisticsRecordType record, final String sql) {
		if (!m_isModifiable)
			return new DBResponse(EDBResponseType.WARNING, String.format("Attempt to delete non-modifiable record of type '%s'", record.getClass()
					.getSimpleName()));
		if (record.getUID() > 0) {
			Connection connection = null;
			try {
				connection = DataSourceManager.getConnection();
				final Statement statement = connection.createStatement();
				statement.executeUpdate(sql);
				statement.close();
				deleteEntry(record.getUID());
				return DBResponse.OK();
			} catch (final SQLException e) {
				AppUtils.getLogger().logException(e);
				return new DBResponse(EDBResponseType.ERROR, String.format("Error deleting data [%s]", "SQL004"));
			} finally {
				SQLUtils.closeQuietly(connection);
			}
		} else {
			return new DBResponse(EDBResponseType.WARNING, "Attempt to delete non-existing record!");
		}
	}

	public DBResponse deleteRecord(final AbstractStatisticsRecordType record) {
		return customDeleteRecord(record, String.format("DELETE FROM %s WHERE UID=%d", getDbTableName(), record.getUID())); //$NON-NLS-1$
	}

	protected void updateEntry(final T copy) {
		final int id = copy.getUID();
		for (int i = 0; i < m_records.size(); i++) {
			if (id == m_records.get(i).getUID()) {
				m_records.remove(i);
				m_records.add(i, copy);
				return;
			}
		}
		m_records.add(copy);
	}

	private void deleteEntry(final int UID) {
		for (int i = 0; i < m_records.size(); i++) {
			if (UID == m_records.get(i).getUID()) {
				m_records.remove(i);
				return;
			}
		}
	}

	protected boolean isExistingEntry(final T copy) {
		final int id = copy.getUID();
		for (int i = 0; i < m_records.size(); i++) {
			if (id == m_records.get(i).getUID()) {
				return true;
			}
		}
		return false;
	}

	public String getDbTableName() {
		return m_dbTableName;
	}

	public T getLastSavedRecord() {
		return m_lastSavedRecord;
	}

}
