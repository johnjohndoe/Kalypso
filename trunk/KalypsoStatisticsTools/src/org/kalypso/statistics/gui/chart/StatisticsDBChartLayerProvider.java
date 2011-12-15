package org.kalypso.statistics.gui.chart;

import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.utils.AppUtils;

import de.openali.odysseus.chart.factory.provider.AbstractLayerProvider;
import de.openali.odysseus.chart.framework.model.exception.ConfigurationException;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IParameterContainer;

public class StatisticsDBChartLayerProvider extends AbstractLayerProvider {

	public static final String ID = StatisticsDBChartLayerProvider.class.getCanonicalName();

	@Override
	public IChartLayer getLayer(URL context) throws ConfigurationException {
		return new StatisticsDateValueLineLayer(this, getDataContainer(), getStyleSet());
	}

	/**
	 * @see org.kalypso.chart.factory.provider.ILayerProvider#getDataContainer()
	 */
	public TupleResultDomainValueData<?, ?> getDataContainer() {
		final IParameterContainer pc = getParameterContainer();

		final String type = pc.getParameterValue("type", null); //$NON-NLS-1$
		final String uidStr = pc.getParameterValue("uid", null); //$NON-NLS-1$
		final String fromStr = pc.getParameterValue("from", null); //$NON-NLS-1$
		final String toStr = pc.getParameterValue("to", null); //$NON-NLS-1$
		final String domainComponentName = pc.getParameterValue("domainComponentId", null); //$NON-NLS-1$
		final String targetComponentName = pc.getParameterValue("targetComponentId", null); //$NON-NLS-1$

		try {
			int uid = Integer.parseInt(uidStr);
			long from = Long.parseLong(fromStr);
			long to = Long.parseLong(toStr);
			IObservation<TupleResult> observation = createObservation(type, uid, from, to);
			if (observation != null && domainComponentName != null && targetComponentName != null) {
				return new TupleResultDomainValueData<Object, Object>(observation, domainComponentName, targetComponentName);
			}
		} catch (Exception e) {
		}
		return null;
	}

	private IObservation<TupleResult> createObservation(String type, int uid, long from, long to) {
		final TupleResult result = new TupleResult();
		final IComponent componentDateTime = new Component(ITimeseriesConstants.TYPE_DATE, "Date", null, "", null, XmlTypes.XS_LONG, null, null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		ETimeseriesType timeseriesType = ETimeseriesType.valueOf(type);
		final IComponent componentDouble = new Component(timeseriesType.getAbbreviation(), timeseriesType.getLabel(), null, timeseriesType.getUnit(), null,
				XmlTypes.XS_DOUBLE, null, null);
		result.addComponent(componentDateTime);
		result.addComponent(componentDouble);
		Connection connection = null;
		try {
			connection = DataSourceManager.getConnection();
			final PreparedStatement stmt = connection
					.prepareStatement("SELECT entryDate, entryValue FROM TimeSerieEntry WHERE timeserieProfile_UID = ? AND entryDate >= ? AND entryDate <= ? ORDER BY entryDate ASC");
			stmt.setInt(1, uid);
			stmt.setLong(2, from);
			stmt.setLong(3, to);
			final ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				final IRecord rec = result.createRecord();
				rec.setValue(0, rs.getLong("entryDate"));
				rec.setValue(1, rs.getDouble("entryValue"));
				result.add(rec);
			}
			stmt.close();
			return new Observation<TupleResult>("ime 1", "opis 1", result);
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
		return null;
	}

}
