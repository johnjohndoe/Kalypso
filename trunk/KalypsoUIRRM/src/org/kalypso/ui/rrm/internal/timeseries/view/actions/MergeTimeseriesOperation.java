/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import java.util.Date;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ogc.sensor.timeseries.base.DatedDataSets;
import org.kalypso.ogc.sensor.util.DataSetTupleModelBuilder;
import org.kalypso.ogc.sensor.util.Observations;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IMergeTimeseriesOperation;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class MergeTimeseriesOperation implements IMergeTimeseriesOperation
{
  private final FeatureBean<ITimeseries> m_timeseries;

  private final boolean m_overwrite;

  private IObservation m_imported;

  public MergeTimeseriesOperation( final FeatureBean<ITimeseries> timeseries, final boolean overwrite )
  {
    m_timeseries = timeseries;
    m_overwrite = overwrite;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ITimeseries timeseries = m_timeseries.getFeature();
    final ZmlLink link = timeseries.getDataLink();

    try
    {
      final IObservation base = link.getObservationFromPool();
      final ITupleModel baseModel = base.getValues( null );

      final CacheTimeSeriesVisitor baseVisitor = new CacheTimeSeriesVisitor( base.getMetadataList() );
      baseModel.accept( baseVisitor, 0 );

      final DateRange baseRange = baseVisitor.getDateRange();

      final ITupleModel importModel = m_imported.getValues( null );
      final CacheTimeSeriesVisitor importVisitor = new CacheTimeSeriesVisitor( base.getMetadataList() );
      importModel.accept( importVisitor, 0 );

      final Map<Date, TupleModelDataSet[]> values = baseVisitor.getValueMap();
      doMerge( stati, values, importVisitor.getValues(), baseRange );

      doStoreObservation( stati, link, base, values );
    }
    catch( final SensorException e )
    {
      stati.add( IStatus.ERROR, Messages.getString( "MergeTimeseriesOperation_0" ), e ); //$NON-NLS-1$
    }

    return stati.asMultiStatus( Messages.getString( "MergeTimeseriesOperation_1" ) ); //$NON-NLS-1$
  }

  private void doStoreObservation( final IStatusCollector stati, final ZmlLink link, final IObservation base, final Map<Date, TupleModelDataSet[]> values )
  {

    final MetadataList metadata = base.getMetadataList();
    final DataSetTupleModelBuilder builder = new DataSetTupleModelBuilder( metadata, values );
    stati.add( builder.execute( new NullProgressMonitor() ) );

    try
    {
      final ITupleModel model = builder.getModel();
      final SimpleObservation observation = new SimpleObservation( base.getHref(), base.getName(), metadata, model );

      final DateRange dateRange = Observations.findDateRange( model );
      MetadataHelper.setTargetDateRange( metadata, dateRange );

      final IFile targetFile = link.getFile();
      ZmlFactory.writeToFile( observation, targetFile );

      m_timeseries.setProperty( ITimeseries.PROPERTY_MEASUREMENT_START, DateUtilities.toXMLGregorianCalendar( dateRange.getFrom() ) );
      m_timeseries.setProperty( ITimeseries.PROPERTY_MEASUREMENT_END, DateUtilities.toXMLGregorianCalendar( dateRange.getFrom() ) );

      final ICommand command = m_timeseries.applyChanges();

      final IScenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      stationsWorkspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      stati.add( IStatus.ERROR, Messages.getString( "MergeTimeseriesOperation_2" ), e ); //$NON-NLS-1$
    }
  }

  private void doMerge( final IStatusCollector stati, final Map<Date, TupleModelDataSet[]> values, final DatedDataSets[] imported, final DateRange daterange )
  {
    for( final DatedDataSets dataset : imported )
    {
      final Date date = dataset.getDate();
      if( !daterange.containsInclusive( date ) )
      {
        values.put( date, dataset.getDataSets() );
      }
      else if( m_overwrite )
      {
        values.put( date, dataset.getDataSets() );
      }
    }

    stati.add( IStatus.OK, Messages.getString( "MergeTimeseriesOperation_3" ) ); //$NON-NLS-1$
  }

  @Override
  public void setObservation( final IObservation observation )
  {
    m_imported = observation;
  }

}
