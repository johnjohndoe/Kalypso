/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.rrm.internal.timeseries.view.evaporation;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.operation.evaporation.WaterbasedEvaporationCalculator;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.CalculateEvaporationData;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.StoreTimeseriesOperation;

/**
 * @author Dirk Kuch
 */
public class CalculateEvaporationWizard extends Wizard
{
  private final CommandableWorkspace m_workspace;

  private final IStation m_station;

  private final CalculateEvaporationData m_data;

  public CalculateEvaporationWizard( final CommandableWorkspace workspace, final IStation station, final CalculateEvaporationData data )
  {
    m_workspace = workspace;
    m_station = station;
    m_data = data;

    addPage( new ChooseEvaporationInputFilesPage( station, data ) );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final IObservation humidity = m_data.toObservation( m_data.getHumidity() );
      final IObservation sunshine = m_data.toObservation( m_data.getSunshineHours() );
      final IObservation temperature = m_data.toObservation( m_data.getTemperature() );
      final IObservation windVelocity = m_data.toObservation( m_data.getWindVelocity() );
      if( Objects.isNull( humidity, sunshine, temperature, windVelocity ) )
        return false;

      final DateRange daterange = m_data.getDateRange();

      final WaterbasedEvaporationCalculator calculator = new WaterbasedEvaporationCalculator( CacheTimeSeriesVisitor.cache( humidity ), CacheTimeSeriesVisitor.cache( sunshine ), CacheTimeSeriesVisitor.cache( temperature ), CacheTimeSeriesVisitor.cache( windVelocity ), daterange );
      final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, calculator );
      if( !status.isOK() )
      {
        StatusDialog.open( getShell(), status, getWindowTitle() );
      }

      final IObservation observation = calculator.getObservation( ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED );

      final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( new TimeseriesBean(), m_workspace, m_station, new CalculateEvaporationImportOpertion( observation ) );
      storeOperation.updateDataAfterFinish();

      final IStatus status2 = RunnableContextHelper.execute( getContainer(), true, false, storeOperation );
      if( !status2.isOK() )
      {
        StatusDialog.open( getShell(), status2, getWindowTitle() );
      }

      // FIXME better error handling
      return status.isOK() && status2.isOK();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return false;
    }
  }
}