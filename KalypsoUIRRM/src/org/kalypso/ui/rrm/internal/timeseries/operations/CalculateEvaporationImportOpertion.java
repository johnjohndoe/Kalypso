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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.joda.time.Period;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.util.FindTimeStepOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IImportTimeseriesOperation;
import org.kalypso.zml.ui.imports.IStoreObservationData;

/**
 * @author Dirk Kuch
 */
public class CalculateEvaporationImportOpertion implements IImportTimeseriesOperation
{

  private final IObservation m_observation;

  public CalculateEvaporationImportOpertion( final IObservation observation )
  {
    m_observation = observation;
  }

  @Override
  public IStoreObservationData getData( )
  {

    return new IStoreObservationData()
    {
      @Override
      public String getParameterType( )
      {
        final IObservation observation = getObservation();
        final IAxis valueAxis = AxisUtils.findValueAxis( observation.getAxes() );

        return valueAxis.getType();
      }

      @Override
      public String[] getExistingTimeserieses( )
      {
        // TODO Auto-generated method stub
        return new String[] {};
      }
    };
  }

  @Override
  public Period getTimestep( )
  {
    final FindTimeStepOperation timeStepOperation = new FindTimeStepOperation( getObservation() );
    timeStepOperation.execute( new NullProgressMonitor() );

    return timeStepOperation.getTimestep();
  }

  @Override
  public IObservation getObservation( )
  {
    return m_observation;
  }

}
