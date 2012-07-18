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
package org.kalypso.model.hydrology.timeseries;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;

/**
 * @author Dirk Kuch
 */
public class SetMissingValuesTo0Visitor extends AbstractMissingValuesVisitor implements IObservationVisitor
{
  public SetMissingValuesTo0Visitor( final double minValue, final double maxValue )
  {
    super( minValue, maxValue );
  }

  @Override
  public void visit( final IObservationValueContainer container ) throws SensorException
  {
    if( isMissingValue( container ) )
    {
      final IAxis valueAxis = findValueAxis( container.getAxes() );
      final IAxis statusAxis = AxisUtils.findStatusAxis( container.getAxes(), valueAxis );
      final IAxis dataSourceAxis = AxisUtils.findDataSourceAxis( container.getAxes(), valueAxis );

      container.set( valueAxis, 0.0 );
      container.set( statusAxis, KalypsoStati.BIT_CHECK );
      container.set( dataSourceAxis, getDataSourceIndex( container, dataSourceAxis ) );
    }
  }
}