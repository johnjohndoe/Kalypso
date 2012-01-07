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
package org.kalypso.model.rcm.util;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * Utilities for {@link org.kalypso.model.rcm.binding.IOmbrometer}'s.
 *
 * @author Gernot Belger
 */
public class OmbrometerUtils
{
  private OmbrometerUtils( )
  {
    // helper class, do not instantiate
  }

  public static String analyseOmbrometer( final IObservation observation ) throws SensorException
  {
    final IAxis axis = ObservationUtilities.findAxisByType( observation.getAxes(), ITimeseriesConstants.TYPE_RAINFALL );
    final ITupleModel values = observation.getValues( null );

    final int goods = countStatus( values, axis );
    final int count = values.size();
    return String.format( "%3d / %3d", goods, count ); //$NON-NLS-1$
  }

  /**
   * Z‰hlt die Anzahl der nicht gewarnten oder editierten Werte.
   *
   * @throws SensorException
   */
  private static int countStatus( final ITupleModel values, final IAxis axis ) throws SensorException
  {
    final IAxis statusAxis = KalypsoStatusUtils.findStatusAxisFor( values.getAxes(), axis );
    int count = 0;
    for( int i = 0; i < values.size(); i++ )
    {
      final int status = ((Number) values.get( i, statusAxis )).intValue();
      if( !KalypsoStatusUtils.checkMask( status, KalypsoStati.BIT_CHECK ) || KalypsoStatusUtils.checkMask( status, KalypsoStati.BIT_USER_MODIFIED ) )
        count++;
    }

    return count;
  }

  public static Boolean checkIfOmbrometershouldBeUsed( final IOmbrometer ombro )
  {
    final String description = ombro.getDescription();
    return getUsedFromDescription( description );
  }

  public static Boolean getUsedFromDescription( final String description )
  {
    final double ratio = getRatioFromOmbrometerDescription( description );
    if( ratio > 0.8 ) // TODO: configure
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }

  public static double getRatioFromOmbrometerDescription( final String description )
  {
    final int index = description.indexOf( '/' );
    double ratio = 0;
    if( index != -1 )
    {
      final Integer goods = NumberUtils.parseQuietInteger( description.substring( 0, index ).trim() );
      final Integer count = NumberUtils.parseQuietInteger( description.substring( index + 1 ).trim() );
      if( goods != null && count != null )
        ratio = goods.doubleValue() / count.doubleValue();
    }
    return ratio;
  }
}
