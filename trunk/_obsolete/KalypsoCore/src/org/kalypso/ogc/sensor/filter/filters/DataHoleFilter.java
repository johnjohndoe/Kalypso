/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.filters;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * DataHoleFilter sets the status of elements that equals the marker-value. TODO: This filter only works if there
 * already is a status axis. It does not add one on its own.
 * 
 * @author schlienger
 */
public class DataHoleFilter extends AbstractObservationFilter
{
  private final Double m_value;

  private final Integer m_status;

  private final Double m_replace;

  private final Map<IAxis, IAxis> m_map = new HashMap<IAxis, IAxis>();

  public DataHoleFilter( final double value, final int status, final Double replace )
  {
    m_value = new Double( value );
    m_status = new Integer( status );
    m_replace = replace;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  @Override
  public ITuppleModel getValues( final IRequest args ) throws SensorException
  {
    final ITuppleModel values = super.getValues( args );
    final IAxis[] valueAxes = KalypsoStatusUtils.findAxesByClass( values.getAxisList(), Number.class, true );

    for( int index = 0; index < values.getCount(); index++ )
    {
      for( int ia = 0; ia < valueAxes.length; ia++ )
      {
        if( m_value.equals( values.getElement( index, valueAxes[ia] ) ) )
        {
          final IAxis sa = getStatusAxisFor( valueAxes[ia], values.getAxisList() );
          if( sa != null )
          {
            // update the status
            values.setElement( index, m_status, sa );

            // and replace value
            if( m_replace != null )
              values.setElement( index, m_replace, valueAxes[ia] );
          }
        }
      }
    }

    return values;
  }

  /**
   * Helper
   * 
   * @return status axis or null if none
   */
  private IAxis getStatusAxisFor( final IAxis axis, final IAxis[] axes )
  {
    if( m_map.containsKey( axis ) )
      return m_map.get( axis );

    final String label = KalypsoStatusUtils.getStatusAxisLabelFor( axis );
    final IAxis statusAxis = ObservationUtilities.findAxisByNameNoEx( axes, label );

    m_map.put( axis, statusAxis );

    return statusAxis;
  }
}