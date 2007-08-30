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

import java.math.BigDecimal;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.proxy.AutoProxyFactory;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.zml.filters.RoundFilterType;

/**
 * @author Holger Albert
 * @author Gernot Belger
 */
public class RoundFilter extends AbstractObservationFilter
{
  private final int m_mode;

  private final int m_factor;

  private final String m_type;

  private IObservation m_baseobservation = null;

  public RoundFilter( final RoundFilterType filter )
  {
    final String mode = filter.getMode();
    m_mode = toBigDecimalRoundingConstant( mode );

    m_factor = filter.getFactor();
    m_type = filter.getAxisType();
  }

  private static int toBigDecimalRoundingConstant( final String mode )
  {
    if( "ROUND_CEILING".equals( mode ) )
      return BigDecimal.ROUND_CEILING;
    else if( "ROUND_DOWN".equals( mode ) )
      return BigDecimal.ROUND_DOWN;
    else if( "ROUND_FLOOR".equals( mode ) )
      return BigDecimal.ROUND_FLOOR;
    else if( "ROUND_HALF_DOWN".equals( mode ) )
      return BigDecimal.ROUND_HALF_DOWN;
    else if( "ROUND_HALF_EVEN".equals( mode ) )
      return BigDecimal.ROUND_HALF_EVEN;
    else if( "ROUND_HALF_UP".equals( mode ) )
      return BigDecimal.ROUND_HALF_UP;
    else if( "ROUND_UNNECESSARY".equals( mode ) )
      return BigDecimal.ROUND_UNNECESSARY;
    else if( "ROUND_UP".equals( mode ) )
      return BigDecimal.ROUND_UP;

    throw new IllegalArgumentException( "Unsupported Rounding-Mode: " + mode );
  }

  public void initFilter( Object dummy, IObservation baseObs, URL context ) throws SensorException
  {
    m_baseobservation = baseObs;
    
    super.initFilter( dummy, baseObs, context );
  }

  public ITuppleModel getValues( final IRequest request ) throws SensorException
  {
    final Date from;
    final Date to;
    final IRequest bufferedRequest;
    if( request != null && request.getDateRange() != null )
    {
      from = request.getDateRange().getFrom();
      to = request.getDateRange().getTo();

      // BUGIFX: fixes the problem with the first value:
      // the first value was always ignored, because the intervall
      // filter cannot handle the first value of the source observation
      // FIX: we just make the request a big bigger in order to get a new first value
      final int bufferField = Calendar.DAY_OF_MONTH;
      final int bufferAmount = 2;

      final Calendar bufferedFrom = Calendar.getInstance();
      bufferedFrom.setTime( from );
      bufferedFrom.add( bufferField, -bufferAmount );

      final Calendar bufferedTo = Calendar.getInstance();
      bufferedTo.setTime( to );
      bufferedTo.add( bufferField, bufferAmount );

      bufferedRequest = new ObservationRequest( bufferedFrom.getTime(), bufferedTo.getTime() );
    }
    else
    {
      from = null;
      to = null;

      bufferedRequest = null;
    }

    
    final IObservation proxiedObservation = AutoProxyFactory.getInstance().proxyObservation(m_baseobservation);
    
    final ITuppleModel values = proxiedObservation.getValues( bufferedRequest );
//    final ITuppleModel values = m_baseobservation.getValues( bufferedRequest );

//    final SimpleTuppleModel simpleTuppleModel = new SimpleTuppleModel( values );
    
//    AutoProxyFactory.getInstance().proxyObservation(m_baseobservation)
    

    // get all non-virtual Double-Axises
    final IAxis axis = ObservationUtilities.findAxisByTypeNoEx( values.getAxisList(), m_type );
    if( axis == null )
      throw new SensorException( "No axes of type: " + m_type );
    
      final int valueCount = values.getCount();
      for( int j = 0; j < valueCount; j++ )
      {
        final Double value = (Double)values.getElement( j, axis );
        if( value != null && !value.isNaN() )
        {
          final double factoredValue = value.doubleValue() / m_factor;
          final BigDecimal decimal = new BigDecimal( factoredValue );
          final BigDecimal roundedDecimal = decimal.setScale( 0, m_mode );
          final double newValue = roundedDecimal.doubleValue() * m_factor;
          values.setElement( j, new Double( newValue ), axis );
        }
      }

    final ITuppleModel orgValues = m_baseobservation.getValues( bufferedRequest );

    final SimpleTuppleModel simpleTuppleModel = new SimpleTuppleModel( orgValues );
    return simpleTuppleModel;
  }

  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName() + " setValues() wird zur Zeit nicht unterstuetzt ." );
  }
}