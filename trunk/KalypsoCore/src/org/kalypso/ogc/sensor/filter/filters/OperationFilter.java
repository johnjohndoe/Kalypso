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

import java.net.URL;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.zml.filters.OperationFilterType;

/**
 * @author doemming
 */
public class OperationFilter extends AbstractObservationFilter
{
  public final static int OPERATION_UNKNOWN = 0;

  public final static int OPERATION_PLUS = 1;

  public final static int OPERATION_MINUS = 2;

  public final static int OPERATION_MAL = 3;

  public final static int OPERATION_DURCH = 4;

  private IObservation m_baseobservation = null;

  private final int m_operation;

  private final double m_operand;

  public OperationFilter( OperationFilterType filter )
  {
    m_operand = Double.parseDouble( filter.getOperand() );
    final String operator = filter.getOperator();
    if( operator.equals( "+" ) )
      m_operation = OPERATION_PLUS;
    else if( operator.equals( "-" ) )
      m_operation = OPERATION_MINUS;
    else if( operator.equals( "*" ) )
      m_operation = OPERATION_MAL;
    else if( operator.equals( "/" ) )
      m_operation = OPERATION_DURCH;
    else
      throw new IllegalArgumentException( "unknown operator '" + operator + "' in filter" );
  }

  public void initFilter( Object dummy, IObservation baseObs, URL context ) throws SensorException
  {
    m_baseobservation = baseObs;
    super.initFilter( dummy, baseObs, context );
  }

  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    return new OperationTupplemodel( m_operand, m_operation, m_baseobservation.getValues( args ) );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName()
        + " setValues() wird zur Zeit nicht unterstuetzt ." );
  }

}