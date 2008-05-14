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

import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.zml.filters.NOperationFilterType;

/**
 * @author doemming
 */
public class NOperationFilter extends AbstractObservationFilter
{
  public final static int OPERATION_UNKNOWN = 0;

  public final static int OPERATION_PLUS = 1;

  public final static int OPERATION_MINUS = 2;

  public final static int OPERATION_MAL = 3;

  public final static int OPERATION_DURCH = 4;

  private final int m_operation;

  private IObservation[] m_innerObservations = null;

  public NOperationFilter( NOperationFilterType filter )
  {
    final String operator = filter.getOperator();
    if( operator.equals( "+" ) ) //$NON-NLS-1$
      m_operation = OPERATION_PLUS;
    else if( operator.equals( "-" ) ) //$NON-NLS-1$
      m_operation = OPERATION_MINUS;
    else if( operator.equals( "*" ) ) //$NON-NLS-1$
      m_operation = OPERATION_MAL;
    else if( operator.equals( "/" ) ) //$NON-NLS-1$
      m_operation = OPERATION_DURCH;
    else
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationFilter.4") + operator + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationFilter.5") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void initFilter( Object conf, IObservation baseObs, URL context ) throws SensorException
  {
    super.initFilter( null, baseObs, context );
    m_innerObservations = (IObservation[])conf;
  }

  @Override
  public ITuppleModel getValues( IRequest args ) throws SensorException
  {
    ITuppleModel models[] = new ITuppleModel[m_innerObservations.length];
    for( int i = 0; i < models.length; i++ )
      models[i] = m_innerObservations[i].getValues( args );
    return new NOperationTupplemodel( models, m_operation );
  }

  @Override
  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationFilter.6") ); //$NON-NLS-1$
  }
}