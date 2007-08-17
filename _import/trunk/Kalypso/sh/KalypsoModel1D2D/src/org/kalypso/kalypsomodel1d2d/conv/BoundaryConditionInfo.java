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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Date;

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class BoundaryConditionInfo implements ITimeStepinfo
{
  private final int m_id;

  private TYPE m_type;

  private IComponent m_valueComponent;

  private TupleResultIndex m_index;

  private double m_steadyValue;

  private double m_theta;

  public BoundaryConditionInfo( final int id, final TYPE type )
  {
    m_id = id;
    m_type = type;
    m_steadyValue = Double.NaN;
  }

  public int getID( )
  {
    return m_id;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo#getType()
   */
  public TYPE getType( )
  {
    return m_type;
  }

  public void setType( final TYPE type )
  {
    m_type = type;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo#getValue(java.util.Date)
   */
  public double getValue( final Date date ) throws SimulationException
  {
    final Number result;
    if( date == null )
    {
      if( Double.isNaN( m_steadyValue ) )
      {
        throw new SimulationException( Messages.getString("BoundaryConditionInfo.0"), new RuntimeException() ); //$NON-NLS-1$
      }
      else
        return m_steadyValue;
    }
    else
      result = (Number) m_index.getValue( m_valueComponent, date );

    return (result == null || Double.isNaN( result.doubleValue() )) ? 0.0 : result.doubleValue();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo#getTheta()
   */
  public double getTheta( )
  {
    return m_theta;
  }

  public void setObservation( final IObservation<TupleResult> obs, final IComponent domainComponent, final IComponent valueComponent )
  {
    m_valueComponent = valueComponent;
    m_index = new TupleResultIndex( obs.getResult(), domainComponent );
  }

  public void setSteadyValue( final double value )
  {
    m_steadyValue = value;
  }

  public void setTheta( final double thetaGrad )
  {
    m_theta = Math.toRadians( thetaGrad );
  }
}
