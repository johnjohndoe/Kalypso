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
package org.kalypso.model.wspm.tuhh.core.results;

import java.math.BigDecimal;
import java.util.Formattable;
import java.util.Formatter;

import javax.xml.namespace.QName;

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;

/**
 * @author Gernot Belger
 */
public class WspmResultLengthSectionColumn implements Formattable
{
  private final IObservation<TupleResult> m_observation;

  private final TupleResultIndex m_stationHash;

  private final int m_component;

  private final String m_label;

  private final QName m_valueTypeName;

  private final String m_componentLabel;

  private final String m_observationName;

  private final String m_componentID;

  public WspmResultLengthSectionColumn( final IObservation<TupleResult> observation, final TupleResultIndex stationHash, final IComponent component )
  {
    m_observation = observation;
    m_stationHash = stationHash;
    final TupleResult result = m_observation.getResult();
    m_component = result.indexOfComponent( component );

    m_valueTypeName = component.getValueTypeName();

    m_componentLabel = component.getName();
    m_componentID = component.getId();

    m_observationName = m_observation.getName();
    m_label = String.format( "%s - %s", m_observationName, m_componentLabel ); //$NON-NLS-1$
  }

  public String getResultName( )
  {
    return m_observationName;
  }

  public String getComponentLabel( )
  {
    return m_componentLabel;
  }

  public String getComponentID( )
  {
    return m_componentID;
  }

  public QName getValueTypeName( )
  {
    return m_valueTypeName;
  }

  /** The result value for the given station */
  public Object getValue( final BigDecimal station )
  {
    final IRecord record = m_stationHash.getRecord( station );
    if( record == null || m_component == -1 )
      return null;

    return record.getValue( m_component );
  }

  public String getLabel( )
  {
    return m_label;
  }

  /**
   * @see java.util.Formattable#formatTo(java.util.Formatter, int, int, int)
   */
  @Override
  public void formatTo( final Formatter formatter, final int flags, final int width, final int precision )
  {
    if( m_observationName.length() < width )
    {
      formatter.format( m_observationName );
    }
    else
    {
      formatter.format( m_observationName.substring( 0, width ) );
    }

    if( m_componentLabel.length() < precision )
    {
      formatter.format( m_componentLabel );
    }
    else
    {
      formatter.format( m_componentLabel.substring( 0, precision ) );
    }
  }
}
