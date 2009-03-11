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
package org.kalypso.observation.table;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author schlienger
 */
public class TupleResultColumn extends MTRMColumn
{
  private final TupleResult m_result;

  private final IComponent m_value;

  private final Map<MTRMRow, IRecord> m_map = new HashMap<MTRMRow, IRecord>();

  public TupleResultColumn( final int position, final TupleResult result, final IComponent key, final IComponent value )
  {
    super( position, value.getName(), key, XmlTypes.toJavaClass( value.getValueTypeName() ) );

    m_result = result;
    m_value = value;
  }

  public IComponent getValueComponent( )
  {
    return m_value;
  }

  public TupleResult getTupleResult( )
  {
    return m_result;
  }

  /**
   * Sets the map association between the rowKey and the record of the underlying TupleResult
   */
  public void setMapping( final MTRMRow rowKey, final IRecord record )
  {
    m_map.put( rowKey, record );
  }

  /**
   * @return the corresponding record for the given IRowKey
   */
  public IRecord getRecordFor( final MTRMRow rowKey )
  {
    return m_map.get( rowKey );
  }

  /**
   * Removes the rowKey from the map
   */
  public void removeKey( final MTRMRow rowKey )
  {
    m_map.remove( rowKey );
  }

  /**
   * Removes all rowKeys from the map
   */
  public void clear( )
  {
    m_map.clear();
  }
}
