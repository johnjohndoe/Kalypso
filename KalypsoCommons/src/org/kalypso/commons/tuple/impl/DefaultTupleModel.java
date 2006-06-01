/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.commons.tuple.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.kalypso.commons.tuple.IColumnKey;
import org.kalypso.commons.tuple.IRowKey;
import org.kalypso.commons.tuple.ITupleModel;

/**
 * Standard implementation of ITupleModel
 * 
 * @author schlienger
 */
public class DefaultTupleModel<R extends IRowKey, C extends IColumnKey> extends AbstractTupleModel<R, C> implements ITupleModel<R, C>
{
  private final Set<C> m_columns = new HashSet<C>();

  private final Map<R, Map<C, Object>> m_model = new HashMap<R, Map<C, Object>>();

  /**
   * @see org.kalypso.tuplemodel.ITupleModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    return m_columns.size();
  }

  /**
   * @see org.kalypso.tuplemodel.ITupleModel#getRowCount()
   */
  public int getRowCount( )
  {
    return m_model.size();
  }

  /**
   * @see org.kalypso.tuple.ITupleModel#getColumnKeySet()
   */
  public Set<C> getColumnKeySet( )
  {
    return m_columns;
  }

  /**
   * @see org.kalypso.tuple.ITupleModel#getRowKeySet()
   */
  public Set<R> getRowKeySet( )
  {
    return m_model.keySet();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasRowKey(R)
   */
  public boolean hasRowKey( R rowKey )
  {
    return m_model.containsKey( rowKey );
  }
  
  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasColumnKey(null)
   */
  public boolean hasColumnKey( C columnKey )
  {
    return m_columns.contains( columnKey );
  }
  
  @SuppressWarnings("unchecked")
  public Object getValue( final R rowKey, final C columnKey )
  {
    final Map<C, Object> row = m_model.get( rowKey );
    if( row == null )
      return null;
    
    return row.get( columnKey );
  }

  @Override
  protected void removeColumnIntern( final C columnKey )
  {
    final Set<R> rowKeySet = getRowKeySet();
    for( final R rowKey : rowKeySet )
      setValue( null, rowKey, columnKey );
    
    m_columns.remove( columnKey );
  }

  @Override
  protected void removeRowIntern( final R rowKey )
  {
    m_model.get( rowKey ).clear();
    m_model.remove( rowKey );
  }

  @Override
  protected void setValueIntern( final Object value, final R rowKey, final C columnKey )
  {
    if( !m_columns.contains( columnKey ) )
      m_columns.add( columnKey );
    if( !m_model.containsKey( rowKey ) )
      m_model.put( rowKey, new HashMap<C, Object>() );

    m_model.get( rowKey ).put( columnKey, value );
  }
}
