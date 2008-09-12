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
package org.kalypso.commons.tuple.event;

import java.util.Set;

import org.kalypso.commons.tuple.IColumnKey;
import org.kalypso.commons.tuple.IRowKey;
import org.kalypso.commons.tuple.ITupleModel;

/**
 * @author schlienger
 */
public class TupleModelEventDelegate<R extends IRowKey, C extends IColumnKey> extends TupleModelEventAdapter<R, C> implements ITupleModel<R, C>, ITupleModelEventProvider<R, C>
{
  private final ITupleModel<R, C> m_model;

  public TupleModelEventDelegate( final ITupleModel<R, C> model )
  {
    m_model = model;
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getRowCount()
   */
  public int getRowCount( )
  {
    return m_model.getRowCount();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    return m_model.getColumnCount();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getRowKeySet()
   */
  public Set<R> getRowKeySet( )
  {
    return m_model.getRowKeySet();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getColumnKeySet()
   */
  public Set<C> getColumnKeySet( )
  {
    return m_model.getColumnKeySet();
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasRowKey(R)
   */
  public boolean hasRowKey( R rowKey )
  {
    return m_model.hasRowKey( rowKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#hasColumnKey(C)
   */
  public boolean hasColumnKey( C columnKey )
  {
    return m_model.hasColumnKey( columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#getValue(R, C)
   */
  public Object getValue( R rowKey, C columnKey )
  {
    return m_model.getValue( rowKey, columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#setValue(java.lang.Object, R, C)
   */
  public void setValue( Object value, R rowKey, C columnKey )
  {
    m_model.setValue( value, rowKey, columnKey );

    fireValueChanged( value, rowKey, columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#isNotNull(R, C)
   */
  public boolean isNotNull( R rowKey, C columnKey )
  {
    return m_model.isNotNull( rowKey, columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#clearValue(R, C)
   */
  public void clearValue( R rowKey, C columnKey )
  {
    m_model.clearValue( rowKey, columnKey );

    fireValueChanged( m_model.getValue( rowKey, columnKey ), rowKey, columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#removeColumn(C)
   */
  public void removeColumn( C columnKey )
  {
    m_model.removeColumn( columnKey );

    fireColumnRemoved( columnKey );
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#removeRow(R)
   */
  public void removeRow( R rowKey )
  {
    m_model.removeRow( rowKey );

    fireRowRemoved( rowKey );
  }
}
