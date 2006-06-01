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
package org.kalypso.commons.tuple.impl;

import java.util.Set;

import org.kalypso.commons.tuple.IColumnKey;
import org.kalypso.commons.tuple.IRowKey;
import org.kalypso.commons.tuple.ITupleModel;

/**
 * @author schlienger
 */
public abstract class AbstractTupleModel<R extends IRowKey, C extends IColumnKey> implements ITupleModel<R, C>
{
  /**
   * This basic implementation checks if the class of the value is compatible with the class defined in the columnKey.
   * It then delegates the call to <code>setValueIntern</code>.
   * 
   * @see org.kalypso.commons.tuple.ITupleModel#setValue(java.lang.Object, R, C)
   */
  public void setValue( final Object value, final R rowKey, final C columnKey )
  {
    if( value != null && !((IColumnKey) columnKey).getValueClass().isAssignableFrom( value.getClass() ) )
      throw new IllegalArgumentException( "Incompatible classes" );

    setValueIntern( value, rowKey, columnKey );
  }

  /**
   * Subclasses must implement this method
   */
  protected abstract void setValueIntern( final Object value, final R rowKey, final C columnKey );

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#isNotNull(R, C)
   */
  public boolean isNotNull( final R rowKey, final C columnKey )
  {
    return getValue( rowKey, columnKey ) != null;
  }

  /**
   * @see org.kalypso.commons.tuple.ITupleModel#clearValue(R, C)
   */
  public void clearValue( final R rowKey, final C columnKey )
  {
    setValue( null, rowKey, columnKey );
  }

  /**
   * This basic implementation checks if this model contains the given column before delegating the call to
   * <code>removeColumnIntern</code>. It does nothing if the column is not present.
   */
  public void removeColumn( final C columnKey )
  {
    final Set<C> columnKeySet = getColumnKeySet();
    if( !columnKeySet.contains( columnKey ) )
      return;

    removeColumnIntern( columnKey );
  }

  /**
   * Subclasses must implement this method
   */
  protected abstract void removeColumnIntern( final C columnKey );

  /**
   * This basic implementation checks if this model contains the given row, it then delegates the call to
   * <code>removeRowIntern</code>. It does nothing if the row is not present.
   * 
   * @see org.kalypso.commons.tuple.ITupleModel#removeRow(R)
   */
  public void removeRow( final R rowKey )
  {
    final Set<R> rowKeySet = getRowKeySet();
    if( !rowKeySet.contains( rowKey ) )
      return;

    removeRowIntern( rowKey );
  }

  /**
   * Subclasses must implement this method
   */
  protected abstract void removeRowIntern( final R rowKey );
}
