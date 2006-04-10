/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.tuple.impl;

import java.util.Set;

import org.kalypso.tuple.IColumnKey;
import org.kalypso.tuple.IKey;
import org.kalypso.tuple.ITupleModel;

/**
 * @author schlienger
 */
public abstract class AbstractTupleModel<R extends IKey, C extends IColumnKey> implements ITupleModel<R, C>
{
  public void setValue( final Object value, final R rowKey, final C columnKey )
  {
    if( value != null && !((IColumnKey)columnKey).getValueClass().isAssignableFrom( value.getClass() ) )
      throw new IllegalArgumentException( "Incompatible classes" );

    setValueIntern( value, rowKey, columnKey );
  }

  protected abstract void setValueIntern( final Object value, final R rowKey, final C columnKey );

  public boolean hasValue( final R rowKey, final C columnKey )
  {
    return getValue( rowKey, columnKey ) != null;
  }

  public void removeValue( final R rowKey, final C columnKey )
  {
    setValue( null, rowKey, columnKey );
  }

  public void deleteColumn( final C columnKey )
  {
    final Set<C> columnKeySet = getColumnKeySet();
    if( !columnKeySet.contains( columnKey ) )
      return;

    final Set<R> rowKeySet = getRowKeySet();
    for( final R rowKey : rowKeySet )
      setValue( null, rowKey, columnKey );

    deleteColumnIntern( columnKey );
  }

  /**
   * Subclass must implement this method. They might perform additional operations when deleting a column.
   */
  protected abstract void deleteColumnIntern( final C columnKey );

  /**
   * @see org.kalypso.tuple.ITupleModel#deleteRow(R)
   */
  public void deleteRow( final R rowKey )
  {
    final Set<R> rowKeySet = getRowKeySet();
    if( !rowKeySet.contains( rowKey ) )
      return;

    deleteRowIntern( rowKey );
  }

  protected abstract void deleteRowIntern( final R rowKey );
}
