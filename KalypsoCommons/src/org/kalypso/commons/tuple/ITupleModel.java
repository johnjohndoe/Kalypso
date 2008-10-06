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

package org.kalypso.commons.tuple;

import java.util.Set;

import org.kalypso.commons.tuple.event.ITupleModelEventProvider;

/**
 * General TupleModel interface.
 * 
 * @author schlienger
 */
public interface ITupleModel<R extends IRowKey, C extends IColumnKey> extends ITupleModelEventProvider<R, C>
{
  public int getRowCount( );

  public int getColumnCount( );

  public Set<R> getRowKeySet( );

  public Set<C> getColumnKeySet( );

  public boolean hasRowKey( R rowKey );

  public boolean hasColumnKey( C columnKey );

  /**
   * @return the value at the given position, can be null
   */
  public Object getValue( R rowKey, C columnKey );

  /**
   * If there's no corresponding row or column in the model, it is automatically added to it.
   */
  public void setValue( Object value, R rowKey, C columnKey );

  /**
   * Same as <code>getValue( rowKey, columnKey ) != null</code>.
   * 
   * @return true if the value at given position is not null
   */
  public boolean isNotNull( R rowKey, C columnKey );

  /**
   * Sets the value to null at the given position. Same as <code>setValue( null, rowKey, columnKey )</code>
   */
  public void clearValue( R rowKey, C columnKey );

  /**
   * Removes the column from this model. Sub-classes may decide what to do when removing a column:
   * <ul>
   * <li>the underlying data might be set to null or some default value</li>
   * <li>the underlying data might be removed as well</li>
   * </ul>
   */
  public void removeColumn( C columnKey );

  /**
   * Removes the row from this model. Sub-classes may decide what to do when removing a row:
   * <ul>
   * <li>the underlying data might be set to null or some default value</li>
   * <li>the underlying data might be removed as well</li>
   * </ul>
   */
  public void removeRow( R rowKey );
}
