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
package org.kalypso.ogc.gml.om.table.handlers;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Table;
import org.kalypso.observation.result.IRecord;

/**
 * Implementors of this class are responsible for UI-representation of values of tuple-result components.
 * 
 * @author Dirk Kuch
 * @author Gernot Belger
 * @see org.kalypso.observation.result.TupleResult
 * @see org.kalypso.observation.result.IComponent
 */
public interface IComponentUiHandler
{
  CellEditor createCellEditor( final Table table );

  String getIdentity( );

  boolean isEditable( );

  boolean isResizeable( );

  boolean isMoveable( );

  /**
   * Format value for the cell editor
   * 
   * @return Must not return <code>null</code>.
   */
  Object doGetValue( IRecord record );

  /**
   * Sets a value to this column as obtained from the cell editor.
   */
  void doSetValue( final IRecord record, Object value );

  /**
   * Sets a value to the record's component represented by this handler.
   * 
   * @param value
   *            An object which's type must fit to the handled component
   */
  void setValue( final IRecord record, Object value );

  /**
   * String representation of the value. Used to show the value in a table.
   */
  String getStringRepresentation( IRecord value );

  Image getImage( IRecord record );

  /**
   * The width of the column in pixels.
   */
  int getColumnWidth( );

  /**
   * The width of the column in percent of the total table width. If -1, the columnWidth property is used instead.
   * <br/>No other validity checks are performed, so column wider than the table may be configured.<br/> If >0, the
   * columnWidth property is interpreted as the minimum width.
   */
  int getColumnWidthPercent( );

  int getColumnStyle( );

  String getColumnLabel( );

  /** Converts a (user entered) text into an object handled by this handler. */
  public Object parseValue( final String text );
}
