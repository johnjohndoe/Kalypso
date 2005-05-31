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
package org.bce.eclipse.jface.viewers;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/**
 * DefaultTableViewer handles common functionality that you wish you had when
 * working with a TableViewer.
 * 
 * @author schlienger
 */
public class DefaultTableViewer extends TableViewer
{
  public static final String COLUMN_PROP_NAME = "columnName";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  public static final String COLUMN_PROP_WIDTH = "columnWidth";

  public DefaultTableViewer( final Composite parent )
  {
    super( parent );
  }

  public DefaultTableViewer( final Composite parent, final int style )
  {
    super( parent, style );
  }

  public DefaultTableViewer( final Table table )
  {
    super( table );
  }

  /**
   * Adds a column to the underlying table control.
   */
  public TableColumn addColumn( final String name, final String title,
      final int width, final boolean isEditable )
  {
    final Table table = getTable();

    final TableColumn tc = new TableColumn( table, SWT.CENTER );
    tc.setData( COLUMN_PROP_NAME, name );
    tc.setData( COLUMN_PROP_EDITABLE, Boolean.valueOf( isEditable ) );
    tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
    tc.setWidth( width );

    tc.setText( title );

    return tc;
  }

  /**
   * Refreshes the column properties according to the current list of columns
   * 
   * @see org.eclipse.jface.viewers.TableViewer#getColumnProperties()
   */
  public void refreshColumnProperties()
  {
    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    final String[] properties = new String[columns.length];

    for( int i = 0; i < properties.length; i++ )
      properties[i] = columns[i].getData( COLUMN_PROP_NAME ).toString();

    setColumnProperties( properties );
  }
}
