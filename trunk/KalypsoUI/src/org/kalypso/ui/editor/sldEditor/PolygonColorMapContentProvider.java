/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.editor.sldEditor;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;

/**
 * @author Thomas Jung
 */
public class PolygonColorMapContentProvider implements IStructuredContentProvider
{
  enum PROPS
  {
    label,
    from,
    to,
    stroke,
    fill
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    final PolygonColorMap colorMap = (PolygonColorMap) inputElement;
    return colorMap.getColorMap();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    if( viewer instanceof TableViewer )
    {
      final TableViewer tableViewer = (TableViewer) viewer;

      final Table table = (tableViewer).getTable();
      final TableColumn[] columns = table.getColumns();
      for( TableColumn tableColumn : columns )
        tableColumn.dispose();

      final TableColumn lableColumn = new TableColumn( table, SWT.NONE );
      lableColumn.setText( "Bezeichnung" );
      lableColumn.setWidth( 100 );

      final TableColumn fromColumn = new TableColumn( table, SWT.NONE );
      fromColumn.setText( "Von" );
      fromColumn.setWidth( 50 );

      final TableColumn toColumn = new TableColumn( table, SWT.NONE );
      toColumn.setText( "Bis" );
      toColumn.setWidth( 50 );

      final TableColumn strokeColumn = new TableColumn( table, SWT.NONE );
      strokeColumn.setText( "Linie" );
      strokeColumn.setWidth( 50 );

      final TableColumn fillColumn = new TableColumn( table, SWT.NONE );
      fillColumn.setText( "Füllung" );
      fillColumn.setWidth( 50 );

      final PROPS[] propValues = PROPS.values();
      final String[] props = new String[propValues.length];
      for( int i = 0; i < props.length; i++ )
        props[i] = propValues[i].name();

      tableViewer.setColumnProperties( props );
    }
  }
}
