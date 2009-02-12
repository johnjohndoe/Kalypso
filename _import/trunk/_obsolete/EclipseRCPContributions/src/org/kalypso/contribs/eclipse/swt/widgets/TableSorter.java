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
package org.kalypso.contribs.eclipse.swt.widgets;

import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.contribs.eclipse.ImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.IViewerSorterFactory;

/**
 * Adds a common sorting mechanism to tables.
 * 
 * @author belger
 */
public class TableSorter
{
  public static final String SORT_KEY = "sortViewer";
  private Image m_emptyImage;
  private Image m_downImage;
  private Image m_upImage;
  private final IViewerSorterFactory m_sorterFactory;

  public TableSorter( final IViewerSorterFactory sorterFactory )
  {
    m_sorterFactory = sorterFactory;
    m_emptyImage = ImageProvider.ID_EMPTY.createImage();
    m_downImage = ImageProvider.ID_SORT_DOWN.createImage();
    m_upImage = ImageProvider.ID_SORT_UP.createImage();
  }
  
  public void dispose()
  {
    m_emptyImage.dispose();
    m_downImage.dispose();
    m_upImage.dispose();
  }
  
  /** For each Tablecolumn you want to sort, call this method wenn TableColumn is selected. */
  public void onColumenSelected( final StructuredViewer viewer, final TableColumn columnToSort )
  {
    final Boolean oldstate = (Boolean)columnToSort.getData( SORT_KEY );

    // clear columns
    final Table table = columnToSort.getParent();
    final TableColumn[] columns = table.getColumns();
    for( int i = 0; i < columns.length; i++ )
    {
      final TableColumn column = columns[i];
      column.setData( SORT_KEY, null );
      column.setImage( m_emptyImage );
    }
    
    final int sortIndex = table.indexOf( columnToSort );

    final ViewerSorter sorter;
    final Boolean newstate;
    final Image img;
    if( oldstate == null )
    {
      sorter = m_sorterFactory.createSorter( sortIndex, false );
      img = m_downImage;
      newstate = Boolean.FALSE;
    }
    else if( !oldstate.booleanValue() )
    {
      sorter = m_sorterFactory.createSorter( sortIndex, true );
      img = m_upImage;
      newstate = Boolean.TRUE;
    }
    else
    {
      sorter = null;
      img = m_emptyImage;
      newstate = null;
    }

    columnToSort.setData( SORT_KEY, newstate );
    columnToSort.setImage( img );

    viewer.setSorter( sorter );
  }
  
  public TableColumn createSortedColumn( final Table table, final String columnText, final StructuredViewer viewer )
  {
    final TableColumn column = new TableColumn( table, SWT.NONE );
    column.setText( columnText );
    column.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        onColumenSelected( viewer, column );
      }
    } );
    return column;
  }

}
