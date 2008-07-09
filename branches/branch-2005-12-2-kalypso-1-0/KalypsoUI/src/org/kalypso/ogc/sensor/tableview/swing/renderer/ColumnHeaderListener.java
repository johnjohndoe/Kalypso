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

package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;

import org.kalypso.ogc.sensor.tableview.TableViewColumn;

/**
 * Captures mouse clicks on a table header, with the intention of triggering a click. Adapted from code by jcommons
 * posted on http://www.jfree.org/jcommon/.
 * 
 * @author Gernot Belger
 */
public class ColumnHeaderListener extends MouseAdapter
{
  /** The index of the column that is sorted - used to determine the state of the renderer. */
  private TableColumn m_pressedColumn;

  /**
   * Handle a mouse press event - if the user is NOT resizing a column and NOT dragging a column then give visual
   * feedback that the column header has been pressed.
   * 
   * @param e
   *          the mouse event.
   */
  public void mousePressed( final MouseEvent e )
  {
    if( e.getButton() != MouseEvent.BUTTON1 )
      return;

    final JTableHeader header = (JTableHeader)e.getComponent();

    if( header.getResizingColumn() == null )
    { // resizing takes precedence over sorting
      if( header.getDraggedDistance() < 1 )
      { // dragging also takes precedence over sorting
        m_pressedColumn = findColumn( e );
        if( m_pressedColumn == null )
          return;

        final ColumnHeaderRenderer renderer = (ColumnHeaderRenderer)m_pressedColumn.getHeaderRenderer();

        renderer.setPressedColumn( m_pressedColumn.getModelIndex() );
        header.repaint();

        if( header.getTable().isEditing() )
          header.getTable().getCellEditor().stopCellEditing();
      }
    }
  }

  /**
   * This event is ignored (not required).
   * 
   * @param e
   *          the mouse event.
   */
  public void mouseClicked( final MouseEvent e )
  {
    if( e.getButton() != MouseEvent.BUTTON1 )
      return;
    
    // not required
    final TableColumn column = findColumn( e );
    if( column == null )
      return;

    final ColumnHeaderRenderer renderer = (ColumnHeaderRenderer)column.getHeaderRenderer();
    final TableViewColumn tvc = (TableViewColumn)column.getHeaderValue();
    renderer.openDialog( tvc, e );
  }

  /**
   * When the user releases the mouse button, we attempt to sort the table.
   * 
   * @param e
   *          the mouse event.
   */
  public void mouseReleased( final MouseEvent e )
  {
    final JTableHeader header = (JTableHeader)e.getComponent();

    if( header.getResizingColumn() == null )
    { // resizing takes precedence over sorting
      if( m_pressedColumn != null )
      {
        final ColumnHeaderRenderer renderer = (ColumnHeaderRenderer)m_pressedColumn.getHeaderRenderer();

        renderer.setPressedColumn( -1 ); // clear
        header.repaint();
      }
    }
  }

  /**
   * @see java.awt.event.MouseAdapter#mouseEntered(java.awt.event.MouseEvent)
   */
  public void mouseEntered( final MouseEvent evt )
  {
    final TableColumn column = findColumn( evt );
    final JTableHeader header = (JTableHeader)evt.getSource();
    header.setToolTipText( getToolTip( column ) );
  }

  private String getToolTip( final TableColumn column )
  {
    if( column == null )
      return null;

    final ColumnHeaderRenderer renderer = (ColumnHeaderRenderer)column.getHeaderRenderer();
    final TableViewColumn tvc = (TableViewColumn)column.getHeaderValue();
    return renderer.getTooltip( tvc );
  }

  private TableColumn findColumn( final MouseEvent e )
  {
    final JTableHeader header = (JTableHeader)e.getComponent();

    final int columnIndex = header.columnAtPoint( e.getPoint() );
    if( columnIndex == -1 )
      return null;

    return header.getColumnModel().getColumn( columnIndex );
  }

}
