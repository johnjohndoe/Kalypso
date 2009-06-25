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

package org.kalypso.contribs.java.swing.table;

import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * This class was taken from: http://javaalmanac.com/egs/javax.swing.table/ColHeadTips.html
 */
public class ColumnHeaderToolTips extends MouseMotionAdapter
{
  // Current column whose tooltip is being displayed.
  // This variable is used to minimize the calls to setToolTipText().
  private TableColumn curCol;

  // Maps TableColumn objects to tooltips
  private final Map tips = new HashMap();

  private final boolean m_showHeaderValueAsDefault;

  public ColumnHeaderToolTips( final boolean showHeaderValueAsDefault )
  {
    m_showHeaderValueAsDefault = showHeaderValueAsDefault;
  }

  // If tooltip is null, removes any tooltip text.
  public void setToolTip( final TableColumn col, final String tooltip )
  {
    if( tooltip == null )
      tips.remove( col );
    else
      tips.put( col, tooltip );
  }

  private String getToolTip( final TableColumn col )
  {
    if( col == null )
      return null;
    
    if( tips.containsKey( col ) )
      return (String)tips.get( col );

    if( m_showHeaderValueAsDefault )
      return (String)col.getHeaderValue();

    return null;
  }

  public void mouseMoved( MouseEvent evt )
  {
    TableColumn col = null;
    final JTableHeader header = (JTableHeader)evt.getSource();
    final JTable table = header.getTable();
    final TableColumnModel colModel = table.getColumnModel();
    int vColIndex = colModel.getColumnIndexAtX( evt.getX() );

    // Return if not clicked on any column header
    if( vColIndex >= 0 )
      col = colModel.getColumn( vColIndex );

    if( col != curCol )
    {
      header.setToolTipText( getToolTip( col ) );
      curCol = col;
    }
  }
}