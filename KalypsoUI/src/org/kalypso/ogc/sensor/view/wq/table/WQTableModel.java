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

package org.kalypso.ogc.sensor.view.wq.table;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

/**
 * @author schlienger
 */
public class WQTableModel extends AbstractTableModel implements TableModel
{
//  private final static String[] COLUMN_NAMES =
//  {
//      "W",
//      "Q0",
//      "Q1",
//      "Q2",
//      "Q3",
//      "Q4",
//      "Q5",
//      "Q6",
//      "Q7",
//      "Q8",
//      "Q9" };

  private final Double m_startW;
  private final Double[] m_Q;
  private final int m_indexOffset;
  private final String m_fromType;
  private final String m_toType;

  public WQTableModel( final String fromType, final String toType, final Double startW, final Double[] Q )
  {
    m_fromType = fromType;
    m_toType = toType;
    m_startW = startW;
    m_Q = Q;

    final int w;
    if( m_startW.intValue() < 0 )
      w = -1 * (m_startW.intValue() / 10 + 1 ) * 10;
    else
      w = m_startW.intValue() / 10 * 10;
    
    m_indexOffset = w - m_startW.intValue();
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount()
  {
    return 11;
  }

  /**
   * @see javax.swing.table.TableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    if( columnIndex == 0 )
      return Integer.class;

    return Number.class;
  }

  /**
   * @see javax.swing.table.TableModel#getColumnName(int)
   */
  public String getColumnName( int columnIndex )
  {
    if( columnIndex == 0 )
      return m_fromType;
    
    return m_toType + columnIndex;
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount()
  {
    return m_Q.length / 10 + ( m_Q.length % 10 == 0 ? 0 : 1 ) + (m_startW.intValue() < 0 ? 1 : 0);
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    if( columnIndex == 0 )
    {
      final int w = m_startW.intValue() + rowIndex * 10;
      if( w < 0 )
        return new Double( -1 * (w / 10 + 1 ) * 10 );

      return new Double( w / 10 * 10 );
    }

    int ix = rowIndex * 10 + columnIndex - 1;
    if( ix < m_indexOffset )
      return null;
    
    ix += m_indexOffset;
    if( ix < 0 || ix >= m_Q.length )
      return null;

    return m_Q[ix];
  }
}
