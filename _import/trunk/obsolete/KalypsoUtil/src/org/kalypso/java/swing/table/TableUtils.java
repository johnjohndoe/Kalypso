/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.java.swing.table;

import java.io.BufferedWriter;
import java.io.IOException;

import javax.swing.JTable;

/**
 * A set of utility methods dealing with JTable and co.
 * 
 * @author schlienger (03.06.2005)
 */
public class TableUtils
{
  private TableUtils()
  {
    // empty
  }

  public static void dump( final JTable table, final String columnSeparator,
      final BufferedWriter writer ) throws IOException
  {
    final int rowCount = table.getRowCount();
    final int colCount = table.getColumnCount();

    for( int row = 0; row < rowCount; row++ )
    {
      for( int col = 0; col < colCount; col++ )
      {
        final Object value = table.getValueAt( row, col );

        // TODO formatter?
        writer.write( value.toString() );

        if( col != colCount - 1 )
          writer.write( columnSeparator );
      }

      writer.newLine();
    }
  }
}
