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
package org.kalypso.util.io;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Parses CSV-Values from a Reader. Save is possible using a Writer.
 * 
 * @author schlienger
 */
public class CSV extends AbstractCSV
{
  private final String m_split;

  /**
   * Constructor
   * 
   * @param split
   *          the string used for spliting each line into chunks
   * @throws IOException
   */
  public CSV( final String split, final int startLine,
      final boolean ignoreEmptyLines ) throws IOException
  {
    super( startLine, ignoreEmptyLines );

    m_split = split;
    
    // note: m_split is used as separator, maybe that's not good enough to
    // use the same token as the one for spliting
    setSeparator( m_split);
  }

  /**
   * @see org.kalypso.util.io.AbstractCSV#handleCurrentLine(java.lang.String)
   */
  protected void handleCurrentLine( final String line )
  {
    m_lines.add( line.split( m_split ) );
  }

  /**
   * Writes an two-dimensional Array of Strings as CSV into a writer. The Writer
   * will NOT be closed after this operation
   * 
   * @param data
   * @param pw
   */
  public static void writeCSV( final String[][] data, final PrintWriter pw )
  {
    for( int i = 0; i < data.length; i++ )
    {
      final String[] line = data[i];

      for( int j = 0; j < line.length; j++ )
      {
        if( j != 0 )
          pw.print( "," );

        pw.print( "\"" );
        pw.print( line[j] );
        pw.print( "\"" );
      }

      pw.println();
    }
  }
}