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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Iterator;
import java.util.Vector;

/**
 * Parses a CSV file. Save is possible using a Writer.
 * 
 * @author schlienger
 */
public class CSV implements ITabledValues
{
  private final Reader m_reader;

  private final String m_split;

  private final Vector m_lines = new Vector();

  private final int m_line;

  private final boolean m_ignoreEmptyLines;

  /**
   * Constructor. Fetches the file and closes the reader;
   * 
   * @param reader
   * @param split
   *          the string used for spliting each line into chunks
   * @param line
   *          the line number to start reading the values at
   * @param ignoreEmptyLines
   *          when true, empty lines are not stored in this object
   * 
   * @throws IOException
   */
  public CSV( final Reader reader, final String split, final int line,
      final boolean ignoreEmptyLines ) throws IOException
  {
    m_reader = reader;
    m_split = split;
    m_line = line;
    m_ignoreEmptyLines = ignoreEmptyLines;

    fetchFile();
  }

  /**
   * Fetches the CSV-File. Closes the reader once finished.
   * 
   * @throws IOException
   */
  private void fetchFile( ) throws IOException
  {
    BufferedReader r = null;

    try
    {
      r = new BufferedReader( m_reader );

      String line = r.readLine();

      // steps over the lines until start-line-number is reached
      int lineNR = 1;
      while( lineNR < m_line && line != null )
      {
        line = r.readLine();
        lineNR++;
      }

      while( line != null )
      {
        if( m_ignoreEmptyLines && line.length() == 0 )
        {
          line = r.readLine();
          continue;
        }

        m_lines.add( line.split( m_split ) );

        line = r.readLine();
      }

      r.close();
    }
    catch( IOException e )
    {
      throw e;
    }
    finally
    {
      if( r != null )
        r.close();
    }
  }

  /**
   * @see org.kalypso.util.io.ITabledValues#getLines()
   */
  public int getLines( )
  {
    return m_lines.size();
  }

  /**
   * @see org.kalypso.util.io.ITabledValues#getItem(int, int)
   */
  public String getItem( final int row, final int col )
  {
    return ((String[]) m_lines.get( row ))[col];
  }

  /**
   * @see org.kalypso.util.io.ITabledValues#setItem(int, int, java.lang.String)
   */
  public void setItem( final int row, final int col, String element )
  {
    ((String[]) m_lines.get( row ))[col] = element;
  }

  /**
   * Saves the contents in the given Writer.
   * 
   * @param writer
   * 
   * @throws IOException
   */
  public void save( final Writer writer ) throws IOException
  {
    BufferedWriter bw = new BufferedWriter( writer );

    for( Iterator iter = m_lines.iterator(); iter.hasNext(); )
    {
      String[] items = (String[]) iter.next();

      for( int i = 0; i < items.length; i++ )
      {
        bw.write( items[i] );

        // note: m_split is used as separator, maybe that's not good enough to
        // use the same token as the one for spliting
        if( i != items.length - 1 )
          bw.write( m_split );
      }

      // end of line
      bw.write( '\n' );
    }
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