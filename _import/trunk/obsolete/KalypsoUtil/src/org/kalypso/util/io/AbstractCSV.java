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
import java.io.Reader;
import java.io.Writer;
import java.util.Iterator;
import java.util.Vector;

/**
 * AbstractCSV
 * 
 * @author schlienger
 */
public abstract class AbstractCSV implements ITabledValues
{
  protected final Vector m_lines = new Vector();

  protected final int m_startLine;

  protected final boolean m_ignoreEmptyLines;

  protected String m_commentedLineBeginString = null;
  
  /** separator used for serializing the contents. It is a semicolon by default. */
  private String m_separator = ";";

  /**
   * Constructor
   * 
   * @param line
   *          the line number to start reading the values at
   * @param ignoreEmptyLines
   *          when true, empty lines are not stored in this object
   */
  public AbstractCSV( final int startLine, final boolean ignoreEmptyLines )
  {
    m_startLine = startLine;
    m_ignoreEmptyLines = ignoreEmptyLines;
  }

  /**
   * Sets the string that delimits the commented lines
   * 
   * @param str
   */
  public void setCommentedLineBeginString( final String str )
  {
    m_commentedLineBeginString = str;
  }
  
  /**
   * @param separator The separator to set.
   */
  public void setSeparator( String separator )
  {
    m_separator = separator;
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
   * Fetch the CSV-Values from the reader. Caller should take care of closing
   * the reader.
   * 
   * @param reader
   */
  public void fetch( final Reader reader ) throws IOException
  {
    final BufferedReader r = new BufferedReader( reader );

    String line = r.readLine();

    // steps over the lines until start-line-number is reached
    int lineNR = 1;
    while( lineNR < m_startLine && line != null )
    {
      line = r.readLine();
      lineNR++;
    }

    while( line != null )
    {
      if( m_ignoreEmptyLines && line.length() == 0
          || m_commentedLineBeginString != null
          && line.startsWith( m_commentedLineBeginString ) )
      {
        line = r.readLine();
        continue;
      }

      handleCurrentLine( line );

      line = r.readLine();
    }
  }

  /**
   * @param line
   */
  protected abstract void handleCurrentLine( final String line );
  
  /**
   * Saves the contents in the given Writer
   * 
   * @param writer
   * 
   * @throws IOException
   */
  public void save( final Writer writer ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( writer );

    for( Iterator iter = m_lines.iterator(); iter.hasNext(); )
    {
      String[] items = (String[]) iter.next();

      for( int i = 0; i < items.length; i++ )
      {
        bw.write( items[i] );

        if( i != items.length - 1 )
          bw.write( m_separator );
      }

      // end of line
      bw.write( '\n' );
    }
  }
}