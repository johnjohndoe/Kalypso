package org.kalypso.util.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Iterator;
import java.util.Vector;

/**
 * Parses a CSV file. Save is possible using a Writer.
 * 
 * @author schlienger
 */
public class CSV
{
  private final Reader m_reader;

  private final String m_split;

  private final Vector m_lines = new Vector();

  private final int m_line;

  /**
   * Constructor. Fetches the file and closes the reader;
   * 
   * @param reader
   * @param split the string used for spliting each line into chunks
   * @param line the line number to start reading the values at
   * 
   * @throws IOException
   */
  public CSV( final Reader reader, final String split, final int line ) throws IOException
  {
    m_reader = reader;
    m_split = split;
    m_line = line;

    fetchFile();
  }

  /**
   * Fetches the CSV-File. Closes the reader once finished.
   */
  private void fetchFile() throws IOException
  {
    BufferedReader r = new BufferedReader( m_reader );

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
      m_lines.add( line.split( m_split ) );

      line = r.readLine();
    }

    r.close();
  }

  /**
   * Returns the number of lines fetched from the CSV-File.
   */
  public int getLines()
  {
    return m_lines.size();
  }

  /**
   * Returns the item at the given position in the CSV-File.
   */
  public String getItem( final int row, final int col )
  {
    return ( (String[])m_lines.get( row ) )[col];
  }

  /**
   * Sets the item at the given position.
   */
  public void setItem( final int row, final int col, String element )
  {
    ( (String[])m_lines.get( row ) )[col] = element;
  }

  /**
   * Saves the contents in the given Writer.
   * 
   * @throws IOException
   */
  public void save( final Writer writer ) throws IOException
  {
    BufferedWriter bw = new BufferedWriter( writer );

    for( Iterator iter = m_lines.iterator(); iter.hasNext(); )
    {
      String[] items = (String[])iter.next();

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
}