package org.kalypso.util.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Vector;

/**
 * Can parse a CSV file.
 * 
 * TODO: save file.
 * 
 * @author schlienger
 */
public class CSV
{
  private final Reader m_reader;

  private final String m_split;

  private final Vector m_lines = new Vector();

  /**
   * Constructor. Fetches the file and closes the reader;
   * @throws IOException
   */
  public CSV( final Reader reader, final String split ) throws IOException
  {
    m_reader = reader;
    m_split = split;
    
    fetchFile();
  }

  /**
   * Fetches the CSV-File.
   */
  private void fetchFile() throws IOException
  {
    BufferedReader r = new BufferedReader( m_reader );

    String line = r.readLine();
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
    ((String[])m_lines.get( row ))[col] = element;
  }
}
