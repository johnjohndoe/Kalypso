package org.kalypso.util.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.text.ParseException;
import java.util.Vector;

import org.kalypso.util.factory.ValueObjectFactory;

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
  public String getItem( int line, int col )
  {
    return ( (String[])m_lines.get( line ) )[col];
  }

  /**
   * Returns the item as 'value Object' at the given position in the CSV-File.
   * <p>
   * Convenience method that returns a value object initialised with the string
   * value of the item. The possible types of Object that are returned depends
   * on the availability of their Class in the
   * ValueObjectFactory.createObjectWithStringValue() method.
   */
  public Object getItem( int line, int col, Class c ) throws ParseException
  {
    return ValueObjectFactory.createObjectWithStringValue( c, getItem( line, col ) );
  }

  /**
   * Sets the item at the given position.
   */
  public void setItem( int index, int col, String element )
  {
    ((String[])m_lines.get( index ))[col] = element;
  }
}
