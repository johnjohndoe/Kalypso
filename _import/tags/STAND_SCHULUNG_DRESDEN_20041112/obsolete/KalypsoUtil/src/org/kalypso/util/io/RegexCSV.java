package org.kalypso.util.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Iterator;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses a CSV file. Save is possible using a Writer.
 * 
 * @author schlienger
 */
public class RegexCSV implements ITabledValues
{
  private final Reader m_reader;

  private final Vector m_lines = new Vector();

  private final int m_line;

  private final Pattern m_pattern;

  private final boolean m_ignoreEmptyLines;

  /**
   * Constructor. Fetches the file and closes the reader;
   * 
   * @param reader
   * @param p
   *          regex pattern to use when disecating the lines
   * @param line
   *          the line number to start reading the values at
   * @param ignoreEmptyLines
   *          when true empty lines are ignored and not added to this object
   * 
   * @throws IOException
   */
  public RegexCSV( final Reader reader, final Pattern p, final int line,
      final boolean ignoreEmptyLines ) throws IOException
  {
    m_reader = reader;
    m_line = line;
    m_pattern = p;
    m_ignoreEmptyLines = ignoreEmptyLines;

    fetchFile();
  }

  /**
   * Fetches the CSV-File. Closes the reader once finished.
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
        
        Matcher m = m_pattern.matcher( line );
        if( m.matches() )
        {
          String[] sLine = new String[m.groupCount()];

          for( int i = 0; i < sLine.length; i++ )
            sLine[i] = m.group( i + 1 );

          m_lines.add( sLine );
        }
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
   * Returns the number of lines fetched from the CSV-File.
   */
  public int getLines( )
  {
    return m_lines.size();
  }

  /**
   * Returns the item at the given position in the CSV-File.
   */
  public String getItem( final int row, final int col )
  {
    return ((String[]) m_lines.get( row ))[col];
  }

  /**
   * Sets the item at the given position.
   */
  public void setItem( final int row, final int col, String element )
  {
    ((String[]) m_lines.get( row ))[col] = element;
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
      String[] items = (String[]) iter.next();

      for( int i = 0; i < items.length; i++ )
      {
        bw.write( items[i] );

        // note: m_split is used as separator, maybe that's not good enough to
        // use the same token as the one for spliting
        if( i != items.length - 1 )
          bw.write( ";" );
      }

      // end of line
      bw.write( '\n' );
    }
  }
}