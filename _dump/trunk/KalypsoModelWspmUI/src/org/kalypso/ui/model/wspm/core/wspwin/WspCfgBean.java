/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;

/**
 * Represents the contents of an wsp.cfg file
 * 
 * @author belger
 */
public class WspCfgBean
{
  /** model type: 'b' is Pasche-TUHH, 'l' is PSW-Knauf */
  private char m_type;

  private File m_projectDir;

  private List<ZustandBean> m_zustaende = new ArrayList<ZustandBean>();

  public WspCfgBean( )
  {
  }

  public File getProjectDir( )
  {
    return m_projectDir;
  }

  public void setProjectDir( final File projectDir )
  {
    m_projectDir = projectDir;
  }

  public char getType( )
  {
    return m_type;
  }

  public void setType( char type )
  {
    m_type = type;
  }

  public void addZustand( final String name, final String waterName, final String fileName, final double startStation, final double endStation, final Date date )
  {
    m_zustaende.add( new ZustandBean( name, waterName, fileName, startStation, endStation, date ) );
  }

  public ZustandBean[] getZustaende( )
  {
    return m_zustaende.toArray( new ZustandBean[m_zustaende.size()] );
  }

  public static WspCfgBean read( final File wspwinDir ) throws IOException, ParseException
  {
    final File wspCfgFile = new File( WspWinImporter.getProfDir( wspwinDir ), "wsp.cfg" );

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( wspCfgFile ) );

      final WspCfgBean bean = new WspCfgBean();
      bean.setProjectDir( wspwinDir );

      final String firstLine = reader.readLine();
      if( firstLine == null || firstLine.length() == 0 )
        throw new ParseException( "First line of wsp.cfg is empty.", reader.getLineNumber() );

      // ignore the values, we read the count from the linecount
      // just parse the type
      final char type = firstLine.charAt( firstLine.length() - 1 );
      if( type != 'b' || type != 'l' )
        bean.setType( 'b' ); // default to pasche
      else
        bean.setType( type );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        // stop at empty line
        if( line == null || line.trim().length() == 0 )
          break;

        final StringTokenizer tokenizer = new StringTokenizer( line );
        if( tokenizer.countTokens() != 6 )
          throw new ParseException( "Wrong number of entries in line: " + reader.getLineNumber(), reader.getLineNumber() );

        try
        {
          final String waterName = tokenizer.nextToken();
          final String name = tokenizer.nextToken();
          // normally it should always be german, but it depends on the wspwin installation
          final DateFormat dateInstance = SimpleDateFormat.getDateInstance( SimpleDateFormat.SHORT, Locale.GERMAN );
          final Date date = dateInstance.parse( tokenizer.nextToken() );
          final Double start = new Double( tokenizer.nextToken() );
          final Double end = new Double( tokenizer.nextToken() );
          final String fileName = tokenizer.nextToken();

          bean.addZustand( name, waterName, fileName, start, end, date );
        }
        catch( final NumberFormatException e )
        {
          e.printStackTrace();
          throw new ParseException( "Wrong syntax in line: " + reader.getLineNumber(), reader.getLineNumber() );
        }

      }

      return bean;
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  public static void write( final WspCfgBean wspcfg, final File wspCfgFile )
  {
    throw new UnsupportedOperationException( "" + wspcfg + wspCfgFile );
  }

  /**
   * Reads the file profproj.txt
   */
  public ProfileBean[] readProfproj( final File wspwinDir ) throws IOException, ParseException
  {
    final File profprojFile = new File( WspWinImporter.getProfDir( wspwinDir ), "profproj.txt" );

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( profprojFile ) );

      final int[] counts = readStrHeader( reader );
      final int profilCount = counts[0];
      final int relationCount = counts[1];

      if( relationCount == 0 )
      {
        // ignore for now; later we may do sanity checks, if there are unused profiles
      }

      return ProfileBean.readProfiles( reader, profilCount );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /** Reads the first line of a profproj.txt or .str file. Returns 2 ints: profile count + second count. */
  public static int[] readStrHeader( LineNumberReader reader ) throws IOException, ParseException
  {
    final String firstLine = reader.readLine();
    if( firstLine == null || firstLine.length() == 0 )
      throw new ParseException( "First line of profproj.txt is empty.", reader.getLineNumber() );

    // ignore the values, we read the count from the linecount
    // just parse the type
    final StringTokenizer firstLineTokenizer = new StringTokenizer( firstLine );
    if( firstLineTokenizer.countTokens() < 2 )
      throw new ParseException( "Syntax of first line ist wrong.", reader.getLineNumber() );

    final int[] counts = new int[2];
    counts[0] = Integer.parseInt( firstLineTokenizer.nextToken() );
    counts[1] = Integer.parseInt( firstLineTokenizer.nextToken() );
    
    // if it is a .str file, we ignore the following name and waterName

    return counts;
  }

  public ZustandContentBean readZustand( final ZustandBean zustandBean ) throws IOException, ParseException
  {
    return zustandBean.readZustand( WspWinImporter.getProfDir( getProjectDir() ) );
  }

}
