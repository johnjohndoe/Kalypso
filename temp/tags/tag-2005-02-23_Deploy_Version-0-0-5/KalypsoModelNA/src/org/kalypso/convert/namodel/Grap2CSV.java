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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Writer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * this is a helper class that transformes the different types of grap-formated
 * files to csv file
 * 
 * @author doemminge
 */
public class Grap2CSV
{

  public static Pattern m_grapPattern = Pattern
      .compile( "([0-9]{2}.[0-9]{2}.[0-9]{4}.[0-9]{2}.[0-9]{2}.[0-9 ]{2}).+?([0-9\\.]+)" );

  public static Pattern m_datePattern = Pattern
      .compile( "([0-9]{2}) ([0-9]{2}) ([0-9]{4}) ([0-9]{2}) ([0-9]{2}) ([0-9 ]{2})" );

  public static void main( String[] args )
  {
    final File baseDir = new File( "/home/doemming/weisseElster" );
    try
    {
      convertDir( new File( baseDir, "klima.dat_org" ), new File( baseDir, "klima.dat" ),
          new GrapFilter( ".kz" ) );
      convertDir( new File( baseDir, "zufluss_org" ), new File( baseDir, "zufluss" ),
          new GrapFilter( ".kz" ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  public static void convertDir( File srcDir, File destDir, FileFilter filter ) throws IOException
  {
    if( !destDir.exists() )
      destDir.mkdirs();
    File[] files = srcDir.listFiles( filter );
    for( int i = 0; i < files.length; i++ )
      convertFile( files[i], destDir );
  }

  private static void convertFile( File srcFile, File destDir ) throws IOException
  {
    File destFile = new File( destDir, srcFile.getName() );
    Writer writer = new FileWriter( destFile );
    LineNumberReader reader = new LineNumberReader( new FileReader( srcFile ) );
    String lineIn = null;
    while( ( lineIn = reader.readLine() ) != null )
    {
      Matcher matcher = m_grapPattern.matcher( lineIn );
      if( matcher.matches() )
      {
        String date = matcher.group( 1 );
        String value = matcher.group( 2 );
        String formatedDate = date.replaceAll( "[:\\.]", " " );
        Matcher dateMatcher = m_datePattern.matcher( formatedDate );
        if( dateMatcher.matches() )
        {
          StringBuffer buffer = new StringBuffer();
          for( int i = 1; i <= dateMatcher.groupCount(); i++ )
          {
            if( i > 1 )
              buffer.append( " " ); // separator
            buffer.append( dateMatcher.group( i ).replaceAll( " ", "0" ) ); // correct
                                                                            // empty
                                                                            // fields
          }
          String correctDate = buffer.toString();
          writer.write( correctDate + "," + value + "\n" );
        }
      }
      else
        System.out.println( "not parseable: \"" + lineIn + "\"" );
    }
    reader.close();
    writer.close();
  }

  public static class GrapFilter implements FileFilter
  {
    private final String m_suffix;

    public GrapFilter( final String suffix )
    {
      m_suffix = suffix;
    }

    /**
     * @see java.io.FileFilter#accept(java.io.File)
     */
    public boolean accept( File pathname )
    {
      if( pathname.getPath().endsWith( m_suffix ) )
        return true;
      return false;
    }
  }
}