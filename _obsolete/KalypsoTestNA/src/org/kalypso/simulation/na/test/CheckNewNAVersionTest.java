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
package org.kalypso.simulation.na.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.logging.Level;

import junit.framework.TestCase;

import org.kalypso.commons.diff.DiffLogger;
import org.kalypso.commons.diff.DiffUtils;
import org.kalypso.commons.diff.IDiffComparator;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffObject;

/**
 * @author kuepfer
 */

public class CheckNewNAVersionTest extends TestCase
{
  private final static String BASE_DIR = "C://Dokumente und Einstellungen//kuepfer//Local Settings//Temp"; // "C://temp//NAModell";

  private final static String BASE_DIR_OLD = BASE_DIR.concat( "//CalcJob-2-Nullvariante//hqJobs//HQ1//calcDir//out_we.nat" );// "//old"

  // );

  // private final static String BASE_DIR_OLD = BASE_DIR.concat( "//out_we.nat" );// "//old" );
  private final static String BASE_DIR_NEW = BASE_DIR.concat( "//CalcJob-4-XPlanung//hqJobs//HQ1//calcDir//out_we.nat" );

  private final static String FILENAME_CONTROL = BASE_DIR.concat( "//start//we_nat_start.txt" );

  private final ArrayList<String> m_nodes = new ArrayList<String>();

  private final ArrayList<String> m_catchments = new ArrayList<String>();

  private final ArrayList<String> m_channels = new ArrayList<String>();

  final String DELIMITTER = "9999";

  final StringBuffer buffer = new StringBuffer();

  final ILogger logger = new ILogger()
  {
    /**
     * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, boolean, java.lang.String)
     */
    public void log( final Level level, final int code, final String message )
    {
      System.out.println( message );
      buffer.append( message );
      buffer.append( "\n" );
    }
  };

  public void testResults( ) throws Exception
  {
    readControl();
// boolean fileChecked = false;
    final StringBuffer sb = new StringBuffer();
    final DiffLogger diffLogger = new DiffLogger( logger );
    final File dirOld = new File( BASE_DIR_OLD );
    final File dirNew = new File( BASE_DIR_NEW );
    sb.append( "\n\n######### File Check Summery #########\n" );
    sb.append( "-- BaseDir1: " + BASE_DIR_OLD + " <---> BaseDir2: " + BASE_DIR_NEW.concat( " --\n" ) );
    final String[] oldFiles = dirOld.list();
    final String[] newFiles = dirNew.list();
    if( oldFiles.length == newFiles.length && oldFiles.length > 0 )
    {
      sb.append( "-- " + oldFiles.length + " Files to compare --\n" );

      for( int i = 0; i < oldFiles.length; i++ )
      {
        final String oldFileName = oldFiles[i];
        final String newFileName = newFiles[i];
        sb.append( "old: " + oldFileName + " new: " + newFileName );
        if( oldFileName.equals( newFileName ) )
        {
          final File oldFile = new File( dirOld.getAbsolutePath().concat( "//".concat( oldFileName ) ) );
          final File newFile = new File( dirNew.getAbsolutePath().concat( "//".concat( newFileName ) ) );
          diffLogger.log( IDiffComparator.DIFF_INFO, "Datei (1) '" + oldFileName + "' erstellt am: " + new Date( oldFile.lastModified() ) );
          diffLogger.log( IDiffComparator.DIFF_INFO, "Datei (2) '" + newFileName + "' erstellt am: " + new Date( newFile.lastModified() ) );
          // fill objects
          final BlockTimeSeries oldSeries = new BlockTimeSeries();
          final BlockTimeSeries newSeries = new BlockTimeSeries();
          newSeries.importBlockFile( newFile );
          oldSeries.importBlockFile( oldFile );
          final BlockTimeSeriesDiffObject diff1 = new BlockTimeSeriesDiffObject( oldFile );
          final BlockTimeSeriesDiffObject diff2 = new BlockTimeSeriesDiffObject( newFile );
          // make diff
          DiffUtils.diff( logger, diff1, diff2, null );
        }

      }

    }
    else
    {
      System.out.println( "Die Anzahl der Dateien ist nicht identisch!\nAnzahl(1): " + oldFiles.length + "\tAnzahl(2): " + newFiles.length );
      System.exit( 0 );
    }
    final String fileStats = sb.toString();
    System.out.print( fileStats );
    final FileWriter diffWriter = new FileWriter( BASE_DIR.concat( "\\diff.res" ) );
    diffWriter.write( fileStats.concat( buffer.toString() ) );
    diffWriter.close();
  }

  private void readControl( )
  {
    URL url = null;
    try
    {
      url = new File( FILENAME_CONTROL ).toURL();
      final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );
      // skip first line (Timestep dt) in control file
      String line = reader.readLine();
      int i = 0;
      while( line != null )
      {
        line = reader.readLine();
        if( line != null && (!line.startsWith( "j" ) && !line.startsWith( "n" )) )
        {
          while( i < 2 )
          {
            while( !line.startsWith( DELIMITTER ) )
            {
              if( i == 0 )
                m_nodes.add( line.trim() );
              if( i == 1 )
                m_catchments.add( line.trim() );
              if( i == 2 )
                m_channels.add( line.trim() );
              line = reader.readLine();
            }
            i++;
            line = reader.readLine();
          }
        }
      }

    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

  }
}
