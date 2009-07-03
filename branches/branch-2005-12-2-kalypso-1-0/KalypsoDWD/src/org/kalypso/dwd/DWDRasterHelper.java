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
package org.kalypso.dwd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * Helper class for dwd raster based methodes
 * 
 * @author doemming
 */
public class DWDRasterHelper
{
  private final static Logger LOG = Logger.getLogger( DWDRasterHelper.class.getName() );

  private final static String DATUM = "([0-9]{10})";

  private final static String STUNDE = "([0-9]+)";

  private final static String KEY = "([0-9]+)";

  private final static Pattern HEADER_STATIC = Pattern.compile( " " + DATUM + " +" + KEY );
  private final static Pattern HEADER_DYNAMIC = Pattern.compile( " " + DATUM + " +" + KEY + " +" + STUNDE );

  private final static SimpleDateFormat DATEFORMAT_RASTER = new SimpleDateFormat( "yyMMddHHmm" );

  static
  {
    // REMARK: Wir setzen hier explizit die Zeitzone f�r die Dat�mer der LM Datei
    // Die Zeitzone ist Momentan uf 'GMT-1:0' gesetzt, da so die Daten identisch zum HWVOR00 (Saale) Modell interpretiert werden.
    // Dies ist vermutlich nicht richtig (TODO: verifizieren)
    // TODO: noch besser w�re es, die Zeitzone 'von aussen' konfigurierbar zu machen
    DATEFORMAT_RASTER.setTimeZone( TimeZone.getTimeZone( "GMT-1:00" ) );
  }

  /**
   * Return the date of the dwd forecast file. The date is coded in the file name.
   * <p>
   * Example filename for dwd raster format: "lm_2004_11_10_00" and its format would be 'lm_'yyyy'_'MM'_'dd'_'hh
   * <p>
   */
  public static Date getDateFromRaster( final File file, final SimpleDateFormat df )
  {
    try
    {
      return df.parse( file.getName() );
    }
    catch( final ParseException e )
    {
      LOG.warning( "DWD-Forecast filename \"" + file.getName() + "\" has not a valid format, should be:"
          + df.toPattern() );
      return null;
    }
  }

  /**
   * @param dwdKey
   */
  public static String getAxisTypeForDWDKey( int dwdKey )
  {
    switch( dwdKey )
    {
    case DWDRaster.KEY_RAIN:
      return TimeserieConstants.TYPE_RAINFALL;
    case DWDRaster.KEY_TEMP:
      return TimeserieConstants.TYPE_TEMPERATURE;
    }
    return null;
  }

  public static DWDRasterGeoLayer loadGeoRaster( final URL url, final String targetEpsg ) throws Exception
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new InputStreamReader( url.openStream() ) );
      String line = null;
      DWDRaster raster = null;
      DWDRaster xRaster = null;
      DWDRaster yRaster = null;
      final double factor = DWDRasterHelper.getFactorForDwdKey( DWDRaster.KEY_100000_LAT );
      final double offset = DWDRasterHelper.getOffsetForDwdKey( DWDRaster.KEY_100000_LAT );
      while( ( line = reader.readLine() ) != null )
      {
        final Matcher staticHeaderMatcher = HEADER_STATIC.matcher( line );
        if( staticHeaderMatcher.matches() )
        {
          if( raster != null && raster.getKey() == DWDRaster.KEY_100000_LAT )
            yRaster = raster;
          if( raster != null && raster.getKey() == DWDRaster.KEY_100000_LON )
            xRaster = raster;
          final Date date = DATEFORMAT_RASTER.parse( staticHeaderMatcher.group( 1 ) );
          final int key = Integer.parseInt( staticHeaderMatcher.group( 2 ) );
          raster = new DWDRaster( date, key );
          continue;
        }

        final String[] values = ( line.trim() ).split( " +", 13 );

        if( raster != null )
        {
          for( int i = 0; i < values.length; i++ )
            raster.addValue( ( Double.parseDouble( values[i] ) + offset ) * factor );
        }
      }
      if( raster != null && raster.getKey() == DWDRaster.KEY_100000_LAT )
        yRaster = raster;
      if( raster != null && raster.getKey() == DWDRaster.KEY_100000_LON )
        xRaster = raster;

      return new DWDRasterGeoLayer( targetEpsg, xRaster, yRaster );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /** Each value read from the raster is multiplied with this factor (applied AFTER the offset) */
  private static double getFactorForDwdKey( final int dwdKey )
  {
    switch( dwdKey )
    {
    case DWDRaster.KEY_HEIGHT: // [m]
    case DWDRaster.KEY_BEDECKUNG: // [%]
      return 1;
    case DWDRaster.KEY_TAU: // [GradC]
    case DWDRaster.KEY_TEMP: // [Kelvin]
      return 1d / 10d;
    case DWDRaster.KEY_RAIN: // [mm]
    case DWDRaster.KEY_SNOW: // [mm]
    case DWDRaster.KEY_WINDM: // [m/s]
    case DWDRaster.KEY_WINDZ: // [m/s]
      return 1d / 100d;
    case DWDRaster.KEY_100000_LAT: //[grad]
    case DWDRaster.KEY_100000_LON: //[grad]
      return 1d / 100000d;
    }
    return 1; // unknown
  }

  /** This offset is added to each value read from the raster (applied BEFORE the factor) */
  private static double getOffsetForDwdKey( final int dwdKey )
  {
    switch( dwdKey )
    {
    case DWDRaster.KEY_TEMP: // [Kelvin]
      return -2731.5;

    default:
      return 0; // unknown
    }
  }

  public static DWDObservationRaster loadObservationRaster( final URL url, final int dwdKey, final int maxCells )
      throws Exception
  {
    LineNumberReader reader = null;
    final double factor = DWDRasterHelper.getFactorForDwdKey( dwdKey );
    final double offset = DWDRasterHelper.getOffsetForDwdKey( dwdKey );

    try
    {
      reader = new LineNumberReader( new InputStreamReader( url.openStream() ) );
      int lmVersion = 0;
      String line = null;
      DWDObservationRaster raster = null;
      Date date = null;
      int cellpos = 0;
      while( ( line = reader.readLine() ) != null )
      {
        final Matcher dynamicHeaderMatcher = HEADER_DYNAMIC.matcher( line );
        if( dynamicHeaderMatcher.matches() ) // lm1
        {
          final Date startDate = DATEFORMAT_RASTER.parse( dynamicHeaderMatcher.group( 1 ) );
          final int key = Integer.parseInt( dynamicHeaderMatcher.group( 2 ) );
          final long hour = Long.parseLong( dynamicHeaderMatcher.group( 3 ) );
          // ARG: use calendar to add hours to date
          date = new Date( startDate.getTime() + 60 * 60 * 1000 * hour );
          if( key == dwdKey )
          {
            if( raster == null ) // if not allready loading
              raster = new DWDObservationRaster( key, maxCells );
          }
          else if( raster != null )
            return raster;
          lmVersion = 1;
          cellpos = 0;
          continue;
        }
        final Matcher staticHeaderMatcher = HEADER_STATIC.matcher( line );
        if( staticHeaderMatcher.matches() ) // lm2 ??
        {
          date = DATEFORMAT_RASTER.parse( staticHeaderMatcher.group( 1 ) );
          final int key = Integer.parseInt( staticHeaderMatcher.group( 2 ) );
          if( key == dwdKey )
            raster = new DWDObservationRaster( key, maxCells );
          else if( raster != null )
            return raster;
          lmVersion = 2;
          cellpos = 0;
          continue;
        }
        if( raster == null )
          continue;

        switch( lmVersion )
        {
        case 1:
          final String[] values;
          values = ( line.trim() ).split( " +", 13 );
          for( int i = 0; i < values.length; i++ )
          {
            final double value = Double.parseDouble( values[i] );
            raster.setValueFor( date, cellpos, ( value + offset ) * factor );
            cellpos++;
          }
          break;
        case 2:
          values = ( line.trim() ).split( " +" );
          for( int i = 0; i < values.length; i++ )
          {
            final double value = Double.parseDouble( values[i] );
            raster.setValueFor( date, cellpos, ( value + offset ) * factor );
            cellpos++;
            if( cellpos >= maxCells )
            {
              long time = date.getTime(); // add one hour
              date = new Date( time + 1000l * 60l * 60l );
              cellpos = 0;
            }
          }
          break;
        }
      }
      return raster;
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * Update the file contents so that the dates reflect the date found in the filename
   * <p>
   * This is for instance used in the context of test scenario with PSICompact where the filename of a historical
   * dwd-file gets the current date but the file content is left unchanged. Kalypso uses the dates found in the file so
   * we need to update it.
   */
  public static void updateDWDFileContents( final File srcFile, final File destFile, final SimpleDateFormat df )
      throws IOException, DWDException
  {
    final Date date = getDateFromRaster( srcFile, df );
    if( date == null )
      throw new DWDException( "Date could not be fetched from dwd-filename: " + srcFile.getName() );

    final SimpleDateFormat dwdf = new SimpleDateFormat( "yyMMddHHmm" );
    final String strDate = dwdf.format( date );

    final Pattern pattern = Pattern.compile( " [0-9]{10}( .*)" );

    int count = 0;

    BufferedReader reader = null;
    BufferedWriter writer = null;
    try
    {
      reader = new BufferedReader( new FileReader( srcFile ) );
      writer = new BufferedWriter( new FileWriter( destFile ) );
      String line = reader.readLine();
      while( line != null )
      {
        final Matcher matcher = pattern.matcher( line );
        final String newLine;
        if( matcher.matches() )
        {
          newLine = " " + strDate + matcher.group( 1 );
          count++;
        }
        else
          newLine = line;

        writer.write( newLine );
        writer.newLine();

        line = reader.readLine();
      }
    }
    finally
    {
      IOUtils.closeQuietly( reader );
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Parses the date from the first line of a lm file.
   */
  public static Date dateFromFirstLine( final String line )
  {
    final Matcher staticHeaderMatcher = HEADER_DYNAMIC.matcher( line );
    if( staticHeaderMatcher.matches() )
    {
      try
      {
        final String dateString = staticHeaderMatcher.group( 1 );
        System.out.println( "Parsing date string: " + dateString );
        return DATEFORMAT_RASTER.parse( dateString );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
      }
    }

    return null;
  }

  /**
   * Read the first line from a file.
   * <p>
   * Remark: we put this method here instead of one of the FileUtility classes, so we may deploy the DWDServlet without
   * too many dependencies.
   * 
   * @return null, f the file is empty or could not be read.
   */
  public static String readFirstLine( final File file )
  {
    BufferedReader r = null;
    try
    {
      r = new BufferedReader( new FileReader( file ) );
      final String result = r.readLine();
      r.close();
      return result;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( r );
    }
  }
}