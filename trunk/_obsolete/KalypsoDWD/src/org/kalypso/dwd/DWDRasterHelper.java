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
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.PrefixFileFilter;
import org.kalypso.contribs.java.io.IOUtilities;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * Helper class for dwd raster based methods
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
    // Die Zeitzone ist Momentan uf 'GMT-1:0' gesetzt, da so die Daten identisch zum HWVOR00 (Saale) Modell
    // interpretiert werden.
    // Dies ist vermutlich nicht richtig (TODO: verifizieren)
    // TODO: noch besser w�re es, die Zeitzone 'von aussen' konfigurierbar zu machen
    DATEFORMAT_RASTER.setTimeZone( TimeZone.getTimeZone( "GMT+1:00" ) );
  }

  /**
   * Return the most recent DWD file from the given folder, or null if nothing found.
   * 
   * @param srcDir
   *          folder to look at
   * @param prefix
   *          prefix used for filtering files from source folder
   * @param df
   *          dateformat used for parsing the file name and to extract date from it
   * @param removeOthers
   *          when true other older dwd forecasts are deleted
   * @deprecated This method is now implemented in DWDCopyTask for use from the DVDFileCopyServlet. It seems that the
   *             method isn't used form any other class anymore.
   */
  @Deprecated
  public static File getNewestFile( final File srcDir, final String prefix, final SimpleDateFormat df, final boolean removeOthers )
  {
    final FileFilter filter = new PrefixFileFilter( prefix );
    final File[] files = srcDir.listFiles( filter );

    if( files == null )
      return null;

    File result = null;
    Date date = null;

    // search newest...
    for( final File file : files )
    {
      if( file.isDirectory() )
        continue;

      final Date testdate = getDateFromRaster( file, df );
      if( testdate == null )
        continue;

      if( result == null )
      {
        result = file;
        date = testdate;
      }
      else if( testdate.after( date ) )
      {
        result = file;
        date = testdate;
      }
    }

    if( result == null )
      return null;

    if( removeOthers )
    {
      // got it, so now remove others
      for( final File file : files )
      {
        if( !file.isDirectory() )
        {
          final Date d = getDateFromRaster( file, df );
          if( d != null && d.before( date ) )
          {
            LOG.info( "Removing old DWD-Forecast file: " + file.getName() );
            file.delete();
          }
        }
      }
    }

    return result;
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
      LOG.warning( "DWD-Forecast filename \"" + file.getName() + "\" has not a valid format, should be:" + df.toPattern() );
      return null;
    }
  }

  /**
   * @param dwdKey
   */
  public static String getAxisTypeForDWDKey( final int dwdKey )
  {
    switch( dwdKey )
    {
      case DWDRaster.KEY_RAIN:
        return ITimeseriesConstants.TYPE_RAINFALL;
      case DWDRaster.KEY_TEMP:
        return ITimeseriesConstants.TYPE_TEMPERATURE;
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
      while( (line = reader.readLine()) != null )
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

        final String[] values = (line.trim()).split( " +", 13 );

        if( raster != null )
        {
          for( final String value : values )
            raster.addValue( (Double.parseDouble( value ) + offset) * factor );
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
      case DWDRaster.KEY_100000_LAT: // [grad]
      case DWDRaster.KEY_100000_LON: // [grad]
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

  /**
   * This function returns the unit for the given key.
   * 
   * @param dwdKey
   *          The key.
   * @return The unit.
   */
  private static String getUnitForDwdKey( final int dwdKey )
  {
    String unit = "";

    switch( dwdKey )
    {
      case DWDRaster.KEY_HEIGHT:
        unit = "m";
        break;
      case DWDRaster.KEY_BEDECKUNG:
        unit = "%";
        break;
      case DWDRaster.KEY_TAU:
        unit = "GradC";
        break;
      case DWDRaster.KEY_TEMP:
        unit = "Kelvin";
        break;
      case DWDRaster.KEY_RAIN:
        unit = "mm";
        break;
      case DWDRaster.KEY_SNOW:
        unit = "mm";
        break;
      case DWDRaster.KEY_WINDM:
        unit = "m/s";
        break;
      case DWDRaster.KEY_WINDZ:
        unit = "m/s";
        break;
      case DWDRaster.KEY_100000_LAT:
        unit = "grad";
        break;
      case DWDRaster.KEY_100000_LON:
        unit = "grad";
        break;
    }

    return unit;
  }

  public static DWDObservationRaster loadObservationRaster( final URL url, final int dwdKey ) throws Exception
  {
    final double factor = getFactorForDwdKey( dwdKey );
    final double offset = getOffsetForDwdKey( dwdKey );
    final String unit = getUnitForDwdKey( dwdKey );

    LineNumberReader lineNumberReader = null;
    try
    {
      /* Create the reader. */
      InputStream inputStream = IOUtilities.getInputStream( url );
      InputStreamReader inputStreamReader = new InputStreamReader( inputStream );
      lineNumberReader = new LineNumberReader( inputStreamReader );

      int lmVersion = 0;
      String line = null;
      DWDObservationRaster raster = null;
      long blockDate = -1;
      int cellpos = 0;
      boolean rightBlock = false; // Is only used by the dynamic header (lmVersion = 1)
      while( (line = lineNumberReader.readLine()) != null )
      {
        final Matcher dynamicHeaderMatcher = HEADER_DYNAMIC.matcher( line );
        if( dynamicHeaderMatcher.matches() ) // lm1
        {
          final Date startDate = DATEFORMAT_RASTER.parse( dynamicHeaderMatcher.group( 1 ) );
          final Calendar startCal = Calendar.getInstance();
          startCal.setTime( startDate );
          final int key = Integer.parseInt( dynamicHeaderMatcher.group( 2 ) );
          final int hour = Integer.parseInt( dynamicHeaderMatcher.group( 3 ) );

          startCal.add( Calendar.HOUR_OF_DAY, hour + 1 );
          blockDate = startCal.getTimeInMillis();

          if( key == dwdKey )
          {
            rightBlock = true;
            if( raster == null ) // if not already loading
              raster = new DWDObservationRaster( key, unit );
          }
          else
            rightBlock = false; // wrong key, but reading the file must be continued, the key can appear again

          lmVersion = 1;
          cellpos = 0;
          continue;
        }

        final Matcher staticHeaderMatcher = HEADER_STATIC.matcher( line );
        if( staticHeaderMatcher.matches() ) // lm2 ??
        {
          // TODO: this is stil not the correct way to parse the date...
          // Google, how to do it correctly...
          blockDate = DATEFORMAT_RASTER.parse( staticHeaderMatcher.group( 1 ) ).getTime();
          final int key = Integer.parseInt( staticHeaderMatcher.group( 2 ) );

          if( key == dwdKey )
            raster = new DWDObservationRaster( key, unit );
          else if( raster != null )
            return raster;

          lmVersion = 2;
          cellpos = 0;
          continue;
        }

        if( raster == null )
          continue;

        /* If we are reading lmVersion = 1 and we have a wrong key, continue. */
        if( lmVersion == 1 && rightBlock == false )
          continue;

        switch( lmVersion )
        {
          case 1:
          {
            final String[] values = readValues( line, 5 ); // Do not trim the line...
            for( final String value2 : values )
            {
              final double value = Double.parseDouble( value2 );
              raster.setValueFor( new Date( blockDate ), cellpos, (value + offset) * factor );
              cellpos++;
            }
            break;
          }

          case 2:
          {
            /* One line represents all 78 values for one position. */
            final String[] values = readValues( line, 5 ); // Do not trim the line...
            final Calendar valueDate = Calendar.getInstance();
            valueDate.setTimeInMillis( blockDate );
            for( final String valueStr : values )
            {
              valueDate.add( Calendar.HOUR_OF_DAY, 1 ); // starting with hour 1, so add first here!
              final double value = Double.parseDouble( valueStr );
              raster.setValueFor( valueDate.getTime(), cellpos, (value + offset) * factor );
            }

            cellpos++;
            break;
          }
        }
      }

      return raster;
    }
    finally
    {
      IOUtils.closeQuietly( lineNumberReader );
    }
  }

  private static String[] readValues( String line, int numberChars )
  {
    /* Memory for the results. */
    List<String> values = new ArrayList<String>();

    /* The number of read chars. */
    int readChars = 0;

    /* Read as long it is possible. */
    while( readChars < line.length() )
    {
      /* Read the next sequence of chars. */
      String value = line.substring( readChars, readChars + numberChars );

      /* Add the value. */
      values.add( value );

      /* Increase the number of read chars. */
      readChars = readChars + numberChars;
    }

    return values.toArray( new String[] {} );
  }

  /**
   * Update the file contents so that the dates reflect the date found in the filename
   * <p>
   * This is for instance used in the context of test scenario with PSICompact where the filename of a historical
   * dwd-file gets the current date but the file content is left unchanged. Kalypso uses the dates found in the file so
   * we need to update it.
   */
  public static void updateDWDFileContents( final File srcFile, final File destFile, final SimpleDateFormat df ) throws IOException, DWDException
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