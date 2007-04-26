package org.kalypso.convert.namodel.timeseries.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;

/**
 * @author flows
 */
public class BlockTimeSeriesTest extends TestCase
{
  public void testBlocktimeSeries( ) throws IOException
  {
    // 25.3.1990 Umstellung
    //    
    // TreeMap map = load( "./resources/test.dat", "103" );
    // TreeMap map2 = load( "resources/qgs.dat", "103" );
    // assertNotNull( map );
    // assertNotNull( map2 );
     TreeMap map3 = load( "./resources/WinterSommerTest.dat", "103" );
//    TreeMap map3 = load( "./resources/WinterSommer24hTest.dat", "103" );
    check( map3, 28 );
  }

  private void check( TreeMap map, int max )
  {
    final String[] timeZones = new String[] {

    "UTC", "GMT", "CET", "GMT+1" };

    final TimeZone default1 = TimeZone.getDefault();
    final DateFormat defaultFormat = new SimpleDateFormat();
    defaultFormat.setTimeZone( default1 );

    final DateFormat naFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat() );

    final Set set = map.keySet();
    final Iterator iterator = set.iterator();
    Date last = null;
    int i = 0;
    while( iterator.hasNext() && i < max )
    {
      final Date date = (Date) iterator.next();
      System.out.println( "\n" + i + " value:" + map.get( date ) );
      for( int j = 0; j < timeZones.length; j++ )
      {
        String timeZone = timeZones[j];
        final TimeZone tz = TimeZone.getTimeZone( timeZone );
        final DateFormat dFormat = new SimpleDateFormat();
        dFormat.setTimeZone( tz );
        System.out.println( tz.getID() + ":" + dFormat.format( date ) );
      }

      System.out.println( "NA :" + naFormat.format( date ) );
      if( last != null )
        System.out.println( date.getTime() - last.getTime() );
      last = date;
      i++;
    }
  }

  public TreeMap load( String resource, String key ) throws IOException
  {
    TimeZone timeZone = TimeZone.getTimeZone( "GMT+1" );
    final BlockTimeSeries block = new BlockTimeSeries( timeZone );
    final File tmpFile = File.createTempFile( "block", "txt" );
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    final FileWriter fileWriter = new FileWriter( tmpFile );
    IOUtils.copy( resourceAsStream, fileWriter );
//    CopyUtils.copy( resourceAsStream, fileWriter );
    IOUtils.closeQuietly( fileWriter );
    block.importBlockFile( tmpFile );
    final TreeMap map = block.getTimeSerie( key );
    assertNotNull( map );
    System.out.println( " von " + map.firstKey() );
    System.out.println( " bis " + map.lastKey() );
    return map;
  }
}
