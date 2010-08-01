package org.kalypso.model.hydrology.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedMap;
import java.util.TimeZone;

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
    final SortedMap<Date, String> map3 = load( "resources/WinterSommerTest.dat", "103" ); //$NON-NLS-1$ //$NON-NLS-2$
    check( map3, 28 );
  }

  private void check( final SortedMap<Date, String> map, final int max )
  {
    final String[] timeZones = new String[] { "UTC", "GMT", "CET", "GMT+1" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    final TimeZone default1 = TimeZone.getDefault();
    final DateFormat defaultFormat = new SimpleDateFormat();
    defaultFormat.setTimeZone( default1 );

    final DateFormat naFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat() );

    final Set<Date> set = map.keySet();
    final Iterator<Date> iterator = set.iterator();
    Date last = null;
    int i = 0;
    while( iterator.hasNext() && i < max )
    {
      final Date date = iterator.next();
      System.out.println( "\n" + i + " value:" + map.get( date ) ); //$NON-NLS-1$ //$NON-NLS-2$
      for( final String timeZone : timeZones )
      {
        final TimeZone tz = TimeZone.getTimeZone( timeZone );
        final DateFormat dFormat = new SimpleDateFormat();
        dFormat.setTimeZone( tz );
        System.out.println( tz.getID() + ":" + dFormat.format( date ) ); //$NON-NLS-1$
      }

      System.out.println( "NA :" + naFormat.format( date ) ); //$NON-NLS-1$
      if( last != null )
        System.out.println( date.getTime() - last.getTime() );
      last = date;
      i++;
    }
  }

  private SortedMap<Date, String> load( final String resource, final String key ) throws IOException
  {
    final TimeZone timeZone = TimeZone.getTimeZone( "GMT+1" ); //$NON-NLS-1$
    final BlockTimeSeries block = new BlockTimeSeries( timeZone );
    final File tmpFile = File.createTempFile( "block", "txt" ); //$NON-NLS-1$ //$NON-NLS-2$
    tmpFile.deleteOnExit();
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    final FileWriter fileWriter = new FileWriter( tmpFile );
    IOUtils.copy( resourceAsStream, fileWriter );
    IOUtils.closeQuietly( fileWriter );
    block.importBlockFile( tmpFile );
    final SortedMap<Date, String> map = block.getTimeSerie( key );
    assertNotNull( map );
    System.out.println( " von " + map.firstKey() ); //$NON-NLS-1$
    System.out.println( " bis " + map.lastKey() ); //$NON-NLS-1$
    return map;
  }
}
