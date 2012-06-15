package org.kalypso.model.hydrology.internal.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.postprocessing.Block;
import org.kalypso.model.hydrology.internal.postprocessing.BlockTimeSeries;

/**
 * @author flows
 */
public class BlockTimeSeriesTest extends TestCase
{
  public void testBlocktimeSeries( ) throws IOException
  {
    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IKalypsoCorePreferences.DISPLAY_TIMEZONE, "GMT+1" );

//    final Block block = load( "resources/timeseries.dat", "4500" ); //$NON-NLS-1$ //$NON-NLS-2$
    final Block block = load( "resources/WinterSommerTest.dat", "103" ); //$NON-NLS-1$ //$NON-NLS-2$
    check( block );
  }

  private void check( final Block block )
  {
    final String[] timeZones = new String[] { "UTC", "GMT", "CET", "GMT+1" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    final TimeZone default1 = TimeZone.getDefault();
    final DateFormat defaultFormat = new SimpleDateFormat();
    defaultFormat.setTimeZone( default1 );

    final DateFormat naFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat() );

    final Date[] dates = block.getDates();

    Date last = null;
    int i = 0;
    for( final Date date : dates )
    {
      System.out.println( "\n" + i + " value:" + block.getValue( date ) ); //$NON-NLS-1$ //$NON-NLS-2$
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

  private Block load( final String resource, final String key ) throws IOException
  {
    final TimeZone timeZone = TimeZone.getTimeZone( "GMT+1" ); //$NON-NLS-1$
    final BlockTimeSeries blockTimeseries = new BlockTimeSeries( timeZone );
    final File tmpFile = File.createTempFile( "block", "txt" ); //$NON-NLS-1$ //$NON-NLS-2$
    tmpFile.deleteOnExit();
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    final FileWriter fileWriter = new FileWriter( tmpFile );
    IOUtils.copy( resourceAsStream, fileWriter );
    IOUtils.closeQuietly( fileWriter );
    blockTimeseries.importBlockFile( tmpFile );
    final Block block = blockTimeseries.getTimeSerie( key );
    assertNotNull( block );
//    System.out.println( " von " + map.firstKey() ); //$NON-NLS-1$
//    System.out.println( " bis " + map.lastKey() ); //$NON-NLS-1$
    return block;
  }
}
