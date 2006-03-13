package org.kalypso.convert.namodel.timeseries.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.TreeMap;

import junit.framework.TestCase;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;

/**
 * @author flows
 */
public class BlockTimeSeriesTest extends TestCase
{
  public void testBlocktimeSeries() throws IOException
  {

    TreeMap map = load( "./resources/test.dat", "103" );
    TreeMap map2 = load( "resources/qgs.dat", "103" );
    assertNotNull(map);
    assertNotNull(map2);
  }

  public TreeMap load( String resource, String key ) throws IOException
  {
    BlockTimeSeries block = new BlockTimeSeries();
    File tmpFile = File.createTempFile( "block", "txt" );
    InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    FileWriter fileWriter = new FileWriter( tmpFile );
    CopyUtils.copy( resourceAsStream, fileWriter );
    IOUtils.closeQuietly( fileWriter );
    block.importBlockFile( tmpFile );
    TreeMap map = block.getTimeSerie( key );
    assertNotNull( map );
    System.out.println( " von " + map.firstKey() );
    System.out.println( " bis " + map.lastKey() );
    return map;
  }
}
