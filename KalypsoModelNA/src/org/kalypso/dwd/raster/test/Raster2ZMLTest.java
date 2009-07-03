package org.kalypso.dwd.raster.test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import junit.framework.TestCase;

import org.kalypso.dwd.raster.ForecastGenerator;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.StreamUtilities;

/**
 * @author doemming
 */
public class Raster2ZMLTest extends TestCase
{
  public void testZMLGenerator() throws IOException
  {
    // prepare files in tmp dir
    final File modellFile = File.createTempFile( "modell", ".gml" );

    final File baseRasterFile = File.createTempFile( "baseraster", ".dwd" );
    baseRasterFile.deleteOnExit();
    final File srcRasterDir = FileUtilities.createNewTempDir( "eingangDWD" );
    srcRasterDir.deleteOnExit();
    final File destZMLDir = FileUtilities.createNewTempDir( "vorhersageDWD" );
    // copy examples from resources
    StreamUtilities.streamCopy( getClass().getResourceAsStream( "testraster/modell.gml" ),
        new FileOutputStream( modellFile ) );
    StreamUtilities.streamCopy( getClass().getResourceAsStream( "testraster/lm_inv_slug" ),
        new FileOutputStream( baseRasterFile ) );

    // put some raster files into the dir
    final File exampleFile = File.createTempFile( "vorhersage", ".dwd", srcRasterDir );
    StreamUtilities.streamCopy( getClass().getResourceAsStream( "testraster/lm_2004_11_09_00" ),
        new FileOutputStream( exampleFile ) );

    // start it
    final String[] arguments = new String[]
    {
        modellFile.getCanonicalPath(),
        baseRasterFile.getCanonicalPath(),
        srcRasterDir.getCanonicalPath(),
        destZMLDir.getCanonicalPath() };
    //    Raster2ZML.TEST_SCENARIO=true;

    final String[] a2 = new String[]
    {
        "c:\\\\TMP\\raster\\modell.gml",
        "c:\\\\TMP\\raster\\base",
        "c:\\\\TMP\\raster\\in",
        "c:\\\\TMP\\raster\\out",
    };
    ForecastGenerator.main( a2 );
  }
}