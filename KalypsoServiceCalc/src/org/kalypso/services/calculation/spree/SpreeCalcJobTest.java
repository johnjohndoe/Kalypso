package org.kalypso.services.calculation.spree;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.kalypso.java.io.FileUtilities;

/**
 * @author belger
 */
public class SpreeCalcJobTest extends TestCase
{
  public void testReadTS() throws Exception
  {
    final File resultDir = FileUtilities.createRandomTmpDir( "SpreeCalcJobTest" );

    final File dbfFile = new File( resultDir, "spree.dbf" );
    final File shpFile = new File( resultDir, "spree.shp" );
    final File shxFile = new File( resultDir, "spree.shx" );
    FileUtilities.makeFileFromStream( false, dbfFile, getClass().getResourceAsStream( "test/HW040427.dbf") );
    FileUtilities.makeFileFromStream( false, shpFile, getClass().getResourceAsStream( "test/HW040427.shp") );
    FileUtilities.makeFileFromStream( false, shxFile, getClass().getResourceAsStream( "test/HW040427.shx") );

    final String inputFilename = dbfFile.getAbsolutePath();
    final int pointindex = inputFilename.lastIndexOf( '.' );
    final String inputFilenameWOext = inputFilename.substring( 0, pointindex );
    
    final Map map = new HashMap();
    map.put( SpreeCalcJob.DATA_LABEL, "Prognose" );
    map.put( SpreeCalcJob.DATA_STARTDATESTRING, "27.4.2005 12:00" );
    
    new SpreeCalcJob().writeResultsToFolder( inputFilenameWOext, resultDir, map );
  }
}
