package org.kalypso.dwd.raster;

import java.io.File;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.net.URL;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.namodel.schema.KalypsoNADefaultSchema;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;

/**
 * @author doemming
 */
public class ForecastGenerator
{

  public static void main( String[] args )
  {
    if( args.length < 2 )
    {
      System.out.println( "3 arguments needed: [modelfile] [baseRaster][srcDir] [destDir])" );
      System.exit( 0 );
    }
    File modelFile = new File( args[0] );
    File baseRasterFile = new File( args[1] );
    File srcDir = new File( args[2] );
    File destDir = new File( args[3] );
    // check existing...
    File srcRaster = getNewestFile( srcDir );
    try
    {
      process( modelFile, baseRasterFile, srcRaster, destDir );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private static void process( File modell, File baseRasterFile, File srcRaster, File destDir )
      throws Exception
  {

    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    final URL modellURL = modell.toURL();
    final URL schemaURL = KalypsoNADefaultSchema.getInstance().getDefaultNaModellSchemaURL();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL, schemaURL );
    final Feature[] features = workspace.getFeatures( workspace.getFeatureType( "Catchment" ) );

    final Raster2ZML raster2ZML = new Raster2ZML( destDir );
    final Reader r1 = new FileReader( srcRaster );
    //                    .getResourceAsStream("testraster/lm_2003_07_07_00.txt"));
    final LineNumberReader r2 = new LineNumberReader( r1 );
    final Reader r3 = new FileReader( baseRasterFile );
    final LineNumberReader r4 = new LineNumberReader( r3 );
    raster2ZML.loadRaster( r2 );
    raster2ZML.loadRaster( r4 );
    r1.close();
    r2.close();
    r3.close();
    r4.close();
    raster2ZML.createGeoRaster();
    raster2ZML.createZML( features );
  }

  private static File getNewestFile( File srcDir )
  {
    final File[] files = srcDir.listFiles();
    File result = null;
    long time = 0;
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      long lastModified = file.lastModified();
      if( !file.isDirectory() && (result == null || lastModified > time ))
      {
        result = file;
        time = file.lastModified();
      }
    }
    return result;
  }
}