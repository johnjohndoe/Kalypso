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
    if( args.length != 4 )
    {
      System.out.println( "4 arguments needed: [modelfile] [baseRaster] [srcDir] [destDir])" );
      System.exit( 0 );
    }
    final File modelFile = new File( args[0] );
    final File baseRasterFile = new File( args[1] );
    final File srcDir = new File( args[2] );
    final File destDir = new File( args[3] );
  
    System.out.println("Konvertierung von Rasterdaten zu Zeitreihen");    
    
    // check existing...
    final File srcRaster = getNewestFileAndRemoveOthers( srcDir );
    System.out.println(" Raster: "+srcRaster.getName());
    try
    {
      process( modelFile, baseRasterFile, srcRaster, destDir );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private static boolean process( File modell, File baseRasterFile, File srcRaster, File destDir )
      throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    System.out.println( "loading model: " + modell.getName() );
    final URL modellURL = modell.toURL();
    final URL schemaURL = KalypsoNADefaultSchema.getInstance().getDefaultNaModellSchemaURL();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL, schemaURL );
    final Feature[] features = workspace.getFeatures( workspace.getFeatureType( "Catchment" ) );

    final Raster2ZML raster2ZML = new Raster2ZML( destDir );
    System.out.println( "loading forecast: " + srcRaster.getName() );
    final Reader r1 = new FileReader( srcRaster );
    final LineNumberReader r2 = new LineNumberReader( r1 );
    raster2ZML.loadRaster( r2 );
    r1.close();
    r2.close();

    System.out.println( "loading base: " + baseRasterFile.getName() );
    final Reader r3 = new FileReader( baseRasterFile );
    final LineNumberReader r4 = new LineNumberReader( r3 );
    raster2ZML.loadRaster( r4 );
    r3.close();
    r4.close();
    raster2ZML.createGeoRaster();
    return raster2ZML.createZML( features );
  }

  private static File getNewestFileAndRemoveOthers( File srcDir )
  {
    final File[] files = srcDir.listFiles();
    File result = null;
    long time = 0;
    // search newest...
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      long lastModified = file.lastModified();
      if( !file.isDirectory() && ( result == null || lastModified > time ) )
      {
        result = file;
        time = file.lastModified();
      }
    }
    // got it
    // remove others
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      if( !file.isDirectory() && ( file!=result))
      {
        System.out.println("remove "+file.getName());
        // TODO real remove it
      }
    } 
    return result;
  }
}