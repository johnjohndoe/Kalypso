package org.kalypso.dwd.raster;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.PrefixFileFilter;
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

    System.out.println( "Konvertierung von Rasterdaten zu Zeitreihen" );

    // check existing...
    final File srcRaster = getNewestFileAndRemoveOthers( srcDir );
    if( srcRaster != null )
    {
      System.out.println( " Raster: " + srcRaster.getName() );
      try
      {
        process( modelFile, baseRasterFile, srcRaster, destDir );
        srcRaster.delete();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    else
      System.out.println( " keine Rasterdatei gefunden" );

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
    final FileFilter filter = new PrefixFileFilter( "lm_" );
    final File[] files = srcDir.listFiles( filter );
    File result = null;
    Date date=null;
    // search newest...
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      if( file.isDirectory() )
        continue;
      Date testdate=getDateFromRaster(file);
      if(testdate==null)
        continue;
      if( result == null )
      {
        result = file;
        date=testdate;
      }
      else if( testdate.after(date))
      {
        result = file;
        date=testdate;
      }
    }
    if( result == null )
      return null;
    // got it
    // remove others
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      if( !file.isDirectory() && ( file != result ) )
      {
        System.out.println( "remove " + file.getName() );
        file.delete();
      }
    }
    return result;
  }

  private static SimpleDateFormat m_lmDateFormat=new SimpleDateFormat("'lm_'yyyy'_'MM'_'dd'_'hh");
//  lm_2004_11_10_00
  private static Date getDateFromRaster( File file )
  {
    String fileName=file.getName();
    try
    {
      return m_lmDateFormat.parse(fileName);
    }
    catch( ParseException e )
    {
      System.out.println(" file "+fileName+" is must be in format \"lm_yyyy_MM_dd_hh\"" );
      return null;
    }
  }
}