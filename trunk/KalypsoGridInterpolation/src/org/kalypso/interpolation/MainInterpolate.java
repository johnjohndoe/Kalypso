/*
 * Created on 14.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation;

import java.io.File;

import org.kalypso.interpolation.grid.GridFactory;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
 */
public class MainInterpolate
{

  // static String logPath = "//home//export//kuepfer//tmp//log.dat";
  public static final String LOG_PATH = "d://temp//";

  public static final String LOG_NAME = "kalypsoInterpolateFEM.log";

  // static String outputPath = "//home//export//kuepfer//tmp//grid.asc";
  public static final String OUTPUT_PATH = "d://temp//res_Interpolation//";

  public static final String INPUT_PATH = "d://Daten//DataForCK//";

  private static final String DATA = "-data";

  private static final String DATA_TYPE = "-type";

  private static final String GAJA_TYPE = "gaja";

  private static final String GML_TYPE = "gml";

  private static final String BBOX = "-bbox";

  private static final String CS = "-cs";

  private static final String SHP_TYPE = "shp";

  // -grid name_of_grid.asc
  private static final String GRID = "-grid";

  private static final String SIZE = "-size";

  // -border d:/temp/shapebase
  private static final String BORDER = "-border";

  public static void main( String[] args )
  {
    String cs = null;
    GM_Envelope bbox = null;
    File[] inputFiles = null;
    File gridFile = null;
    String borderPath = null;
    double size = 0;
    String type = null;
    int i = 0;
    while( i != args.length )
    {

      if( args[i].equalsIgnoreCase( CS ) )
      {
        String str = args[++i];
        if( CRSHelper.isKnownCRS( str ) == false )
        {
          System.out.println( "Das Koordinatensystem: " + args[i] + " wird nicht unterstützt" );
          System.exit( 0 );
        }
        i++;
      }
      else if( args[i].equalsIgnoreCase( BBOX ) )
      {
        try
        {
          bbox = GeometryFactory.createGM_Envelope( Double.parseDouble( args[++i] ), Double.parseDouble( args[++i] ), Double.parseDouble( args[++i] ), Double.parseDouble( args[++i] ) );
        }
        catch( NumberFormatException e )
        {
          System.out.println( "Format Fehler: bbox-werte müssen \"doubles\" sein" );
          System.exit( 0 );
        }
        i++;
      }
      else if( args[i].equalsIgnoreCase( DATA_TYPE ) )
      {
        type = args[++i];
        i++;
      }
      else if( args[i].equalsIgnoreCase( DATA ) )
      {
        if( type.equalsIgnoreCase( GML_TYPE ) )
        {
          File file = new File( args[++i] );

          if( file.exists() )
            inputFiles = new File[] { file };
          else
          {
            System.out.println( "Die Datei: " + file.toString() + " existiert nicht" );
            System.exit( 0 );
          }
          i++;
        }
        else if( type.equalsIgnoreCase( SHP_TYPE ) )
        {
          inputFiles = new File[2];
          for( int j = 0; j < 2; j++ )
          {
            File file = new File( args[++i] );
            if( file.exists() )
              inputFiles[j] = file;
            else
            {
              System.out.println( "Die Datei: " + file.toString() + " existiert nicht" );
              System.exit( 0 );
            }
          }
          String valueProperty = args[++i];
          if( valueProperty == null )
          {
            // avoid yellow thingies!
          }
          i++;

        }
        else if( type.equalsIgnoreCase( GAJA_TYPE ) )
        {
          inputFiles = new File[3];
          for( int j = 0; j < 3; j++ )
          {
            File file = new File( args[++i] );
            if( file.exists() )
              inputFiles[j] = file;
            else
            {
              System.out.println( "Die Datei: " + file.toString() + " existiert nicht" );
              System.exit( 0 );
            }
          }
          i++;
        }
        else
        {
          System.out.println( "Der Dateityp: " + type + " wird nicht untestützt" );
          System.exit( 0 );
        }
      }
      else if( args[i].equalsIgnoreCase( GRID ) )
      {
        gridFile = new File( args[++i] );
        i++;
      }
      else if( args[i].equalsIgnoreCase( SIZE ) )
      {
        try
        {
          size = Double.parseDouble( args[++i] );
        }
        catch( NumberFormatException e )
        {
          System.out.println( "Format Fehler: Rasterweite muss ein \"double\" sein" );
          System.exit( 0 );
        }
        i++;
      }
      else if( args[i].equalsIgnoreCase( BORDER ) )
      {
        borderPath = args[++i];
        i++;
      }
    }// while
    try
    {
      KalypsoGridTools tools = new KalypsoGridTools();
      // File file = new File( "D://TEMP//res_Kellinghusen//grid_khHQ5_4.asc" );
      // File newGridFile = new File( "d://temp//grid_subtract.asc" );
      // IGrid girdHQ5 = tools.importGrid( file, cs );
      // girdHQ5.export( newGridFile );

      IGrid gridHQ10 = tools.interpolateGrid( inputFiles, cs, bbox, borderPath, size );
      gridHQ10.export( gridFile );

      // IGrid grid = tools.subtract( gridHQ10, girdHQ5 );
      // grid.export( newGridFile );

    }
    catch( Exception e )
    {
      e.printStackTrace( System.out );
    }
    finally
    {
      GridFactory.getInstance().clearFactory();
      System.exit( 0 );
    }
  }// main
}// class MainInterpolate
