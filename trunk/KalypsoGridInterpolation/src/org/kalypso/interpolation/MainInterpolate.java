/*
 * Created on 14.12.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.kalypso.interpolation.grid.GridFactory;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypso.interpolation.mesh.MeshFactory;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class MainInterpolate
{

  //	static String logPath = "//home//export//kuepfer//tmp//log.dat";
  public static final String LOG_PATH = "d://temp//";

  public static final String LOG_NAME = "kalypsoInterpolateFEM.log";

  //static String outputPath = "//home//export//kuepfer//tmp//grid.asc";
  public static final String OUTPUT_PATH = "d://temp//res_Interpolation//";

  public static final String INPUT_PATH = "d://Daten//DataForCK//";

  private static final String DATA = "-data";

  private static final String DATA_TYPE = "-type";

  private static final String GAJA_TYPE = "gaja";

  private static final String GML_TYPE = "gml";

  private static final String BBOX = "-bbox";

  private static final String CS = "-cs";

  private static final String SHP_TYPE = "shp";

  //-grid name_of_grid.asc
  private static final String GRID = "-grid";

  private static final String SIZE = "-size";

  //-border d:/temp/shapebase
  private static final String BORDER = "-border";

  public static void main( String[] args )
  {
    CS_CoordinateSystem cs = null;
    GM_Envelope bbox = null;
    File[] inputFiles = null;
    File gridFile = null;
    String borderPath = null;
    double size = 0;
    String type = null;
    String valueProperty = null;
    int i = 0;
    while( i != args.length )
    {

      if( args[i].equalsIgnoreCase( CS ) )
      {
        String str = args[++i];
        cs = ConvenienceCSFactory.getInstance().getOGCCSByName( str );
        if( cs == null )
        {
          System.out.println( "Das Koordinatensystem: " + args[i]
              + " wird nicht unterstützt" );
          System.exit( 0 );
        }
        i++;
      }
      else if( args[i].equalsIgnoreCase( BBOX ) )
      {
        try
        {
          bbox = GeometryFactory.createGM_Envelope( Double
              .parseDouble( args[++i] ), Double.parseDouble( args[++i] ),
              Double.parseDouble( args[++i] ), Double.parseDouble( args[++i] ) );
        }
        catch( NumberFormatException e )
        {
          System.out
              .println( "Format Fehler: bbox-werte müssen \"doubles\" sein" );
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
            inputFiles = new File[]
            { file };
          else
          {
            System.out.println( "Die Datei: " + file.toString()
                + " existiert nicht" );
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
              System.out.println( "Die Datei: " + file.toString()
                  + " existiert nicht" );
              System.exit( 0 );
            }
          }
          valueProperty = args[++i];
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
              System.out.println( "Die Datei: " + file.toString()
                  + " existiert nicht" );
              System.exit( 0 );
            }
          }
          i++;
        }
        else
        {
          System.out.println( "Der Dateityp: " + type
              + " wird nicht untestützt" );
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
          System.out
              .println( "Format Fehler: Rasterweite muss ein \"double\" sein" );
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
      BufferedWriter logWriter = null;
      //Starting interpolation
      System.out.print( "\n" + "Starting logger..." );

      long startTime = System.currentTimeMillis();

      logWriter = new BufferedWriter( new FileWriter( new File( LOG_PATH
          + LOG_NAME ) ) );

      System.out.println( "finished" );

      SimpleDateFormat logformatter = new SimpleDateFormat(
          "dd.MM.yyyy G 'at' hh:mm:ss z" );
      Date today = new Date();
      String dateLogformat = logformatter.format( today );
      logWriter.write( "Start time: " + dateLogformat );

      System.out.println( "\n" + "StartTime : " + dateLogformat );

      System.out.println( "\n" + "Importing input files..started" );

      //creates mesh out of input files and set name

      MeshFactory mf = MeshFactory.getInstance();

      GM_Surface surfaceBBox = null;
      if( bbox != null )
        surfaceBBox = GeometryFactory.createGM_Surface( bbox, cs );
      Mesh mesh = mf.readMesh( inputFiles, cs, surfaceBBox, borderPath );

      System.out.println( "\n"
          + "total importing duration (in milliseconds) : "
          + ( System.currentTimeMillis() - startTime ) );

      System.out.println( "\n" + "Interpolate Mesh into Grid..started" );

      //gets bounding box of created mesh and interpolate it with
      // bounding box
      System.out.println( "\nMesh bbox: " + mesh.getEnvelope() );

      IGrid grid = GridFactory.getInstance().createGrid( bbox, cs, size, mesh );

      mesh.interpolateGrid( gridFile, mesh.getBorderLine(), grid );

      System.out.print( "\n..finished" );

      System.out.print( "\n" + "Closing Logger..." );

      today = new Date();
      dateLogformat = logformatter.format( today );
      logWriter.newLine();
      logWriter.write( "End time: " + dateLogformat );
      logWriter.close();
      long exDuration = System.currentTimeMillis() - startTime;

      System.out.print( "finished" );
      System.out.println( "\n" + "EndTime : " + dateLogformat );
      System.out.println( "\n" + "total running duration ( seconds ) : "
          + exDuration / 1000 );
      //JOptionPane.showMessageDialog(null,"Interpolation finished
      // successfully in " + exDuration/1000 + " Seconds.");

    }//try
    catch( Exception e )
    {
      e.printStackTrace( System.out );
    }
    finally
    {
      System.exit( 0 );
    }
  }//main
}//class MainInterpolate
