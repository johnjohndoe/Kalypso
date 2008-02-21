/*
 * Created on 15.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.io.File;
import java.util.Random;

import junit.framework.TestCase;

import org.kalypso.interpolation.KalypsoGridTools;
import org.kalypso.interpolation.mesh.Element;
import org.kalypso.interpolation.mesh.Point;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
 */
public class TestGridMethods extends TestCase
{
  public static String cs = "EPSG:31467";

  static String testFile = "d://temp//testNeu.asc";

  static String originalGrid = "d://temp//original.asc";

  static String copiedGrid = "d://temp//copy.asc";

  static String operation = "d://temp//operation.asc";

  static KalypsoGridTools tools = new KalypsoGridTools();

  public static Grid createGrid( ) throws Exception
  {
    Random generator = new Random();
    Grid grid = (Grid) GridFactory.getInstance().createGrid( GeometryFactory.createGM_Position( 0, 0 ), cs, 300, 200, 1, null );
    for( int r = 0; r < grid.getRows(); r++ )
    {
      for( int c = 0; c < grid.getCols(); c++ )
      {
        grid.writeGridValue( r, c, generator.nextDouble() * 5 / 3.8 );
      }
    }
    return grid;
  }

  public static IGrid testRandomAccessFile( ) throws Exception
  {
    Grid grid = createGrid();
    GM_Position pos1 = GeometryFactory.createGM_Position( 0.5, 0.5 );
    GM_Position pos2 = GeometryFactory.createGM_Position( 8.5, 8.5 );
    GM_Position pos3 = GeometryFactory.createGM_Position( 5.5, 7.5 );

    int row1 = grid.getRowIndex( pos1 );
    int col1 = grid.getColIndex( pos1 );
    int row2 = grid.getRowIndex( pos2 );
    int col2 = grid.getColIndex( pos2 );
    int row3 = grid.getRowIndex( pos3 );
    int col3 = grid.getColIndex( pos3 );

    grid.writeGridValue( pos1, 25.25 );
    grid.writeGridValue( pos2, 55.85 );
    grid.writeGridValue( pos3, 55.75 );

    GM_Position pos11 = grid.getPosition( row1, col1 );
    GM_Position pos12 = grid.getPosition( row2, col2 );
    GM_Position pos13 = grid.getPosition( row3, col3 );

    /* double v1 = */grid.readGridValue( pos1 );
    /* double v2 = */grid.readGridValue( pos2 );
    /* double v3 = */grid.readGridValue( pos3 );

    /* double v111 = */grid.readGridValue( pos11 );
    /* double v121 = */grid.readGridValue( pos12 );
    /* double v131 = */grid.readGridValue( pos13 );
    grid.export( new File( originalGrid ) );
    return grid;

  }// testRandomAccsessFile

  // TODO: this test depends on external data. Please never do such a thing
  // If you want to test something for yourself, please write a main() Method
  public static void testRandomGrid( ) throws Exception
  {
    fail( "This test depends on external data, so it fails!" );

    // IGrid grid1 = testRandomAccessFile();
    // grid1.export( new File( originalGrid ) );
    // IGrid grid2 = tools.importGrid( new File( originalGrid ), cs );
    // IGrid grid3 = tools.subtract( grid1, grid2 );
    // grid3.export( new File( operation ) );
    // GridFactory.getInstance().clearFactory();
  }

  // TODO: this test depends on external data. Please never do such a thing
  // If you want to test something for yourself, please write a main() Method
  public static void testArithmetics( ) throws Exception
  {
    fail( "This test depends on external data, so it fails!" );

    // IGrid grid1 = null;
    // IGrid grid2 = null;
    // IGrid grid3 = null;
    // grid1 = tools.importGrid( new File( testFile ), cs );
    // grid2 = tools.importGrid( new File( testFile ), cs );
    // grid3 = tools.subtract( grid1, grid2 );
    // grid3.export( new File( operation ) );
    // GridFactory.getInstance().clearFactory();
  }

  // TODO: this test depends on external data. Please never do such a thing
  // If you want to test something for yourself, please write a main() Method
  public static void testImportESRIasc( ) throws Exception
  {
    fail( "This test depends on external data, so it fails!" );
    // IGrid grid1 = tools.importGrid( new File( testFile ), cs );
    // grid1.export( new File( copiedGrid ) );
  }

  public static void testPosition( )
  {
    GM_Position point1 = GeometryFactory.createGM_Position( 10, 4 );
    GM_Position point2 = GeometryFactory.createGM_Position( 10, 4 );
    boolean test = point1.equals( point2 );
    System.out.println( test );
  }

  public static void testOnEdge( ) throws GM_Exception
  {
    GM_Position pointOn = GeometryFactory.createGM_Position( 10, 4 );
    GM_Position pointOff = GeometryFactory.createGM_Position( 14, 9 );

    GM_Position[] array = { GeometryFactory.createGM_Position( 3, 0, 0 ), GeometryFactory.createGM_Position( 20, 0, 0 ), GeometryFactory.createGM_Position( 15, 10, 0 ),
        GeometryFactory.createGM_Position( 3, 0, 0 ) };

    Element e = new Element( "1", null, array, null );
    boolean testOn = e.isPointOnEdge( new Point( "on", pointOn, null ) );
    boolean testOff = e.isPointOnEdge( new Point( "off", pointOff, null ) );
    System.out.println( "testOn: " + testOn + "\ttestOff: " + testOff );

  }

  public static void testContains( )
  {
    GM_Position point = GeometryFactory.createGM_Position( 5, 5 );

    GM_Position[] array = { GeometryFactory.createGM_Position( 3, 0, 0 ), GeometryFactory.createGM_Position( 20, 0, 0 ), GeometryFactory.createGM_Position( 15, 10, 0 ),
        GeometryFactory.createGM_Position( 3, 0, 0 ) };

    GM_Surface surface;
    try
    {
      surface = GeometryFactory.createGM_Surface( array, null, null, null );
      boolean test = surface.contains( point );
      System.out.println( test );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      fail( "fehler in contains" );
    }

  }// testContains

  public static void testPositionGrid( ) throws Exception
  {
    Grid grid = (Grid) testRandomAccessFile();
    GM_Position position = GeometryFactory.createGM_Position( 11.5, 10.5 );
    /* boolean onGridpos = */grid.isPointOnGrid( position, true );
    System.out.println( position );
    int testc = grid.getColIndex( position );
    int testr = grid.getRowIndex( position );
    System.out.println( "row: " + testr + "\tcol: " + testc );
    GM_Position test3 = grid.getPosition( testr, testc );
    System.out.println( test3 );
  }// testPositionGrid
}