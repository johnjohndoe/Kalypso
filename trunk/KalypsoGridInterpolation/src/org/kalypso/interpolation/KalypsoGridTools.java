/*
 * Created on 05.01.2005
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation;

import java.io.EOFException;
import java.io.File;
import java.io.IOException;

import org.kalypso.interpolation.grid.Grid;
import org.kalypso.interpolation.grid.GridFactory;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypso.interpolation.mesh.MeshFactory;
import org.kalypso.interpolation.mesh.MeshReader;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window - Preferences - Java - Code Style - Code
 * Templates
 */
public class KalypsoGridTools implements IKalyposGridTools
{
  private static final MeshReader reader = new MeshReader();

  public KalypsoGridTools()
  {};

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.interpolation.IKalyposGridTools#interpolateGrid(org.deegree.model.feature.Feature[],
   *      org.deegree.model.feature.Feature[], java.lang.String, java.lang.String, java.lang.String, double,
   *      org.deegree.model.geometry.GM_Envelope, java.lang.String)
   */
  public void interpolateGrid( Feature[] meshElements, Feature[] nodes, String geometryPropertyElement,
      String geometryPropertyPoint, String valueProperty, double cellsize, GM_Envelope gridsize, GM_Surface wishbox,
      GM_Surface polyline, CS_CoordinateSystem crs, File out ) throws Exception
  {
    Mesh mesh = MeshFactory.getInstance().getMesh( crs );
    //read mesh
    reader.importMesh( mesh, nodes, geometryPropertyPoint, valueProperty, meshElements, geometryPropertyElement,
        wishbox );
    IGrid grid = GridFactory.getInstance().createGrid( wishbox.getEnvelope(), crs, cellsize, mesh );
    long st = System.currentTimeMillis();
    mesh.interpolateGrid( grid );
    grid.export( out );
    long dur = System.currentTimeMillis() - st;
    System.out.print( " in " + dur + " ms." );

  }

  public IGrid interpolateGrid( File[] inputFiles, CS_CoordinateSystem cs, GM_Envelope bbox, String borderPath,
      double size )
  {

    System.out.println( "\n" + "Importing input files..started" );
    long startTime = System.currentTimeMillis();
    //creates mesh out of input files and set name
    IGrid grid = null;
    try
    {
      MeshFactory mf = MeshFactory.getInstance();

      GM_Surface surfaceBBox = null;
      if( bbox != null )
        surfaceBBox = GeometryFactory.createGM_Surface( bbox, cs );
      Mesh mesh = mf.readMesh( inputFiles, cs, surfaceBBox, borderPath );

      long currentTime1 = System.currentTimeMillis();
      System.out.println( "\n" + "total importing duration (in seconds) : " + ( currentTime1 - startTime ) / 1000 );
      //create grid
      grid = GridFactory.getInstance().createGrid( bbox, cs, size, mesh );
      System.out.println( "\n" + "Interpolate Mesh: " + mesh.getMeshName() + " into Grid: " + grid.getGridID() );
      //interpolate grid
      mesh.interpolateGrid( grid );
      long currentTime2 = System.currentTimeMillis();
      System.out.println( "\n" + "total interpolation duration (in seconds) : " + ( currentTime2 - currentTime1 )
          / 1000 );
      System.out.println( "\n" + "total duration (in seconds) : " + ( currentTime2 - startTime ) / 1000 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return grid;
  }

  /**
   * @param grid1
   * 
   * @param grid2
   * 
   * @see org.kalypso.interpolation.IKalyposGridTools#subtract(org.kalypso.interpolation.grid.IGrid,
   *      org.kalypso.interpolation.grid.IGrid)
   */
  public IGrid subtract( IGrid grid1, IGrid grid2 )
  {
    if( grid1 == null && grid2 != null )
      return grid2;
    if( grid1 != null && grid2 == null )
      return grid1;
    if( grid1 == null && grid2 == null )
      return null;
    GM_Position llc1 = grid1.getOrigin();
    GM_Position llc2 = grid2.getOrigin();
    double nodata = Double.parseDouble( IGrid.DEFAULT_NO_DATA );
    double cell1 = grid1.getCellSize();
    double cell2 = grid2.getCellSize();
    int maxRows1 = grid1.getRows();
    int maxRows2 = grid2.getRows();
    int maxCols1 = grid1.getCols();
    int maxCols2 = grid2.getCols();
    GM_Envelope env1 = grid1.getExtend().getEnvelope();
    GM_Envelope env2 = grid2.getExtend().getEnvelope();
    GM_Position check = GeometryFactory.createGM_Position( llc2.getX() + cell2 / 2, llc2.getY() + cell2 / 2 );
    CS_CoordinateSystem cs1 = grid1.getCoordinateSystem();
    CS_CoordinateSystem cs2 = grid2.getCoordinateSystem();
    boolean coincides = grid1.isPointOnGrid( check, false );
    int row = 0;
    int col = 0;
    if( cs1.equals( cs2 ) && cell1 == cell2 && ( llc1.equals( llc2 ) || coincides ) )
    {
      GM_Envelope mergedEnv = env1.getMerged( env2 );
      IGrid grid = null;
      try
      {
        grid = GridFactory.getInstance().createGrid( mergedEnv, grid1.getCoordinateSystem(), cell1, null );
        int maxRows = grid.getRows();
        int maxCols = grid.getCols();
        for( row = 0; row < maxRows; row++ )
        {
          for( col = 0; col < maxCols; col++ )
          {
            if( row == 19 && col == 20 )
              System.out.print( "rowAndCols" );
            //            {
            //              ( (Grid)grid1 ).writeGridValue( row, col, 50000d );
            //
            //              System.out.println( ( (Grid)grid1 ).readGridValue( row, col ) );
            //              System.out.println( ( (Grid)grid1 ).getPosition( row, col ) );
            //
            //            }
            if( row > maxRows1 || row > maxRows2 || col > maxCols1 || col > maxCols2 )
              ( (Grid)grid ).writeGridValue( row, col, nodata );
            else
            {
              double value1 = ( (Grid)grid1 ).readGridValue( row, col );
              double value2 = ( (Grid)grid2 ).readGridValue( row, col );
              double value = value1 - value2;
              if( value1 != nodata && value2 != nodata )
                ( (Grid)grid ).writeGridValue( row, col, value );
              else
                ( (Grid)grid ).writeGridValue( row, col, nodata );
            }
          }
        }
        return grid;
      }
      catch( EOFException e )
      {
        System.out.println( "error in row: " + row + "\tcol: " + col );
        return null;
      }
      catch( IOException e )
      {
        e.printStackTrace();
        return null;
      }
      catch( Exception e )
      {
        e.printStackTrace();
        return null;
      }
    }

    return null;
  }

  /**
   * @see org.kalypso.interpolation.IKalyposGridTools#add(org.kalypso.interpolation.grid.IGrid,
   *      org.kalypso.interpolation.grid.IGrid)
   */
  public IGrid add( IGrid grid1, IGrid grid2 )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.interpolation.IKalyposGridTools#importGrid(File, CS_CoordinateSystem)
   */
  public IGrid importGrid( File file, CS_CoordinateSystem cs ) throws Exception
  {
    return GridFactory.getInstance().importESRIasc( file, cs );
  }

}