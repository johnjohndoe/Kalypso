/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;
import java.util.Vector;

import javax.naming.OperationNotSupportedException;

import org.kalypso.interpolation.mesh.Point;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This Class represents a Grid, the row and column index starts at [0,0] at the lower left corner on the Grid. The cell
 * center of the grid cell at [0,0] has a offset of cellsize/2 for both x and y-cordinates
 * 
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window - Preferences - Java - Code Style - Code
 * Templates
 */
public class Grid implements IGrid
{

  private final int m_rows;

  private final int m_cols;

  static final String DEFAULT_FILE_NAME = "grid_" + String.valueOf( System.currentTimeMillis() ) + ".asc";

  private String m_name = null;

  // This variable holds the value for the cell center of the cell at the
  // lower left corner of the raster.
  private GM_Surface m_env = null;

  private GM_Surface m_wishbox = null;

  private final double m_cellsize;

  //private final double[][] gridValues;

  private final RandomAccessFile m_gridValues;

  private final String rafPath = "d://temp//array.raf";

  private static String nodata = "-9999";

  public static final String DEFAULT_SUFFIX = "asc";

  public static final int NOT_ON_GRID = -1;

  private final String m_id;

  /**
   * <B>public Grid(Point refPoint, Integer r, Integer c, Double cellsize) </B>
   * <P>
   * Constructor
   * 
   * @param refPoint
   *          Point lowerleft corner as a reference
   * @param r
   *          Integer number of rows in grid
   * @param c
   *          Integer number of columns in grid
   * @param cellsize
   *          Double cellsize in grid
   * @throws FileNotFoundException
   */
  protected Grid( String id, String gridName, GM_Position llc, CS_CoordinateSystem crs, int r, int c, double cellsize,
      GM_Envelope wishbox ) throws Exception
  {
    File file = new File( rafPath );
    if( file.exists() )
      file.delete();
    m_gridValues = new RandomAccessFile( rafPath, "rw" );
    GM_Envelope env = GeometryFactory.createGM_Envelope( llc.getX(), llc.getY(), llc.getX() + cellsize * c, llc.getY()
        + cellsize * r );
    m_env = GeometryFactory.createGM_Surface( env, crs );
    m_rows = r;
    m_cols = c;
    m_cellsize = cellsize;
    m_name = gridName;
    m_id = id;
    m_wishbox = GeometryFactory.createGM_Surface( wishbox, crs );
    initGrid();
  }//constructor

  public GM_Envelope getEnvelope()
  {
    return m_env.getEnvelope();
  }

  /**
   * This constructs a Grid from a GM_Envelope. If the cellsize and grid size match the grid is created if they do not
   * match a grid with rows = 0 and cols = 0 is generated.
   * 
   * @param gridSize
   *          An Envelope giving the width and height of the grid to be created.
   * @param cellsize
   *          The cell size of the grid cells (dx=dy).
   * @throws FileNotFoundException
   */
  protected Grid( String id, String gridName, GM_Envelope gridSize, CS_CoordinateSystem crs, double cellsize,
      GM_Envelope wishbox ) throws Exception
  {
    m_cols = ( (int)Math.ceil( gridSize.getWidth() / cellsize ) + 2 );
    m_rows = ( (int)Math.ceil( gridSize.getHeight() / cellsize ) + 2 );
    //create a grid for output with an additional row and column
    GM_Position llcGrid = GeometryFactory.createGM_Position( gridSize.getMin().getX() - cellsize, gridSize.getMin()
        .getY()
        - cellsize );
    GM_Position urcGrid = GeometryFactory.createGM_Position( llcGrid.getX() + m_cols * cellsize, llcGrid.getY()
        + m_rows * cellsize );

    GM_Envelope gridenv = GeometryFactory.createGM_Envelope( llcGrid, urcGrid );
    //    double width = ( gridSize.getWidth() - cellsize );
    //    if( Math.IEEEremainder( width, cellsize ) < 1E-10 )
    //    {
    //      m_cols = (int)Math.round( width / cellsize );
    //    }
    //    else
    //      m_cols = 0;
    //    double hight = ( gridSize.getHeight() - cellsize );
    //    if( Math.IEEEremainder( hight, cellsize ) < 1E-10 )
    //    {
    //      m_rows = (int)Math.round( hight / cellsize );
    //    }
    //    else
    //      m_rows = 0;
    //    if( getRows() == 0 || getCols() == 0 )
    //      throw new Exception( "Cellsize ( " + cellsize
    //          + ") does not match gridsize (w: " + width + " h: " + hight + " )! " );
    m_env = GeometryFactory.createGM_Surface( gridSize, crs );
    File file = new File( rafPath );
    if( file.exists() )
      file.delete();
    m_gridValues = new RandomAccessFile( rafPath, "rw" );
    m_cellsize = cellsize;
    m_name = gridName;
    m_id = id;
    m_wishbox = GeometryFactory.createGM_Surface( wishbox, crs );
    initGrid();
  }

  /**
   * <B>public void initGrid() </B>
   * <P>
   * Initialize the grid with nodata
   * 
   * @throws IOException
   *  
   */
  private long initGrid() throws IOException
  {
    m_gridValues.setLength( getRows() * getCols() * 8 + 8 );
    //    System.out.println( "Initialized grid: " + m_env.getEnvelope()
    //        + "\tcell size: " + m_cellsize );
    System.out.println( "Starting grid initialization.." );
    long index = 0;
    while( index < m_gridValues.length() )
    {
      m_gridValues.writeDouble( Double.parseDouble( nodata ) );
      index = index + 8;
    }//while
    System.out.print( "..finished" );
    m_gridValues.seek( 0 );
    return m_gridValues.length();
  } //initgrid

  public String getGridName()
  {
    return m_name;
  }

  private long getPosInFile( GM_Position pos )
  {
    if( pos != null )
    {
      int col = getColIndex( pos );
      int row = getRowIndex( pos );
      return getPosInFile( row, col );
    }
    return -1;

  }//getPosInFile

  private long getPosInFile( int row, int col )
  {
    return (long)( row * ( getCols() - 1 ) * 8 + col * 8 );
    //    return Long.parseLong( String.valueOf( row * ( getCols() - 1 ) * 8 + col * 8 ) );

  }

  public double readGridValue( GM_Position pos ) throws Exception
  {
    if( isPointOnGrid( pos ) )
    {
      return readGridValue( getPosInFile( pos ) );
    }
    return -1;
  }

  public double readGridValue( int row, int col ) throws IOException
  {

    m_gridValues.seek( getPosInFile( row, col ) );
    return m_gridValues.readDouble();
  }

  private double readGridValue( long pos ) throws Exception
  {
    if( pos > -1 )
    {
      m_gridValues.seek( pos );
      return m_gridValues.readDouble();
    }
    return -1;
  }

  /**
   * <B>public int getColIndex(GM_Position pos) </B>
   * <P>
   * This Method returns the column index of the GM_Position pos in the grid.
   * 
   * @param pos
   * @return Method returns -1 when pos is not on the grid, else the column index is returned.
   */

  private int getColIndex( GM_Position pos )
  {
    if( !isPointOnGrid( pos ) || pos.getY() >= getEnvelope().getMax().getY() )
      return NOT_ON_GRID;
    else
    {
      double c = ( pos.getX() - getEnvelope().getMin().getX() - m_cellsize / 2 ) / m_cellsize;
      return (int)Math.round( c );
    }//else
  }//getColIndex

  /**
   * <B>public int getRowIndex(GM_Position pos) </B>
   * <P>
   * This Method returns the row index of the GM_Position pos in the grid.
   * 
   * @param pos
   * @return Method returns -1 when pos lays not on the grid, else the row index is returned.
   */
  private int getRowIndex( GM_Position pos )
  {
    if( !isPointOnGrid( pos ) || pos.getX() <= getEnvelope().getMin().getX() )
      return NOT_ON_GRID;
    else
    {
      //      double r = ( pos.getY() - env.getMin().getY() - cellsize / 2 ) /
      // cellsize;
      double r = ( getEnvelope().getMax().getY() - pos.getY() - m_cellsize / 2 ) / m_cellsize;
      return (int)Math.round( r );
    }//else
  }//getRowIndex

  /**
   * <B>public Integer getCols() </B>
   * <P>
   * Returns number of columns in grid
   * 
   * @return int number of columns in grid
   */
  public int getCols()
  {
    return m_cols;
  }

  /**
   * <B>public Integer getRows() </B>
   * <P>
   * Returns number of rows in grid
   * 
   * @return int number of rows in grid
   */
  public int getRows()
  {
    return m_rows;
  }

  /**
   * <B>public double getGridSize() </B>
   * <P>
   * Returns cellsize of the grid
   * 
   * @return double cellsize of the grid
   */
  public double getGridSize()
  {
    return m_cellsize;
  }

  /**
   * <B>public void setNodata(String str) </B>
   * <P>
   * Sets the string which should represent nodata
   * 
   * @param str
   *          String string which should represent nodata
   */
  public void setNodata( String str )
  {
    nodata = str;
  }

  /**
   * <B>public Point getCoordinateOfCell(int row, int col) </B>
   * <P>
   * Returns grid coordinates from given row,column of a cell of matrix
   * 
   * @param row
   *          int row value of matrix
   * @param col
   *          int column value of matrix
   * @return Point point of given coordinates, if the Point lays not on the grid null is returned.
   * @see Point
   */
  public GM_Position getPosition( int row, int col )
  {
    double xcor = ( getEnvelope().getMin().getX() + m_cellsize / 2 + ( col * m_cellsize ) );
    //    double ycor = ( env.getMin().getY() + cellsize / 2 + ( row * cellsize )
    // );
    double ycor = ( getEnvelope().getMax().getY() - m_cellsize / 2 - ( row * m_cellsize ) );
    GM_Position p = GeometryFactory.createGM_Position( xcor, ycor );
    if( isPointOnGrid( p ) )
      return p;
    else
      return null;
  }//getCoordinateOfCell

  /**
   * <B>public void writeGridValue(Point centerOfCell, double value) </B>
   * <P>
   * sets the given elevation to the grid on given cell
   * 
   * @param centerOfCell
   *          Point point to set in grid
   * @param value
   *          double elevation value used instead of cells attribute value
   * @throws Exception
   *           if parameter pos lays not on the grid.
   */

  public void writeGridValue( GM_Position centerOfCell, double value ) throws Exception
  {
    if( !isPointOnGrid( centerOfCell ) )
      throw new Exception( "GM_Position is not on the Grid!" );
    else
    {//gets row and col value for grid
      //set given elevation value at particular row and col in grid
      m_gridValues.seek( getPosInFile( centerOfCell ) );
      m_gridValues.writeDouble( value );
      //System.out.println("value: " + value);

    }

  }//setGridValue

  /**
   * This Method gets all neighbouring cells of a praticular grid cell.
   * 
   * @param p
   *          The cell to get neighbours for.
   * @return Vector containing GM_Position of all heigbouring cells.
   */
  private Vector getNeighborCells( GM_Position p )
  {
    if( isPointOnGrid( p ) )
    {
      return getNeighborCellsOnGrid( p );
    }//if
    else
    {
      //      double dy = ( p.getY() - env.getMin().getY() ) - cellsize / 2;
      //      double dx = ( p.getX() - env.getMax().getX() ) - cellsize / 2;
      double dx = ( p.getX() - getEnvelope().getMin().getX() ) - m_cellsize / 2;
      double dy = ( getEnvelope().getMax().getY() - p.getY() ) - m_cellsize / 2;
      // -0.01 to assure if dx is exactly XX.5 that the lower c resp. r number
      // is taken
      double c = Math.round( ( dx ) / m_cellsize );
      double r = Math.round( ( dy ) / m_cellsize );
      return getNeighborCellsOnGrid( getPosition( (int)r, (int)c ) );
    }
  }

  /**
   * This method checks if the postition p is on the grid.
   * 
   * @param p
   *          the position of the point to be checked
   * @return true if the p is on the grid, false if not.
   */
  public boolean isPointOnGrid( GM_Position p )
  {
    if( p == null )
      return false;
    //    double dx = ( p.getX() - env.getMin().getX() - cellsize / 2 );
    //    double dy = ( p.getY() - env.getMin().getY() - cellsize / 2 );
    double px = p.getX();
    double py = p.getY();
    double dx = ( px - getEnvelope().getMin().getX() - m_cellsize / 2 );
    double dy = ( getEnvelope().getMax().getY() - py - m_cellsize / 2 );
    double testx = Math.abs( Math.IEEEremainder( dx, m_cellsize ) );
    double testy = Math.abs( Math.IEEEremainder( dy, m_cellsize ) );
    if( testx < 0.000001 && testy < 0.000001 && px >= getEnvelope().getMin().getX()
        && py <= getEnvelope().getMax().getY() && px <= getEnvelope().getMax().getX()
        && py >= getEnvelope().getMin().getY() )
      return true;
    return false;

  }//setGridValue

  /**
   * This Method returns its neighboring cells from a position on the grid. It returns also itself
   * 
   * @param p
   *          Position on grid to find neighboring cells(p must lay on grid!!).
   * @return res A Vector containing all neighboring cells and itself.
   */
  private Vector getNeighborCellsOnGrid( GM_Position p )
  {
    Vector res = new Vector();
    int row = getRowIndex( p );
    int col = getColIndex( p );
    if( row == -1 || col == -1 )
      return res;
    //no boundary
    if( row != getRows() && row != 0 && col != getCols() && col != 0 )
    {
      res.add( getPosition( row - 1, col - 1 ) );
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row - 1, col + 1 ) );
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row + 1, col + 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row + 1, col - 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if upper left corner
    else if( row == 0 && col == 0 )
    {
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row + 1, col + 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if lower right corner
    else if( row == getRows() && col == getCols() )
    {
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row - 1, col - 1 ) );
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if upper right corner
    else if( row == 0 && col == getCols() )
    {
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row + 1, col - 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if lower left corner
    else if( row == getRows() && col == 0 )
    {
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row - 1, col + 1 ) );
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if upper boundary
    else if( row == 0 && col >= 0 && col <= getCols() )
    {
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row + 1, col + 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row + 1, col - 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if lower boundary
    else if( row == getRows() && col >= 0 && col <= getCols() )
    {
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row - 1, col + 1 ) );
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row - 1, col - 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if left boundary
    else if( col == 0 && row >= 0 && row <= getRows() )
    {
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row - 1, col + 1 ) );
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row + 1, col + 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    //checks if right boundary
    else if( col == getCols() && row >= 0 && row <= getRows() )
    {
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row - 1, col - 1 ) );
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row + 1, col - 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    return null;
  }//getNeighborCellsOnGrid

  /**
   * This Method finds the row and col index of a given envelope.
   * <P>[ cmin rmin ] (llc)
   * <P>[ cmax rmax ] (urc)
   * <P>
   * 
   * @param envelope
   * @return res A 2x2 array with the min row/col and max row/col indecies on the grid that are NOT in the parameter
   *         envelope and the most upper right resp. most lower left corner cell
   */

  private int[][] getRowColIndexFromEnv( GM_Envelope envelope )
  {
    int[][] res = new int[2][2];
    GM_Position llc = envelope.getMin();
    GM_Position urc = envelope.getMax();
    int elementIndex = -1;
    Vector cells = getNeighborCells( llc );
    double minDistance = 0.0;
    for( int i = 0; i < cells.size(); i++ )
    {
      GM_Position pos = (GM_Position)cells.elementAt( i );
      double distance = pos.getDistance( llc );
      if( i == 0 )
        minDistance = distance;
      if( distance <= minDistance && llc.getX() >= pos.getX() && llc.getY() >= pos.getY() )
      {
        minDistance = distance;
        elementIndex = i;
      }
    }//for
    llc = (GM_Position)cells.elementAt( elementIndex );

    elementIndex = -1;
    cells = getNeighborCells( urc );
    minDistance = 0.0;
    for( int i = 0; i < cells.size(); i++ )
    {
      GM_Position pos = (GM_Position)cells.elementAt( i );
      double distance = pos.getDistance( urc );
      if( i == 0 )
        minDistance = distance;
      if( distance <= minDistance && urc.getX() <= pos.getX() && urc.getY() <= pos.getY() )
      {
        minDistance = distance;
        elementIndex = i;
      }
    }
    urc = (GM_Position)cells.elementAt( elementIndex );

    res[0][0] = getColIndex( llc );
    res[0][1] = getRowIndex( llc );
    res[1][0] = getColIndex( urc );
    res[1][1] = getRowIndex( urc );

    return res;
  }

  /**
   * This Method returns all the Cells in the given envelope
   * 
   * @param envelope
   * @return Vector with x,y coordinates (GM_Position) of the cells in the envelope
   * @throws GM_Exception
   */
  public Vector getCellsFromGrid( GM_Envelope envelope, CS_CoordinateSystem cs ) throws GM_Exception
  {
    GM_Object intersection = getExtend().intersection( GeometryFactory.createGM_Surface( envelope, cs ) );
    Vector cells = new Vector();
    int[][] range = getRowColIndexFromEnv( ( (GM_Surface)intersection ).getEnvelope() );
    GM_Position[] pos;
    for( int r = range[1][1]; r < range[0][1]; r++ )
      for( int c = range[0][0]; c < range[1][0]; c++ )
        cells.addElement( getPosition( r, c ) );
    return cells;

  }//getCellsFromGrid

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.interpolation.grid.IGrid#getID()
   */
  public String getGridID()
  {
    return m_id;
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getExtend()
   */
  public GM_Surface getExtend()
  {
    return m_env;
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#merge(org.kalypso.interpolation.grid.IGrid,
   *      org.kalypso.interpolation.grid.IGrid)
   */
  public IGrid merge( IGrid grid1, IGrid grid2 )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#tile(org.kalypso.interpolation.grid.IGrid,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public IGrid tile( IGrid gird, GM_Envelope tilesize ) throws OperationNotSupportedException
  {
    throw new OperationNotSupportedException( "The operation to tile a grid is not implemented yet." );
  }

  /**
   * @throws GM_Exception
   * @see org.kalypso.interpolation.grid.IGrid#getGridCells()
   */
  public GM_Position[] getGridCells() throws GM_Exception
  {
    List list = getCellsFromGrid( m_env.getEnvelope(), m_env.getCoordinateSystem() );
    return (GM_Position[])list.toArray( new GM_Position[list.size()] );
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getOrigin()
   */
  public GM_Position getOrigin()
  {
    return m_env.getEnvelope().getMin();
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getCellSize()
   */
  public double getCellSize()
  {
    return m_cellsize;
  }

  public CS_CoordinateSystem getCoordinateSystem()
  {
    return m_env.getCoordinateSystem();
  }

  public void export( GM_Surface border, File out ) throws Exception
  {
    if( out == null )
      out = File.createTempFile( m_id, Grid.DEFAULT_SUFFIX );
    int[][] newRange = null;
    GM_Object intersection = null;
    BufferedWriter bw = new BufferedWriter( new FileWriter( out ) );
    System.out.print( "\n" + "Exporting Grid..." );
    try
    {
      if( border != null )
      {
        GM_Object intersection1 = getExtend().intersection( border );
        intersection = intersection1.intersection( m_wishbox );
        if( intersection == null )
          throw new Exception( "The boundary (polyline) does not intersect the requested bounding box.\n"
              + "Interpolation not successful!!" );
      }
      else if( m_wishbox != null && border == null )
      {
        intersection = getExtend().intersection( m_wishbox );
      }
      else
        intersection = getExtend();
      bw.write( "ncols " );
      bw.write( String.valueOf( m_cols ) );
      bw.write( "\nnrows " );
      bw.write( String.valueOf( m_rows ) );
      bw.write( "\nxllcorner " );
      bw.write( String.valueOf( m_env.getEnvelope().getMin().getX() ) );
      bw.write( "\nyllcorner " );
      bw.write( String.valueOf( m_env.getEnvelope().getMin().getY() ) );
      bw.write( "\ncellsize " );
      bw.write( String.valueOf( m_cellsize ) );
      bw.write( "\nnodata_value " );
      bw.write( nodata );
      bw.write( "\n" );

      for( int row = 0; row < m_rows; row++ )
      {
        for( int col = 0; col < m_cols; col++ )
        {
          double value = readGridValue( row, col );
          if( intersection.contains( getPosition( row, col ) ) )
            bw.write( String.valueOf( value ) );
          else
            bw.write( nodata );
          bw.write( ' ' );
        }//for j
      }//for i
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      bw.close();
      m_gridValues.close();
      File file = new File( rafPath );
      file.delete();
    }

  }

  public void setGridName( String name )
  {
    m_name = name;
  }
}