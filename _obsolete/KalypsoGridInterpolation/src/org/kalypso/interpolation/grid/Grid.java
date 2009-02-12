/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.List;
import java.util.Vector;

import javax.naming.OperationNotSupportedException;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This Class represents a Grid, the row and column index starts at [0,0] at the lower left corner on the Grid. The cell
 * center of the grid cell at [0,0] has a offset of cellsize/2 for both x and y-cordinates
 * 
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
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

  // private final double[][] gridValues;

  private final RandomAccessFile m_gridValues;

  private final File rafPath;// = "d://temp//array.raf";

  private static String m_nodata = "-9999";

  public static final String DEFAULT_SUFFIX = "asc";

  public static final int NOT_ON_GRID = -1;

  private final String m_id;

  private GM_Surface m_borderLine;

  /**
   * <B>public Grid(Point refPoint, Integer r, Integer c, Double cellsize) </B>
   * <P>
   * Constructor
   * 
   * @param llc
   *            Point lowerleft corner as a reference
   * @param r
   *            Integer number of rows in grid
   * @param c
   *            Integer number of columns in grid
   * @param cellsize
   *            Double cellsize in grid
   * @throws FileNotFoundException
   */
  protected Grid( String id, String gridName, GM_Position llc, String crs, int r, int c, double cellsize, GM_Envelope wishbox, GM_Surface borderLine ) throws Exception
  {
    m_borderLine = borderLine;
    rafPath = File.createTempFile( "grid", "raf" );
    if( rafPath.exists() )
      rafPath.delete();
    m_gridValues = new RandomAccessFile( rafPath, "rw" );
    GM_Envelope env = GeometryFactory.createGM_Envelope( llc.getX(), llc.getY(), llc.getX() + cellsize * c, llc.getY() + cellsize * r, crs );
    m_env = GeometryFactory.createGM_Surface( env, crs );
    m_rows = r;
    m_cols = c;
    m_cellsize = cellsize;
    m_name = gridName;
    m_id = id;
    if( wishbox != null )
      m_wishbox = GeometryFactory.createGM_Surface( wishbox, crs );
    else
      m_wishbox = m_env;
    initGrid();
  }// constructor

  public GM_Envelope getEnvelope( )
  {
    return m_env.getEnvelope();
  }

  /**
   * This constructs a Grid from a GM_Envelope. If the cellsize and grid size match the grid is created if they do not
   * match a grid with rows = 0 and cols = 0 is generated.
   * 
   * @param gridSize
   *            An Envelope giving the width and height of the grid to be created.
   * @param cellsize
   *            The cell size of the grid cells (dx=dy).
   * @throws FileNotFoundException
   */
  protected Grid( String id, String gridName, GM_Envelope gridSize, String crs, double cellsize, GM_Envelope wishbox, GM_Surface borderline ) throws Exception
  {
    m_borderLine = borderline;
    rafPath = File.createTempFile( "grid", "raf" );
    m_cols = ((int) Math.ceil( gridSize.getWidth() / cellsize ) + 2);
    m_rows = ((int) Math.ceil( gridSize.getHeight() / cellsize ) + 2);
    // create a grid for output with an additional row and column
    GM_Position llcGrid = GeometryFactory.createGM_Position( gridSize.getMin().getX() - cellsize, gridSize.getMin().getY() - cellsize );
    GM_Position urcGrid = GeometryFactory.createGM_Position( llcGrid.getX() + m_cols * cellsize, llcGrid.getY() + m_rows * cellsize );

    /* GM_Envelope gridenv = */GeometryFactory.createGM_Envelope( llcGrid, urcGrid, crs );
    m_env = GeometryFactory.createGM_Surface( gridSize, crs );
    if( rafPath.exists() )
      rafPath.delete();
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
   */
  private void initGrid( ) throws IOException
  {
    m_gridValues.setLength( m_rows * m_cols * 8 + 8 );
    System.out.print( " ..initialization of grid: " + m_id + " .." );
    long index = 0;
    while( index < m_gridValues.length() )
    {
      m_gridValues.writeDouble( Double.parseDouble( m_nodata ) );
      index = index + 8;
    }// while
    System.out.print( "..finished\n" );
    m_gridValues.seek( 0 );
  } // initgrid

  public String getGridName( )
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
    return -1l;

  }// getPosInFile

  private long getPosInFile( int row, int col )
  {
    return (row * (m_cols - 1) * 8 + col * 8);
  }

  public double readGridValue( GM_Position pos ) throws Exception
  {
    if( isPointOnGrid( pos, true ) )
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

  protected int getColIndex( GM_Position pos )
  {
    if( !isPointOnGrid( pos, true ) || pos.getY() >= getEnvelope().getMax().getY() )
      return NOT_ON_GRID;

    double c = (pos.getX() - getEnvelope().getMin().getX() - m_cellsize / 2) / m_cellsize;
    return (int) Math.round( c );
  }// getColIndex

  /**
   * <B>public int getRowIndex(GM_Position pos) </B>
   * <P>
   * This Method returns the row index of the GM_Position pos in the grid.
   * 
   * @param pos
   * @return Method returns -1 when pos lays not on the grid, else the row index is returned.
   */
  protected int getRowIndex( GM_Position pos )
  {
    if( !isPointOnGrid( pos, true ) )// || pos.getX() <= getEnvelope().getMin().getX() )
      return NOT_ON_GRID;

    // double r = ( pos.getY() - env.getMin().getY() - cellsize / 2 ) /
    // cellsize;
    double r = (getEnvelope().getMax().getY() - pos.getY() - m_cellsize / 2) / m_cellsize;
    return (int) Math.round( r );
  }// getRowIndex

  /**
   * <B>public Integer getCols() </B>
   * <P>
   * Returns number of columns in grid
   * 
   * @return int number of columns in grid
   */
  public int getCols( )
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
  public int getRows( )
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
  public double getGridSize( )
  {
    return m_cellsize;
  }

  /**
   * <B>public void setNodata(String str) </B>
   * <P>
   * Sets the string which should represent nodata
   * 
   * @param str
   *            String string which should represent nodata
   */
  protected void setNodata( String str )
  {
    m_nodata = str;
  }

  /**
   * <B>public Point getCoordinateOfCell(int row, int col) </B>
   * <P>
   * Returns grid coordinates from given row,column of a cell of matrix
   * 
   * @param row
   *            int row value of matrix
   * @param col
   *            int column value of matrix
   * @return Point point of given coordinates, if the Point lays not on the grid null is returned.
   * @see Point
   */
  public GM_Position getPosition( int row, int col )
  {
    double xcor = (getEnvelope().getMin().getX() + m_cellsize / 2 + (col * m_cellsize));
    double ycor = (getEnvelope().getMax().getY() - m_cellsize / 2 - (row * m_cellsize));
    GM_Position p = GeometryFactory.createGM_Position( xcor, ycor );
    if( isPointOnGrid( p, false ) )
      return p;

    return null;
  }// getCoordinateOfCell

  /**
   * <B>public void writeGridValue(Point centerOfCell, double value) </B>
   * <P>
   * sets the given elevation to the grid on given cell
   * 
   * @param centerOfCell
   *            Point point to set in grid
   * @param value
   *            double elevation value used instead of cells attribute value
   * @throws Exception
   *             if parameter pos lays not on the grid.
   */

  public void writeGridValue( GM_Position centerOfCell, double value ) throws Exception
  {
    if( !isPointOnGrid( centerOfCell, true ) )
      throw new Exception( "GM_Position is not on the Grid!" );

    // gets row and col value for grid
    // set given elevation value at particular row and col in grid
    m_gridValues.seek( getPosInFile( centerOfCell ) );
    m_gridValues.writeDouble( value );
    // System.out.println("value: " + value);

  }// setGridValue

  public void writeGridValue( int row, int col, double value ) throws Exception
  {
    if( (row >= 0 || row <= m_rows) && (col >= 0 || col <= m_cols) )
    {
      // gets row and col value for grid
      // set given elevation value at particular row and col in grid
      m_gridValues.seek( getPosInFile( row, col ) );
      m_gridValues.writeDouble( value );
    }
    else
      throw new Exception( "GM_Position is not on the Grid. Value cannot be written to grid!" );
  }// setGridValue

  /**
   * This Method gets all neighbouring cells of a praticular grid cell.
   * 
   * @param p
   *            The cell to get neighbours for.
   * @return Vector containing GM_Position of all heigbouring cells.
   */
  private Vector getNeighborCells( GM_Position p )
  {
    if( isPointOnGrid( p, true ) )
    {
      return getNeighborCellsOnGrid( p );
    }// if

    // double dy = ( p.getY() - env.getMin().getY() ) - cellsize / 2;
    // double dx = ( p.getX() - env.getMax().getX() ) - cellsize / 2;
    double dx = (p.getX() - getEnvelope().getMin().getX()) - m_cellsize / 2;
    double dy = (getEnvelope().getMax().getY() - p.getY()) - m_cellsize / 2;
    // -0.01 to assure if dx is exactly XX.5 that the lower c resp. r number
    // is taken
    double c = Math.round( (dx) / m_cellsize );
    double r = Math.round( (dy) / m_cellsize );
    return getNeighborCellsOnGrid( getPosition( (int) r, (int) c ) );
  }

  /**
   * This method checks if the postition p is on the grid.
   * 
   * @param p
   *            the position of the point to be checked
   * @param inEnvelope
   *            additional restraint, not just on the grid but also within the envelope of the grid if the value is true
   *            false the point only needs to match the position and cell width.
   * @return true if the p is on the grid, false if not.
   */
  public boolean isPointOnGrid( GM_Position p, boolean inEnvelope )
  {
    if( p == null )
      return false;
    double px = p.getX();
    double py = p.getY();
    double llcx = getEnvelope().getMin().getX();
    double llcy = getEnvelope().getMin().getY();
    double urcx = getEnvelope().getMax().getX();
    double urcy = getEnvelope().getMax().getY();
    double dx = (px - llcx - m_cellsize / 2);
    double dy = (urcy - py - m_cellsize / 2);
    double testx = Math.abs( Math.IEEEremainder( dx, m_cellsize ) );
    double testy = Math.abs( Math.IEEEremainder( dy, m_cellsize ) );
    boolean first = testx < 1e-4 && testy < 1e-4;
    boolean second = inEnvelope && px >= llcx && py <= urcy && px <= urcx && py >= llcy;
    if( first && (second || !inEnvelope) )
      return true;
    return false;

  }// setGridValue

  /**
   * This Method returns its neighboring cells from a position on the grid. It returns also itself
   * 
   * @param p
   *            Position on grid to find neighboring cells(p must lay on grid!!).
   * @return res A Vector containing all neighboring cells and itself.
   */
  private Vector getNeighborCellsOnGrid( GM_Position p )
  {
    final Vector<GM_Position> res = new Vector<GM_Position>();
    int row = getRowIndex( p );
    int col = getColIndex( p );
    if( row == -1 || col == -1 )
      return res;
    // no boundary
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
    // checks if upper left corner
    else if( row == 0 && col == 0 )
    {
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row + 1, col + 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    // checks if lower right corner
    else if( row == getRows() && col == getCols() )
    {
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row - 1, col - 1 ) );
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    // checks if upper right corner
    else if( row == 0 && col == getCols() )
    {
      res.add( getPosition( row, col - 1 ) );
      res.add( getPosition( row + 1, col - 1 ) );
      res.add( getPosition( row + 1, col ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    // checks if lower left corner
    else if( row == getRows() && col == 0 )
    {
      res.add( getPosition( row - 1, col ) );
      res.add( getPosition( row - 1, col + 1 ) );
      res.add( getPosition( row, col + 1 ) );
      res.add( getPosition( row, col ) );
      return res;
    }
    // checks if upper boundary
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
    // checks if lower boundary
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
    // checks if left boundary
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
    // checks if right boundary
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
  }// getNeighborCellsOnGrid

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
      GM_Position pos = (GM_Position) cells.elementAt( i );
      double distance = pos.getDistance( llc );
      if( i == 0 )
        minDistance = distance;
      if( distance <= minDistance && llc.getX() >= pos.getX() && llc.getY() >= pos.getY() )
      {
        minDistance = distance;
        elementIndex = i;
      }
    }// for
    llc = (GM_Position) cells.elementAt( elementIndex );

    elementIndex = -1;
    cells = getNeighborCells( urc );
    minDistance = 0.0;
    for( int i = 0; i < cells.size(); i++ )
    {
      GM_Position pos = (GM_Position) cells.elementAt( i );
      double distance = pos.getDistance( urc );
      if( i == 0 )
        minDistance = distance;
      if( distance <= minDistance && urc.getX() <= pos.getX() && urc.getY() <= pos.getY() )
      {
        minDistance = distance;
        elementIndex = i;
      }
    }
    urc = (GM_Position) cells.elementAt( elementIndex );

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
  public Vector<GM_Position> getCellsFromGrid( GM_Envelope envelope, String cs ) throws GM_Exception
  {
    GM_Object intersection = getExtend().intersection( GeometryFactory.createGM_Surface( envelope, cs ) );
    final Vector<GM_Position> cells = new Vector<GM_Position>();
    int[][] range = getRowColIndexFromEnv( ((GM_Surface) intersection).getEnvelope() );

    for( int r = range[1][1]; r < range[0][1]; r++ )
      for( int c = range[0][0]; c < range[1][0]; c++ )
        cells.addElement( getPosition( r, c ) );
    return cells;

  }// getCellsFromGrid

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.interpolation.grid.IGrid#getID()
   */
  public String getGridID( )
  {
    return m_id;
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getExtend()
   */
  public GM_Surface getExtend( )
  {
    return m_env;
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#merge(org.kalypso.interpolation.grid.IGrid,
   *      org.kalypso.interpolation.grid.IGrid)
   */
  public IGrid merge( IGrid grid1, IGrid grid2 )
  {
    // Auto-generated method stub
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
  public GM_Position[] getGridCells( ) throws GM_Exception
  {
    final List<GM_Position> list = getCellsFromGrid( m_env.getEnvelope(), m_env.getCoordinateSystem() );
    return list.toArray( new GM_Position[list.size()] );
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getOrigin()
   */
  public GM_Position getOrigin( )
  {
    return m_env.getEnvelope().getMin();
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#getCellSize()
   */
  public double getCellSize( )
  {
    return m_cellsize;
  }

  public String getCoordinateSystem( )
  {
    return m_env.getCoordinateSystem();
  }

  private void exportESRIasc( File out ) throws Exception
  {
    DecimalFormat fix = new DecimalFormat( "0.00000" );
    DecimalFormatSymbols dfs = new DecimalFormatSymbols();
    dfs.setDecimalSeparator( '.' );
    fix.setDecimalFormatSymbols( dfs );
    if( out == null )
      out = File.createTempFile( m_id, Grid.DEFAULT_SUFFIX );

    GM_Object intersection = null;
    BufferedWriter bw = new BufferedWriter( new FileWriter( out ) );
    try
    {
      if( m_borderLine != null )
      {
        GM_Object intersection1 = getExtend().intersection( m_borderLine );
        intersection = intersection1.intersection( m_wishbox );
        if( intersection == null )
          throw new Exception( "The boundary (polyline) does not intersect the requested bounding box.\n" + "Interpolation not successful!!" );
      }
      else if( m_wishbox != null && m_borderLine == null )
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
      bw.write( m_nodata );
      bw.write( "\n" );
      double nodata = Double.parseDouble( m_nodata );
      for( int row = 0; row < m_rows; row++ )
      {
        for( int col = 0; col < m_cols; col++ )
        {
          double value = readGridValue( row, col );
          if( value != nodata && intersection.contains( getPosition( row, col ) ) )
            bw.write( fix.format( value ) );
          else
            bw.write( m_nodata );
          bw.write( ' ' );
        }// for j
        bw.newLine();
      }// for i
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      bw.close();
    }

  }

  public void setGridName( String name )
  {
    m_name = name;
  }

  protected void clean( )
  {
    try
    {
      m_gridValues.close();
      rafPath.delete();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.interpolation.grid.IGrid#export(java.io.File)
   */
  public void export( File file )
  {
    System.out.print( "Exporting Grid..." );
    try
    {
      String type = file.getName();
      if( type.matches( ".+\\.(a|A)(s|S)(c|C)$" ) )
        exportESRIasc( file );
      else
        throw new UnsupportedOperationException( "The choosen raster data type is not supported" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    System.out.print( " finished\n" );
  }

}