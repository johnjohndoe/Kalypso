/*
 * Created on 04.01.2005
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window - Preferences - Java - Code Style - Code
 * Templates
 */
public class GridFactory
{
  private static GridFactory myInstance = new GridFactory();

  private final HashMap gridTable;

  private static final String GRID_NAMESPACE = "http://www.tu-harburg.de/Grid";

  private GridFactory()
  {
    gridTable = new HashMap();
  }

  public static GridFactory getInstance()
  {
    return myInstance;
  }

  public IGrid createGrid( GM_Position llc, CS_CoordinateSystem crs, int r, int c, double cellsize,
      GM_Surface borderline ) throws Exception
  {
    String key = getKey();
    gridTable.put( key, new Grid( key, null, llc, crs, r, c, cellsize, null, borderline ) );
    return (Grid)gridTable.get( key );
  }//getMesh

  public IGrid createGrid( GM_Envelope wishbox, CS_CoordinateSystem crs, double cellsize, Mesh mesh ) throws Exception
  {
    String key = getKey();
    Grid grid = null;
    GM_Envelope meshEnv = null;
    GM_Surface borderline = null;
    if( mesh != null )
    {
      meshEnv = mesh.getEnvelope();
      borderline = mesh.getBorderLine();
    }
    if( meshEnv == null && wishbox != null )
      grid = (Grid)getGrid( wishbox, wishbox, crs, cellsize, borderline );
    else if( wishbox.contains( meshEnv ) )
      grid = (Grid)getGrid( meshEnv, meshEnv, crs, cellsize, borderline );
    else
    {
      grid = (Grid)getGrid( wishbox, meshEnv, crs, cellsize, borderline );
    }
    gridTable.put( key, grid );
    return (Grid)gridTable.get( key );
  }//getMesh

  public Set keySet()
  {
    return myInstance.gridTable.keySet();
  }

  private String getKey()
  {
    int i = 1 + gridTable.size();
    String key = GRID_NAMESPACE + i;
    return key;
  }

  public IGrid[] getAllGrids()
  {

    Object[] a = new Object[gridTable.size()];
    gridTable.values().toArray( a );
    return (IGrid[])a;

  }

  private IGrid getGrid( GM_Envelope wishbox, GM_Envelope meshbox, CS_CoordinateSystem crs, double cellsize,
      GM_Surface borderline ) throws Exception
  {
    String key = getKey();
    GM_Position llc = null;
    /*
     * find the largest grid extend where all Elements of the mesh are inside and the grid is positioned according the
     * wishbox
     */
    GM_Envelope merge = meshbox.getMerged( wishbox );
    //find lower left corner
    double minDeltaX = ( wishbox.getMin().getX() - merge.getMin().getX() ) / cellsize;
    double minDeltaY = ( wishbox.getMin().getY() - merge.getMin().getY() ) / cellsize;
    int minDeltaCol = (int)Math.abs( minDeltaX ) + 2;
    int minDeltaRow = (int)Math.abs( minDeltaY ) + 2;
    //merge lays left of wishbox (x and y)
    if( minDeltaX >= 0 && minDeltaY >= 0 )
      llc = GeometryFactory.createGM_Position( wishbox.getMin().getX() - minDeltaCol * cellsize, wishbox.getMin()
          .getY()
          - minDeltaRow * cellsize );
    //merge lays left of wishbox (x) and above wishbox (y)
    else if( minDeltaX >= 0 && minDeltaY <= 0 )
      llc = GeometryFactory.createGM_Position( wishbox.getMin().getX() - minDeltaCol * cellsize, wishbox.getMin()
          .getY()
          + minDeltaRow * cellsize );
    //merge lays right of wishbox (x) and above wishbox (y)
    else if( minDeltaX <= 0 && minDeltaY <= 0 )
      llc = GeometryFactory.createGM_Position( wishbox.getMin().getX(), wishbox.getMin().getY() );
    //merge lays right of wishbox (x) and below wishbox (y)
    else if( minDeltaX <= 0 && minDeltaY >= 0 )
      llc = GeometryFactory.createGM_Position( wishbox.getMin().getX(), wishbox.getMin().getY() - minDeltaRow
          * cellsize );
    double width = merge.getMax().getX() - llc.getX();
    double height = merge.getMax().getY() - llc.getY();
    int cols = (int)( width / cellsize ) + 2;
    int rows = (int)( height / cellsize ) + 2;
    return new Grid( key, null, llc, crs, rows, cols, cellsize, wishbox, borderline );
  }

  public IGrid importESRIasc( File file, CS_CoordinateSystem crs ) throws IOException, Exception
  {
    System.out.println( "Starting ESRI ASCII raster import ...." );
    long startTime = System.currentTimeMillis();
    StreamTokenizer st = new StreamTokenizer( new FileReader( file ) );
    st.parseNumbers();
    st.wordChars( 'a', 'z' );
    st.wordChars( 'A', 'Z' );
    st.wordChars( '_', '_' );
    st.nextToken();
    st.nextToken();
    int cols = (int)st.nval;
    st.nextToken();
    st.nextToken();
    int rows = (int)st.nval;
    st.nextToken();
    st.nextToken();
    double llcx = st.nval;
    st.nextToken();
    st.nextToken();
    double llcy = st.nval;
    st.nextToken();
    st.nextToken();
    double cellsize = st.nval;
    st.nextToken();
    st.nextToken();

    String nodata = String.valueOf( (int)st.nval );
    Grid grid = (Grid)createGrid( GeometryFactory.createGM_Position( llcx, llcy ), crs, rows, cols, cellsize, null );
    grid.setNodata( nodata );
    for( int r = 0; r < rows; r++ )
    {
      for( int c = 0; c < cols; c++ )
      {
        st.nextToken();
        double value = st.nval;
        grid.writeGridValue( r, c, value );
      }
    }
    System.out.print( " ...finished in " + (( System.currentTimeMillis() - startTime )/1000 )+ " seconds \n" );
    return grid;
  }

  public void clearFactory()
  {
    Iterator it = gridTable.keySet().iterator();
    while( it.hasNext() )
    {
      Grid grid = (Grid)gridTable.get( it.next() );
      grid.clean();
      grid = null;
    }
    gridTable.clear();
  }
}