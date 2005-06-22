/*
 * Created on 04.01.2005
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.util.HashMap;
import java.util.Set;

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

  //  public IGrid createGrid( GM_Position llc, CS_CoordinateSystem crs, int r,
  //      int c, double cellsize, Mesh mesh ) throws Exception
  //  {
  //    String key = getKey();
  //    gridTable
  //        .put( key, new Grid( key, null, llc, crs, r, c, cellsize, wishbox ) );
  //    return (Grid)gridTable.get( key );
  //  }//getMesh

  public IGrid createGrid( GM_Envelope wishbox, CS_CoordinateSystem crs, double cellsize, Mesh mesh ) throws Exception
  {
    String key = getKey();
    Grid grid = null;
    GM_Envelope meshEnv = mesh.getEnvelope();
    if( wishbox.contains( meshEnv ) )
      grid = (Grid)getGrid( meshEnv, meshEnv, crs, cellsize );
    else
    {
      grid = (Grid)getGrid( wishbox, meshEnv, crs, cellsize );
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

  private IGrid getGrid( GM_Envelope wishbox, GM_Envelope meshbox, CS_CoordinateSystem crs, double cellsize )
      throws Exception
  {
    String key = getKey();
    GM_Position llc = null;
    GM_Position urc = null;
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
    return new Grid( key, null, llc, crs, rows, cols, cellsize, wishbox );
  }
}