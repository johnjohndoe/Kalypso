/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.kalypso.interpolation.grid.Grid;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window - Preferences - Java - Code Style - Code
 * Templates
 */
public class Mesh
{
  private String name = null;

  private GM_Surface m_env = null;

  private ElementTable et = null;

  private final HashMap m_meshElements = new HashMap();

  private final CS_CoordinateSystem m_crs;

  private Grid m_grid = null;

  private boolean m_gridInitialise = false;

  private GM_Surface m_border = null;

  protected Mesh( String name )
  {
    CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
    this.name = name;
    this.m_crs = cs;
  }//constructor

  protected Mesh( String name, CS_CoordinateSystem cs )
  {
    this.name = name;
    this.m_crs = cs;
  }//constructor

  protected Mesh( PointTable pt, ElementTable et, GM_Surface env, CS_CoordinateSystem cs )
  {
    this.et = et;
    //    this.pt = pt;
    m_env = env;
    m_crs = cs;
  }

  public String getMeshName()
  {
    return name;
  }

  //  public PointTable getPoints()
  //  {
  //    return pt;
  //  }

  public void setMeshName( String name )
  {
    this.name = name;
  }

  public HashMap getMeshElements()
  {
    return m_meshElements;
  }

  /**
   * <B>public void interpolateGrid (Vector bbox,BufferedWriter logWriter)throws Exception </B>
   * <P>
   * Finds cells in each element based on given bounding box of mesh. then interpolate each point using elevations of
   * vertices and make grid using those interpolated values
   * 
   * @param bbox
   *          Vector bounding box of mesh containing lower left and upper right corner points
   * @throws Exception
   */
  //  public void interpolateGrid( String s, GM_Envelope gridSize,
  //      BufferedWriter logWriter ) throws Exception
  //  {
  //    //take cellsize from user as input
  //    //lesser for more fine resolution at
  //    // more cost of time
  //    double cellsize = ( new Double( s ) ).doubleValue();
  //    if( gridSize == null )
  //    {
  //      //calculates the size required for output grid
  //      int cols = ( (int)Math.ceil( m_env.getWidth() / cellsize ) );
  //      int rows = ( (int)Math.ceil( m_env.getHeight() / cellsize ) );
  //      //create a grid for output with an additional row and column
  //      GM_Position llcGrid = GeometryFactory.createGM_Position( m_env.getMin()
  //          .getX(), m_env.getMin().getY() );
  //      m_grid = (Grid)GridFactory.getInstance().getGrid( llcGrid, rows, cols,
  //          cellsize );
  //    }
  //    else
  //    {
  //      m_grid = (Grid)GridFactory.getInstance().getGrid( gridSize, cellsize );
  //    }
  //
  //    logWriter.newLine();
  //    logWriter.write( "Interpolation Method used: " + "Finite Element Method" );
  //
  //    System.out.print( "\n" + "Interpolation started.." );
  //    GM_Envelope gridEnv = m_grid.getGridBBox();
  //    Set keys = et.keySet();
  //    Iterator itElements = keys.iterator();
  //    System.out.print( " for " + keys.size() + " elements." );
  //    int ecounter = 1;
  //    double eNo = getNoElementsInMesh();
  //    while( itElements.hasNext() )
  //    {
  //      long st = System.currentTimeMillis();
  //      String eID = (String)itElements.next();
  //      int percent = (int)Math.round( ecounter / eNo * 100 );
  //      System.out.print( "\nInterpolating element " + eID + "( " + percent
  //          + " % completed )" );
  //      Element e = et.getElement( eID );
  //
  //      if( gridEnv.contains( e.getBBox() ) )
  //      {
  //        Vector cells = m_grid.getCellsFromGrid( e.getBBox() );
  //        Iterator itCells = cells.iterator();
  //        Vector vList = e.getVertList();
  //        while( itCells.hasNext() )
  //        {
  //          GM_Position cell = (GM_Position)itCells.next();
  //          if( cell != null && ( e.getGeometry().contains( cell ) == true ) )
  //          {
  //            Point[] attributes = new Point[e.getVertList().size()];
  //            for( int i = 0; i < vList.size(); i++ )
  //            {
  //              String pID = (String)vList.elementAt( i );
  //              //is this point null ?
  //              Point p = pt.getPoint( pID );
  //              //problems with attribute
  //              attributes[i] = p;
  //              //System.out.println(pID + " " + i + " " + val);
  //            }
  //            // m_grid.writeGridValue( cell, e.interpolatePointFEM( cell,
  //            // attributes ) );
  //          }//if
  //        }//while it
  //        long dur = System.currentTimeMillis() - st;
  //        System.out.print( " in " + dur + " ms." );
  //      }//if elements is contained
  //      ecounter = ecounter + 1;
  //    }//while element
  //    // grid.close();
  //    System.out.println( ".finished." );
  //    logWriter.write( "... successful run ..." );
  //
  //    //export the grid into dat file format, which we can after import in
  //    // grass or arcview
  //    m_grid.export( null );
  //  }//interpolateGrid
  public ElementTable getElements()
  {
    return et;
  }

  public GM_Envelope getEnvelope()
  {

    try
    {
      if( m_env == null )
      {
        GM_Envelope env = null;
        Iterator itTable = m_meshElements.keySet().iterator();
        int counter = 0;
        while( itTable.hasNext() )
        {
          MeshElement me = (MeshElement)m_meshElements.get( itTable.next() );
          if( counter == 0 )
            env = me.getEnvelope();
          else
            env = env.getMerged( me.getEnvelope() );
          counter++;
        }//while
        m_env = GeometryFactory.createGM_Surface( env, m_crs );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
    return m_env.getEnvelope();
  }//getBBox

  public Element getElement( String id )
  {
    return getElements().getElement( id );
  }//getElement

  public int getNoElementsInMesh()
  {
    return et.size();
  }

  //  public int getNoPointsInMesh()
  //  {
  //    return pt.size();
  //  }

  public Grid getGrid()
  {
    return m_grid;
  }

  public void addElement( MeshElement me )
  {
    String id = me.getMeshElementID();
    m_meshElements.put( id, me );

  }

  public void interpolateGrid( File file, GM_Surface polyline, IGrid grid )
  {
    if( m_meshElements.size() == 0 )
      return;
    int progress = 1;
    int noElements = m_meshElements.size();
    try
    {
      Iterator it = m_meshElements.keySet().iterator();
      while( it.hasNext() )
      {

        MeshElement me = (MeshElement)m_meshElements.get( (String)it.next() );
        Vector cells = ( (Grid)grid ).getCellsFromGrid( me.getEnvelope(), me.getCoordinateSystem() );
        System.out.println( progress + " of " + noElements );
        me.interpolateMeshElement( (GM_Position[])cells.toArray( new GM_Position[cells.size()] ), (Grid)grid );
        progress++;
      }
      ( (Grid)grid ).export( polyline, file );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  protected void setBoundingBox( GM_Surface env )
  {
    m_env = env;
  }

  public int size()
  {
    return m_meshElements.size();
  }

  public void removeElement( MeshElement me )
  {
    m_meshElements.remove( me.getMeshElementID() );
  }

  protected boolean isGridInitialised()
  {
    return m_gridInitialise;

  }

  public void setGridInitialize( boolean initialise )
  {
    m_gridInitialise = initialise;
  }

  public void setBorderLine( GM_Surface surface )
  {
    m_border = surface;
  }

  public GM_Surface getBorderLine()
  {
    return m_border;
  }

  public void setGrid( IGrid grid )
  {
    m_grid = (Grid)grid;
  }
}//class
