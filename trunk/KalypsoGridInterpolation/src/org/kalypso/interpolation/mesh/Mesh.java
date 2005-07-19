/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

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

  //  private ElementTable et = null;

  private final HashMap m_meshElements = new HashMap();

  private final CS_CoordinateSystem m_crs;

  private Grid m_grid = null;

//  private boolean m_gridInitialise = false;

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

  public Grid getGrid()
  {
    return m_grid;
  }

  public void addElement( MeshElement me )
  {
    String id = me.getMeshElementID();
    m_meshElements.put( id, me );

  }

  public void interpolateGrid( IGrid grid )
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
        //System.out.println( progress + " of " + noElements );
        me.interpolateMeshElement( (GM_Position[])cells.toArray( new GM_Position[cells.size()] ), (Grid)grid );
        progress++;
      }
      //      ( (Grid)grid ).exportESRIasc( polyline, file );
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

//  protected boolean isGridInitialised()
//  {
//    return m_gridInitialise;
//
//  }
//
//  public void setGridInitialize( boolean initialise )
//  {
//    m_gridInitialise = initialise;
//  }

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
