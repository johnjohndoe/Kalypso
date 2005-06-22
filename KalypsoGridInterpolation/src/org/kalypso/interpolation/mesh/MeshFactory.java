/*
 * Created on 14.12.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import java.io.File;
import java.util.HashMap;

import org.kalypsodeegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class MeshFactory
{
  public final static String MESH_NAMESPACE = "http://www.tu-harburg.de/GridInterpolation";

  private static MeshFactory myInstance = new MeshFactory();

  private MeshReader m_reader = new MeshReader();

  //private Mesh myMesh;
  private final HashMap meshTable;

  /*
   * private MeshFactory(){ myMesh = new Mesh(); }
   */
  private MeshFactory()
  {
    meshTable = new HashMap();
  }

  public static MeshFactory getInstance()
  {
    return myInstance;
  }

  private String generateKey()
  {
    String key = null;
    int i = 1 + meshTable.size();
    key = MESH_NAMESPACE + "#" + i;
    return key;
  }

  public Mesh getMesh( CS_CoordinateSystem cs )
  {
    String key = generateKey();
    meshTable.put( key, new Mesh( key, cs ) );
    return (Mesh)meshTable.get( key );
  }//getMesh

  public String[] getMeshKeys()
  {
    return (String[])meshTable.keySet().toArray(
        new String[meshTable.keySet().size()] );
  }

  public Mesh getMesh( String key )
  {
    return (Mesh)meshTable.get( key );
  }

  public Mesh readMesh( File[] files, CS_CoordinateSystem cs,
      GM_Surface wishbox, String type, String shapeBase )
  {
    
    return m_reader.importMesh( getMesh(cs), files, cs, wishbox, type, shapeBase ); 
  }
}//class MeshFactory
