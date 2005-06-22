/*
 * Created on 05.01.2005
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation;

import java.io.File;

import org.kalypso.interpolation.grid.GridFactory;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypso.interpolation.mesh.MeshFactory;
import org.kalypso.interpolation.mesh.MeshReader;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class KalypsoGridTools implements IKalyposGridTools
{
  private static final MeshReader reader = new MeshReader();

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.interpolation.IKalyposGridTools#interpolateGrid(org.deegree.model.feature.Feature[],
   *      org.deegree.model.feature.Feature[], java.lang.String,
   *      java.lang.String, java.lang.String, double,
   *      org.deegree.model.geometry.GM_Envelope, java.lang.String)
   */
  public void interpolateGrid( Feature[] meshElements, Feature[] nodes,
      String geometryPropertyElement, String geometryPropertyPoint,
      String valueProperty, double cellsize, GM_Envelope gridsize,
      GM_Surface wishbox, GM_Surface polyline, CS_CoordinateSystem crs, File out )
      throws Exception
  {
    CS_CoordinateSystem cs = meshElements[0].getDefaultGeometryProperty()
        .getCoordinateSystem();

    Mesh mesh = MeshFactory.getInstance().getMesh( cs );
    //read mesh
    reader.importMesh( mesh, nodes, geometryPropertyPoint, valueProperty,
        meshElements, geometryPropertyElement, wishbox );
    IGrid grid = GridFactory.getInstance().createGrid( wishbox.getEnvelope(),
        crs, cellsize, mesh );
    long st = System.currentTimeMillis();
    mesh.interpolateGrid( out, polyline, grid );
    long dur = System.currentTimeMillis() - st;
    System.out.print( " in " + dur + " ms." );

  }

}