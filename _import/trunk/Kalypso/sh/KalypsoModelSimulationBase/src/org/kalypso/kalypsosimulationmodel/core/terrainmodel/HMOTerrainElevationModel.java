/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.bce.gis.io.hmo.HMOReader;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jts.io.ParseException;

/**
 * An {@link IElevationProvider} based on an hmo file
 * 
 * @author Patrice Congo
 */
public class HMOTerrainElevationModel 
                    implements  IElevationProvider,
                                SurfacePatchVisitable
{
  
  public static final double[][] NO_INTERIOR = {};
  
  
  
  class TriangleData implements SurfacePatchVisitable
  {
    private LinearRing ring;
    
    public TriangleData( LinearRing ring )
    {
      this.ring=ring;
    }
    
    public org.kalypsodeegree.model.geometry.GM_Surface getSurface()
    {
      return null;
    }
    
    public double getCenterElevation()
    {
      return ring.getCentroid().getCoordinate().x;
    }

    /**
     * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
     */
    public void aceptSurfacePatches( GM_Envelope envToVisit, SurfacePatchVisitor surfacePatchVisitor ) throws GM_Exception
    {
      Coordinate[] coordinates = 
                  ring.getCoordinates();
      double[] exterior = 
            {   
            coordinates[0].x,coordinates[0].y,coordinates[0].z,
            coordinates[1].x,coordinates[1].y,coordinates[1].z,
            coordinates[2].x,coordinates[2].y,coordinates[2].z}; 
      
      GM_Surface surfacePatch = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( 
                                        exterior, 
                                        NO_INTERIOR, 
                                        3,
                                        CRS_GAUSS_KRUEGER);
      surfacePatchVisitor.visit( surfacePatch, getCenterElevation() );
    }    
    
  }
  
  private Quadtree triangles;
 
  public HMOTerrainElevationModel(
                URL hmoFileURL,
                GM_Envelope regionOfInterest )
  {
    
  }
  
  private final void parseFile(URL hmoFileURL) throws IOException, ParseException
  {
    HMOReader hmoReader= new HMOReader(new GeometryFactory());
    Reader r= new InputStreamReader(hmoFileURL.openStream());
    LinearRing[] rings = hmoReader.read( r );
    this.triangles= new Quadtree();
    for(LinearRing ring:rings)
    {
      triangles.insert( 
          ring.getEnvelopeInternal(), 
          new TriangleData(ring));
    }
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  public CS_CoordinateSystem getCoordinateSystem( )
  {
    //TODO Patrice this hard coded and not okay put it into the gml
    
    return CRS_GAUSS_KRUEGER;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    double x = location.getX();
    double y = location.getY();
    Envelope searchEnv= new Envelope(x,x,y,y);
    List list = triangles.query( searchEnv );
    if(list.isEmpty())
    {
      return Double.NaN;
    }
    else
    {
      return ((TriangleData)list.get( 0 )).getCenterElevation(); 
    }
  }


  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
   */
  public void aceptSurfacePatches( 
                      GM_Envelope envToVisit, 
                      SurfacePatchVisitor surfacePatchVisitor ) 
                      throws GM_Exception
  {
    Assert.throwIAEOnNullParam( 
                      envToVisit, "envToVisit" );
    Assert.throwIAEOnNullParam( 
                      surfacePatchVisitor, "surfacePatchVisitor");
    Coordinate max = JTSAdapter.export( envToVisit.getMax());
    Coordinate min = JTSAdapter.export( envToVisit.getMin());
    Envelope jtsEnv= new Envelope(min,max);
    GM_Surface surface = null;
    
    List triToVisit = triangles.query( jtsEnv );
    
    CS_CoordinateSystem crs=getCoordinateSystem();
    for(Object tri:triToVisit)
    {
      ((TriangleData)tri).aceptSurfacePatches( 
                                envToVisit, 
                                surfacePatchVisitor ); 
    }
  }

}
