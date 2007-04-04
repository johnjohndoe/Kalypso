/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.bce.gis.io.hmo.HMOReader;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
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
  
  private double minElevation;
  private double maxElevation;
  private Envelope union;
  
  private Quadtree triangles;

  private Object regionOfInterest;

  private CS_CoordinateSystem crs = CRS_GAUSS_KRUEGER;
 
  public HMOTerrainElevationModel(
                URL hmoFileURL,
                GM_Envelope regionOfInterest ) throws IOException, ParseException
  {
    this.regionOfInterest = regionOfInterest;
    parseFile( hmoFileURL );
  }
  
  private final void parseFile(URL hmoFileURL) throws IOException, ParseException
  {
    HMOReader hmoReader= new HMOReader(new GeometryFactory());
    Reader r= new InputStreamReader(hmoFileURL.openStream());
    LinearRing[] rings = hmoReader.read( r );
    
    this.triangles= new Quadtree();
    
    TriangleData triangleData;
    minElevation = Double.MAX_VALUE;
    maxElevation = Double.MIN_VALUE;
    double extremum;
    
    
//    System.out.println("Parsing:"+rings.length);
    union = rings[0].getEnvelopeInternal();
    for(LinearRing ring:rings)
    {
//      System.out.println("ring:"+ring);
      triangleData=new TriangleData(ring);
      Envelope envelopeInternal = ring.getEnvelopeInternal();
      triangles.insert( 
          envelopeInternal, 
          triangleData);
      //set min
      extremum=triangleData.getMinElevation();
      if(minElevation>extremum)
      {
        minElevation=extremum;
      }      
      //set max 
      extremum=triangleData.getMaxElevation();
      if(maxElevation<extremum)
      {
        maxElevation = extremum;
      }
      
      union.expandToInclude( envelopeInternal );
    }
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    //TODO patrice why not return the real geo object
    try
    {
//      GM_Position min = JTSAdapter.wrap( union. ).getEnvelope();
      return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Envelope( 
                                              union.getMinX(),//minx,
                                              union.getMinY(),//miny,
                                              union.getMaxX(),//maxx,
                                              union.getMaxY()//maxy 
                                              );
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  public CS_CoordinateSystem getCoordinateSystem( )
  {
    //TODO Patrice this hard coded and not okay put it into the gml    
    return crs;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    try
    {
      double x = location.getX();
      double y = location.getY();
      Point jtsPoint = (Point)JTSAdapter.export( location );
      Envelope searchEnv= new Envelope(x,x,y,y);
      List<TriangleData> list = triangles.query( searchEnv );
      if(list.isEmpty())
      {
        System.out.println("List is empty");
        return Double.NaN;
      }
      else
      {
       // System.out.println("Selected triange liste size="+list.size());
        for(TriangleData data:list)
        {
          if(data.contains( jtsPoint ))
          {
            return data.computeZOfTrianglePlanePoint( x, y );//getCenterElevation();
          }
        }
        //System.out.println("trinagle location not in list");
        return Double.NaN;//((TriangleData)list.get( 0 )).getCenterElevation(); 
      }
    }
    catch( Throwable th )
    {
      throw new RuntimeException("Error while getting the elevation",th);
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
    
    List triToVisit = triangles.query( jtsEnv );
    
    for(Object tri:triToVisit)
    {
      ((TriangleData)tri).aceptSurfacePatches( 
                                envToVisit, 
                                surfacePatchVisitor ); 
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  public double getMaxElevation( )
  {
    return maxElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  public double getMinElevation( )
  {
    return minElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( String coordinateSystem )
  {
    // TODO    
  }

}
