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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Implementation of roughness estimate based on the
 * Intersection {@link org.kalypsodeegree.model.geometry.GM_Object#intersection(org.kalypsodeegree.model.geometry.GM_Object)} 
 * 
 * @author Patrice Congo
 */
public class IntersectionBasedRoughnessEstimate implements IRoughnessEstimateSpec
{
  /**
   * List of roughness polygones that contributes to this estimate
   */
  final private List<IRoughnessPolygon> contributingRP;
  
  final private GM_Surface estimateZone; 
  
  private Map<IRoughnessCls, Double> histogram = null; 
                        //new HashMap<IRoughnessCls, Double>();
  private double mostSpreadPart;

  private IRoughnessCls[] mostSpreadRoughnesses;
  
  public IntersectionBasedRoughnessEstimate(
                    IRoughnessPolygonCollection roughnessPolygonCollection, 
                    GM_Surface estimateZone )
  {
    Assert.throwIAEOnNullParam( roughnessPolygonCollection, "roughnessPolygonCollection" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( estimateZone, "estimateZone" ); //$NON-NLS-1$
    this.estimateZone = estimateZone;
    contributingRP = 
      roughnessPolygonCollection.selectRoughnessPolygons( estimateZone );
//    makeHistrogram();
  }
  
  public IntersectionBasedRoughnessEstimate( 
                        RoughnessPolygonCollection roughnessPolygonCollection, 
                        GM_Curve estimateLine )
  {
    this(roughnessPolygonCollection, toSurface( estimateLine ));
  }

  private static final GM_Surface toSurface( GM_Curve estimateLine )
  {
    Assert.throwIAEOnNullParam( estimateLine, "estimateLine" ); //$NON-NLS-1$
    try
    {
      final double delta = estimateLine.getLength()/100;
      GM_Position[] positions = estimateLine.getAsLineString().getPositions();
      List<GM_Position> surfaceOuter= new ArrayList<GM_Position>(positions.length*3);
      for( int i = 0; i<positions.length; i++ )
      {
        final GM_Position pos= positions[i];
        final GM_Position posT = GeometryFactory.createGM_Position( pos.getX(), pos.getY() + delta );
        surfaceOuter.add( posT );
      }
      
      for(int i = positions.length-1; i>=0; i-- )
      {
        final GM_Position pos= positions[i];
        final GM_Position posT = GeometryFactory.createGM_Position( pos.getX(), pos.getY() - delta );
        surfaceOuter.add( posT );
      }
      
      surfaceOuter.add( surfaceOuter.get( 0 ) );
      GM_Surface surface = 
        GeometryFactory.createGM_Surface( 
                surfaceOuter.toArray( new GM_Position[surfaceOuter.size()] ), 
                new GM_Position[0][], 
                new GM_SurfaceInterpolation_Impl( 
                              GM_SurfaceInterpolation.PLANAR ),
                estimateLine.getCoordinateSystem() );
      return surface;
    }
    catch ( Throwable th ) 
    {
      throw new RuntimeException(th);
    }
  }

  private final void makeHistrogram( ) throws GM_Exception
  {
    histogram = new HashMap<IRoughnessCls, Double>();
    Geometry jtsEstimateZone = JTSAdapter.export( estimateZone );
    final double areaOfEstimateZone = jtsEstimateZone.getArea();
    for( IRoughnessPolygon roughnessPolygon : contributingRP )
    {
      
      final Geometry rpolygone = JTSAdapter.export( roughnessPolygon.getSurface() );
      final Geometry jtsIntersect = jtsEstimateZone.intersection( rpolygone );
      if(jtsIntersect != null )
      {
        IRoughnessCls roughnessCls = roughnessPolygon.getRoughnessCls();
        Double currentArea = histogram.get(  roughnessCls );
        double areaToAdd = jtsIntersect.getArea( );
        if( areaToAdd == 0 )
        {
          //skip
        }
        else if( currentArea == null )
        {
          histogram.put( roughnessCls, areaToAdd/areaOfEstimateZone );
        }
        else
        {
          histogram.put( roughnessCls, currentArea + areaToAdd/areaOfEstimateZone );
        }
      }

    }
  }
  

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getAreaRatio()
   */
  public double getAreaRatio( )
  {
    return Double.NaN;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getCells()
   */
  public GM_Envelope[] getCells( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getContributingRoughnessPolygons()
   */
  public List<IRoughnessPolygon> getContributingRoughnessPolygons( )
  {
    return contributingRP;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getHistogramm()
   */
  public Map<IRoughnessCls, Double> getHistogramm( )
  {
    if( histogram == null )
    {
      try
      {
        makeHistrogram();
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
        throw new RuntimeException(e);
      }
    }
    return histogram;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#mostSpreadRoughness()
   */
  public IRoughnessCls[] mostSpreadRoughness( )
  {
//    final int size = contributingRP.size();
//    if( size == 1 )
//    {
//      return new IRoughnessCls[]{ 
//                  contributingRP.get( 0 ).getRoughnessCls() };
//    }
//    else if( size == 0 )
//    {
//      return new IRoughnessCls[]{};
//    }
//    else
//    {
//      if( histogram == null )
//      {
//        try
//        {
//          makeHistrogram();
//        }
//        catch( GM_Exception e )
//        {
//          e.printStackTrace();
//          throw new RuntimeException(e);
//        }
//      }
//      double[] retMostSpreadPart = { Double.NaN };
//      mostSpreadRoughnesses = mostSpreadRoughness( histogram, retMostSpreadPart );
//      
//    }
    if( mostSpreadRoughnesses == null )
    {
      initMostSpread();
    }
    return mostSpreadRoughnesses;
    
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getMostSpreadPart()
   */
  public double getMostSpreadPart( )
  {
    if( mostSpreadRoughnesses == null )
    {
      initMostSpread();
    }
    return mostSpreadPart;
  }
  
  private final void initMostSpread()
  {
    final int size = contributingRP.size();
    if( size == 1 )
    {
      mostSpreadRoughnesses = 
          new IRoughnessCls[]{ 
                  contributingRP.get( 0 ).getRoughnessCls() };
      mostSpreadPart = 1 ;
    }
    else if( size == 0 )
    {
      mostSpreadRoughnesses = 
                new IRoughnessCls[]{ };
      mostSpreadPart = Double.NaN ;
    }
    else
    {
      if( histogram == null )
      {
        try
        {
          makeHistrogram();
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException(e);
        }
      }
      double[] retMostSpreadPart = { Double.NaN };
      mostSpreadRoughnesses = mostSpreadRoughness( histogram, retMostSpreadPart );
      mostSpreadPart = retMostSpreadPart[0]; 
    }
  }
  
  public static final IRoughnessCls[] mostSpreadRoughness( 
                                        Map <IRoughnessCls, Double> histogram, double[] mostSpreadPart)
  {
    List<IRoughnessCls> mostSpreads =  new ArrayList<IRoughnessCls>();
    Double max = Double.MIN_VALUE;
    Set<Entry<IRoughnessCls, Double>> entries = histogram.entrySet();
    for( Entry<IRoughnessCls, Double> entry : entries )
    {
      final double area = entry.getValue();
      if( area == max )
      {
        mostSpreads.add(  entry.getKey() );
      }
      else if( area > max )
      {
        max = area;
        mostSpreads.clear();
        mostSpreads.add( entry.getKey() );
        if( mostSpreadPart != null )
        {
          if( mostSpreadPart.length >0 )
          {
            mostSpreadPart[0] = max;
          }
        }
      }
      else //area<max
      {
        //nothing to do
      }
    }
    return mostSpreads.toArray( new IRoughnessCls[]{} );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#mostSpreadRoughness(org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec.ERoughnessSelectionMechanism)
   */
  public IRoughnessCls mostSpreadRoughness( ERoughnessSelectionMechanism rsm )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#possibleRoughnesses()
   */
  public IRoughnessCls[] possibleRoughnesses( )
  {
    return histogram.keySet().toArray( new IRoughnessCls[]{} );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#setRatio(double)
   */
  public void setRatio( double ratio )
  {
    
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    StringBuilder strBuilder = new StringBuilder( 256 );
    strBuilder.append("IntersectionBasedRoughnessEstimate[\n"); //$NON-NLS-1$
    strBuilder.append("\tcontributingRP:\n\t\t"); //$NON-NLS-1$
    strBuilder.append(contributingRP);
    strBuilder.append("\testimateZone:\n\t\t"); //$NON-NLS-1$
    strBuilder.append(estimateZone);
    strBuilder.append("\thistogram:\n\t\t"); //$NON-NLS-1$
    strBuilder.append(histogram);
    strBuilder.append("\n]"); //$NON-NLS-1$
    return strBuilder.toString();
  }
}
