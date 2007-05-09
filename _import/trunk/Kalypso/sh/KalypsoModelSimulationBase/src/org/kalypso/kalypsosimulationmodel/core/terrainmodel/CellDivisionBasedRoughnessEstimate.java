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
import org.kalypso.kalypsosimulationmodel.core.ICellDivisionControl;
import org.kalypso.kalypsosimulationmodel.core.SurfaceCellDivision;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * Implementation of roughness estimate based on the
 * Intersection {@link org.kalypsodeegree.model.geometry.GM_Object#intersection(org.kalypsodeegree.model.geometry.GM_Object)} 
 * 
 * @author Patrice Congo
 */
public class CellDivisionBasedRoughnessEstimate implements IRoughnessEstimateSpec
{
  /**
   * List of roughness polygones that contributes to this estimate
   */
  final private List<IRoughnessPolygon> contributingRP;
  
  final private GM_Surface estimateZone; 
  
  final private Map<IRoughnessCls, Double> histogram = 
                        new HashMap<IRoughnessCls, Double>();
  
  class AreaRatioBaseDivisionControl implements ICellDivisionControl
  {

    final double maxAreaRatio;
    final double area;
    final double areaDivMaxAreaRatio;
    final GM_Surface surface;
    
    public AreaRatioBaseDivisionControl( 
                                    final double maxAreaRatio, 
                                    final GM_Surface surface )
    {
      this.surface = surface;
      this.area = surface.getArea();      
      this.maxAreaRatio = maxAreaRatio;
      this.areaDivMaxAreaRatio = area / maxAreaRatio;
    }
    
    public int needDivision( GM_Position[] positions )
    {
      boolean containsLL = surface.contains( positions[0] );
      boolean containsLR = surface.contains( positions[1] );
      boolean containsUL = surface.contains( positions[3] );
      boolean containsUR = surface.contains( positions[2] );
//      if( containsLL & containsLR && containsUL && containsUR )
//      {
//        return DIVISION_TO_BE_STOPED;
//      }
      if( !containsLL  && !containsLR && !containsUL && !containsUR )
      {
        return REMOVE_FROM_PROCESSING;
      }
      else
      {

        double cellArea = SurfaceCellDivision.getCellArea( positions );
        
        if( cellArea > areaDivMaxAreaRatio ) 
        {
          return DIVISION_TO_BE_CONTINUED;
        }
        else
        {
          return DIVISION_TO_BE_STOPED;
        }
      }
      
    }
    
  };
  
  public CellDivisionBasedRoughnessEstimate(
                    final IRoughnessPolygonCollection roughnessPolygonCollection, 
                    final GM_Surface estimateZone,
                    final double maxAreaRatio)
  {
    Assert.throwIAEOnNullParam( roughnessPolygonCollection, "roughnessPolygonCollection" );
    Assert.throwIAEOnNullParam( estimateZone, "estimateZone" );
    this.estimateZone = estimateZone;
    contributingRP = 
      roughnessPolygonCollection.selectRoughnessPolygons( estimateZone );
    ICellDivisionControl cellDivisionControl =
      new AreaRatioBaseDivisionControl(maxAreaRatio, estimateZone );
    List<GM_Position[]> cells = 
          SurfaceCellDivision.toCells( estimateZone, cellDivisionControl );
    makeHistrogram( cells, roughnessPolygonCollection );
  }
  
  private final void makeHistrogram( 
                final List<GM_Position[]> cells,
                final IRoughnessPolygonCollection roughnessPolygonCollection )
  {
    
//    GM_Position center;
//    double areaToAdd;
    double area = 0 ;
    for( GM_Position[] cell : cells )
    {
      final GM_Position center = 
            SurfaceCellDivision.getCellCenter( cell );
      List<IRoughnessPolygon> rps = 
        roughnessPolygonCollection.selectRoughnessPolygons( center );
      final double areaToAdd = SurfaceCellDivision.getCellArea( cell );
      if( !rps.isEmpty() )
      {
        area = area + areaToAdd;
        for( IRoughnessPolygon rp : rps )
        {
          IRoughnessCls roughnessCls = rp.getRoughnessCls();
          Double double1 = histogram.get( roughnessCls );
          if( double1 == null )
          {
            histogram.put( roughnessCls, areaToAdd );
            
          }
          else
          {
            histogram.put( roughnessCls, double1 + areaToAdd );
          }
        }
      }
    }
    System.out.println("TOTAL CELL AREA="+area);
    //normalize
    for( Entry<IRoughnessCls, Double> entry : histogram.entrySet())
    {
      entry.setValue( entry.getValue()/area );
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
    return histogram;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#mostSpreadRoughness()
   */
  public IRoughnessCls[] mostSpreadRoughness( )
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
    strBuilder.append("IntersectionBasedRoughnessEstimate[\n");
    strBuilder.append("\tcontributingRP:\n\t\t");
    strBuilder.append(contributingRP);
    strBuilder.append("\testimateZone:\n\t\t");
    strBuilder.append(estimateZone);
    strBuilder.append("\thistogram:\n\t\t");
    strBuilder.append(histogram);
    strBuilder.append("\n]");
    return strBuilder.toString();
  }
}
