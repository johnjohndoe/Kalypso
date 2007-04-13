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
package org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness;

import java.util.List;
import java.util.Map;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author madanago
 *
 */
public class MakeRoughnessEstimate implements IRoughnessEstimateSpec
{

  private GM_Polygon gPolygon;
  private ColorOnFEDataModel dataModel;

  MakeRoughnessEstimate(GM_Polygon gp,ColorOnFEDataModel feDataModel ){
    this.gPolygon = gp;
    this.dataModel = feDataModel;
    
  }
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getAreaRatio()
   */
  public double getAreaRatio( )
  {
    
    return 0;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getCells()
   */
  public GM_Envelope[] getCells( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getContributingRoughnessPolygons()
   */
  public List<IRoughnessPolygon> getContributingRoughnessPolygons( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#getHistogramm()
   */
  public Map<String, Double> getHistogramm( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#mostSpreadRoughness()
   */
  public String[] mostSpreadRoughness( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#mostSpreadRoughness(org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec.ERoughnessSelectionMechanism)
   */
  public String mostSpreadRoughness( ERoughnessSelectionMechanism rsm )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#possibleRoughnesses()
   */
  public String[] possibleRoughnesses( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec#setRatio(double)
   */
  public void setRatio( double ratio )
  {
    // TODO Auto-generated method stub

  }

}
