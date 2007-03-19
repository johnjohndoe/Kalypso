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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This class provides mechanism for getting feature that are affected by
 * the modification of the geometrie of another feature.
 * This mechnanism are only provided in the context of 1D/2d finit
 * element model, where only the node has got a geomtrie and 
 * the other concepts like edge element complex element have computed geomtrie
 * that change the position. These concepts do share 
 * {@link org.kalypsodeegree.model.geometry.GM_Position}. 
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
public class OpsGeoEditAffected
{

  /**
   * Implements the algorithm to get the fe concept features affected
   * by a modification of a PolyElement geometry 
   */
  private static final void addAffectedFeaturesByPolyElementGeomChange( Feature feature, List<Feature> affectedFeature )
  {
    IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> polyElement= 
          (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
    if(polyElement==null)
    {
      throw new IllegalArgumentException("feature mus be a Polyelement:"+polyElement);
    }
    
    for(IFE1D2DEdge edge : polyElement.getEdges())
    {
      Feature wrappedFeature = edge.getWrappedFeature();
      wrappedFeature.invalidEnvelope();
      affectedFeature.add(wrappedFeature);
    }
    
    List<IFE1D2DNode> nodes = polyElement.getNodes();
    for(IFE1D2DNode node:nodes)
    {
      Feature wrappedFeature = node.getWrappedFeature();
      wrappedFeature.invalidEnvelope();
      affectedFeature.add( wrappedFeature );
    }
  }

  private static final void addAffectedFeaturesByNodeGeomChange( Feature feature, List<Feature> targetFeature )
  {
    throw new UnsupportedOperationException("not supported yet");
  } 

  /**
   * To get the features which are affected by a modification
   * of the given feature geometry.
   * Note that this method also invalidate the envvelope of the 
   * affected features.
   * 
   * @param changedFeature the feature which geometry has been modified
   * @param affectedFeatures a list to added the feature affected by the 
   *            the geometrie modification 
   */
  public static final  void addAffectedsByFEFeatureGeomChange(
                                              Feature changedFeature, 
                                              List<Feature> affectedFeatures )
  {
    if(TypeInfo.isNode( changedFeature ))
    {
      OpsGeoEditAffected.addAffectedFeaturesByNodeGeomChange(
                                  changedFeature, affectedFeatures);
    }
    else if(TypeInfo.isPolyElementFeature( changedFeature ))
    {
      OpsGeoEditAffected.addAffectedFeaturesByPolyElementGeomChange(
                                         changedFeature, affectedFeatures);
    }
    else
    {
      throw new RuntimeException(
          "Moving this type of node is not supported:"+changedFeature);
    }
  }
  
}
