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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
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
  private static final void addAffectedFeaturesByPolyElementGeomChange( 
                                                  Feature feature, 
                                                  Set<Feature> affectedFeature )
  {    
    final IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge> polyElement= 
                    (IPolyElement) feature.getAdapter( IPolyElement.class );
    if(polyElement==null)
    {
      throw new IllegalArgumentException("feature mus be a Polyelement:"+polyElement);
    }
    List<IFE1D2DNode> nodes = polyElement.getNodes();
    for(IFE1D2DNode node: nodes)
    {
      addAffectedFeaturesByNodeGeomChange( 
                              node.getWrappedFeature(), 
                              affectedFeature );
    }
    
  }

  private static final void addAffectedFeaturesByNodeGeomChange( 
                                          Feature feature, 
                                          Set<Feature> targetFeature )
  {
    Assert.throwIAEOnNullParam( feature, "feature" );
    IFE1D2DNode<IFE1D2DEdge> node = 
        (IFE1D2DNode<IFE1D2DEdge>) feature.getAdapter( IFE1D2DNode.class );
    if(node == null)
    {
      throw new IllegalArgumentException(
          "Unable to adapt to node argument must be a node"+
          "Argument feature must be a node; but is "+feature.getFeatureType().getQName()
          );
    }
    
    IFeatureWrapperCollection<IFE1D2DEdge> edges = node.getContainers();
    List<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>> elements=
      new ArrayList<IFE1D2DElement<IFE1D2DComplexElement,IFE1D2DEdge>>();
    for(IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge:edges)
    {
      targetFeature.add(edge.getWrappedFeature());
      elements.addAll( 
            (Collection< ? extends IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>>) edge.getContainers());
    }
    
    for(IFE1D2DElement element:elements)
    {
      targetFeature.add( element.getWrappedFeature() );
    }
    
    
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
                                              Set<Feature> affectedFeatures )
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
    else if(TypeInfo.isElement1DFeature( changedFeature ))
    {
      OpsGeoEditAffected.addAffectedFeaturesByElement1DGeomChange(
                                                    changedFeature, 
                                                    affectedFeatures);
    }
    else if(TypeInfo.isContinuityLine( changedFeature ))
    {
      addAffectedFeaturesByContinuityLineGeomChange(
                                    changedFeature, 
                                    affectedFeatures);
    }
    else
    {
      throw new RuntimeException(
          "Moving this type of node is not supported:"+changedFeature);
    }
  }

  private static void addAffectedFeaturesByContinuityLineGeomChange( Feature changedFeature, Set<Feature> affectedFeatures )
  {
    final IFE1D2DContinuityLine cLine= 
      (IFE1D2DContinuityLine)changedFeature.getAdapter( IFE1D2DContinuityLine.class );
    if(cLine==null)
    {
      throw new IllegalArgumentException(
          "feature mus be a continuity line element:"+cLine);
    }
    
    List<IFE1D2DNode> nodes = cLine.getNodes();
    for(IFE1D2DNode node: nodes)
    {
      addAffectedFeaturesByNodeGeomChange( 
                              node.getWrappedFeature(), 
                              affectedFeatures );
    }
    
  }

  /**
   * To get the element affected by a change of the geometry of a 1D element
   */
  private static void addAffectedFeaturesByElement1DGeomChange( 
                                          Feature changedFeature, 
                                          Set<Feature> affectedFeatures )
  {
    final IElement1D element1D= 
      (IElement1D) changedFeature.getAdapter( IElement1D.class );
    if(element1D==null)
    {
      throw new IllegalArgumentException("feature mus be a 1d element:"+element1D);
    }
    
    List<IFE1D2DNode> nodes = element1D.getNodes();
    for(IFE1D2DNode node: nodes)
    {
      addAffectedFeaturesByNodeGeomChange( 
                              node.getWrappedFeature(), 
                              affectedFeatures );
    }
  }
  
}
