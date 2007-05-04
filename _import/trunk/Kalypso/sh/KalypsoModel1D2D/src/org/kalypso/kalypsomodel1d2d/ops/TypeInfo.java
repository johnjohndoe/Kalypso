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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides mechanism to get the type info for
 * fe element concepts
 * 
 * @author Patrice Congo
 *
 */
public class TypeInfo
{
  /**
   * Answer whether the given edge is a 1D edge.
   * E.i. an edge in a 1D element.
   * This method get the edge container an checks 
   * return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_ELEMENT1D}
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is1DEdge(IFE1D2DEdge edge)
  {
    if(edge==null)
    {
      return false;
    }
    IFeatureWrapperCollection<IFE1D2DElement> containers = edge.getContainers();
    for( IFE1D2DElement element : containers )
    {
      if(Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D.equals( 
                  element.getWrappedFeature().getFeatureType().getQName()))
      {
       return true; 
      }
    }
    return false;
  }
  
  /**
   * Answer whether the given edge is a 1D edge.
   * E.i. an edge in a 1D element.
   * This method get the edge container an checks 
   * return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_ELEMENT1D}
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is1DEdgeFeature(Feature edgeFeature)
  {
    if(edgeFeature == null )
    {
      return false;
    }
    IFE1D2DEdge edge = (IFE1D2DEdge)edgeFeature.getAdapter( IFE1D2DEdge.class );
    return is1DEdge( edge );
  }

  /**
   * Answer whether the given edge is a 1D edge.
   * E.i. an edge in a 2D element.
   * This method get the edge container an checks 
   * return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_POLY_ELEMENT}
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is2DEdge( IFE1D2DEdge edge )
  {
    if(edge==null)
    {
      return false;
    }
    IFeatureWrapperCollection<IFE1D2DElement> containers = edge.getContainers();
    for(IFE1D2DElement element:containers)
    {
      if(Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT.equals( 
                  element.getWrappedFeature().getFeatureType().getQName()))
      {
       return true; 
      }
    }
    IEdgeInv edgeInv = edge.getEdgeInv();
    if(edgeInv != null)
    {
      IFeatureWrapperCollection<IFE1D2DElement> invContainers = edgeInv.getContainers();
      for(IFE1D2DElement element:invContainers)
      {
        if(Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT.equals( 
                    element.getWrappedFeature().getFeatureType().getQName()))
        {
         return true; 
        }
      }
    }
    return false;
  }

  /**
   * checks whether the provided feature is of type Element1D
   * The check is base on {@link QName} equality and not on 
   * substitution
   * @param feature the feature to check for type
   * @return true if the given feature is an element 1d feature
   */
  public static final boolean isElement1DFeature( Feature feature )
  {
    if(feature==null)
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    }
  }
  
  /**
   * checks whether the provided feature is of type Poly element
   * The check is base on {@link QName} equality and not on 
   * substitution
   * @param feature the feature to check for type
   * @return true if the given feature is an element 1d feature
   */
  public static final boolean isPolyElementFeature( Feature feature )
  {
    if(feature==null)
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
    }
  }

  public static final boolean isContinuityLine( Feature feature )
  {
    if(feature==null)
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
    }
  }

  public static final boolean isEdgeToEdgeJunction( Feature feature )
  {
    if(feature==null)
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE );
    }
  }

  public static final boolean isEdgeToCLineJunction( Feature feature )
  {
    if(feature==null)
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_CLINE );
    }
  }

  public static boolean isNode( Feature selecFeature )
  {
    if(selecFeature==null)
    {
      return false;
    }
    return Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( selecFeature.getFeatureType().getQName() );
  }
  
  public static final boolean isBorderEdge(IFE1D2DEdge edge)
  {
    IFeatureWrapperCollection edgeContainer = edge.getContainers();
    if(edgeContainer.size()!=1)
    {
      return false;      
    }
    IEdgeInv edgeInv = edge.getEdgeInv();
    if(edgeInv!=null)
    {
      if(!edgeInv.getContainers().isEmpty())
      {
        return false;
      }
    }
    
    return true;
    
  }

  /**
   * Check if the given complex element wrapper object
   * wrapps a junction context feature, i.e. a feature which is
   * substituable to wb1d2d:JunctionContext
   * @param complexElement the complex element to test for its
   *        junction context nature
   * @return true if the given complex element wrapper object wrapps
   *            a junction context feature otherwise false, including
   *            the case where the passed junction element is null
   */
  public static final boolean isJuntionContext( IFE1D2DComplexElement complexElement )
  {
    if(complexElement == null )
    {
      return false;
    }
    Feature ceFeature = complexElement.getWrappedFeature();
    IFeatureType featureType = ceFeature.getFeatureType();
    
    if( GMLSchemaUtilities.substitutes( 
            featureType, 
            Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT ))
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
