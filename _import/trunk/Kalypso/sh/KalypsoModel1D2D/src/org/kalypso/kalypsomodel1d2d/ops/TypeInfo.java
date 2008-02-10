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
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Provides mechanism to get the type info for fe element concepts
 * 
 * @author Patrice Congo
 * 
 */
public class TypeInfo
{
  /**
   * Answer whether the given edge is a 1D edge. E.i. an edge in a 1D element. This method get the edge container an
   * checks return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_ELEMENT1D}
   * 
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is1DEdge( IFE1D2DEdge edge )
  {
    if( edge == null )
    {
      return false;
    }
    IFeatureWrapperCollection<IFE1D2DElement> containers = edge.getContainers();
    for( IFE1D2DElement element : containers )
    {
      if( IElement1D.QNAME.equals( element.getFeature().getFeatureType().getQName() ) )
      {
        return true;
      }
    }
    return false;
  }

  /**
   * Answer whether the given edge is a 1D edge. E.i. an edge in a 1D element. This method get the edge container an
   * checks return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_ELEMENT1D}
   * 
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is1DEdgeFeature( Feature edgeFeature )
  {
    if( edgeFeature == null )
    {
      return false;
    }
    IFE1D2DEdge edge = (IFE1D2DEdge) edgeFeature.getAdapter( IFE1D2DEdge.class );
    return is1DEdge( edge );
  }

  /**
   * Answer whether the given edge is a 1D edge. E.i. an edge in a 2D element. This method get the edge container an
   * checks return true if a container has the Name {@link Kalypso1D2DSchemaConstants#WB1D2D_F_POLY_ELEMENT}
   * 
   * @return true if the given edge is an 1d edge otherwise false
   * 
   */
  public static final boolean is2DEdge( IFE1D2DEdge edge )
  {
    if( edge == null )
    {
      return false;
    }
    IFeatureWrapperCollection<IFE1D2DElement> containers = edge.getContainers();
    for( IFE1D2DElement element : containers )
    {
      if( IPolyElement.QNAME.equals( element.getFeature().getFeatureType().getQName() ) )
      {
        return true;
      }
    }
    return false;
  }

  /**
   * checks whether the provided feature is of type Element1D The check is base on {@link QName} equality and not on
   * substitution
   * 
   * @param feature
   *            the feature to check for type
   * @return true if the given feature is an element 1d feature
   */
  public static final boolean isElement1DFeature( Feature feature )
  {
    if( feature == null )
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( IElement1D.QNAME );
    }
  }

  public static final boolean isPolyElementFeature( IFeatureWrapper2 featureWrapper )
  {
    if( featureWrapper == null )
    {
      return false;
    }
    else
    {
      return isPolyElementFeature( featureWrapper.getFeature() );
    }
  }

  /**
   * checks whether the provided feature is of type Poly element The check is base on {@link QName} equality and not on
   * substitution
   * 
   * @param feature
   *            the feature to check for type
   * @return true if the given feature is an element 1d feature
   */
  public static final boolean isPolyElementFeature( Feature feature )
  {
    if( feature == null )
    {
      return false;
    }
    else
    {
      QName featureQName = feature.getFeatureType().getQName();
      return featureQName.equals( IPolyElement.QNAME );
    }
  }

  public static final boolean isContinuityLine( final Feature feature )
  {
    if( feature == null )
      return false;
    else
      return (GMLSchemaUtilities.substitutes( feature.getFeatureType(), IFELine.QNAME ));
  }

  public static boolean isNode( Feature selecFeature )
  {
    if( selecFeature == null )
    {
      return false;
    }
    return Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( selecFeature.getFeatureType().getQName() );
  }
}
