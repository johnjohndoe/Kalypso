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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Utility stuff related to the 1d2d discretisation model.
 * 
 * @author Gernot Belger
 */
public class DiscretisationModelUtils
{
  private DiscretisationModelUtils( )
  {
    throw new UnsupportedOperationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils.0" ) ); //$NON-NLS-1$
  }

  /**
   * Checks if a node is a 1d-node.
   * <p>
   * A 1d-node is a node which as at least on 1D-element connected to it. TODO move this into the TypeInfo class
   */
  @SuppressWarnings("unchecked")
  public static boolean is1DNode( final IFE1D2DNode node )
  {
    final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] elements = node.getElements();
    for( final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element : elements )
    {
      if( element instanceof IElement1D )
        return true;
    }

    return false;
  }

  /**
   */
  public static IFeatureWrapper2 findModelElementForBC( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {

    // We want to select nodes, but if the node is the first to be selected, that line will never be selected
    // if mouse is near any node (i.e. line can be selected only if mouse is outside the model)

    // solution: we will select the nodes with small grab distance, so node is selected only if mouse is
    // really near to the node; otherwise, line will be selected

    // nice :)

    // but in the moment we don't want to support nodal BCs...
    // final IFE1D2DNode node = discModel.findNode( currentPos, grabDistance / 32 );
    // if( node != null )
    // return node;

    final IFELine contiLine = discModel.findContinuityLine( currentPos, grabDistance / 2 );
    if( contiLine != null )
      return contiLine;

    final IElement1D element1d = discModel.find1DElement( currentPos, grabDistance );
    if( element1d != null )
      return element1d;

    final IPolyElement element2d = discModel.find2DElement( currentPos, grabDistance );
    return element2d;
  }

  /** Find the opposite node from an element.@return The first node of the element which is not qual tro the given node. */
  public static IFE1D2DNode findOtherNode( final IFE1D2DNode node, final IFE1D2DElement element )
  {
    final List<IFE1D2DNode> nodes = element.getNodes();
    for( final IFE1D2DNode otherNode : nodes )
    {
      if( otherNode != null && !(otherNode.equals( node )) )
        return otherNode;
    }

    return null;
  }

  /**
   * Finds the discretisation model for an item of a model.
   * 
   * @return <code>null</code>, if the parent feature of the given item is not a discretisation model.
   */
  public static IFEDiscretisationModel1d2d modelForItem( final IFeatureWrapper2 modelItem )
  {
    final Feature parent = modelItem.getFeature().getParent();
    return (IFEDiscretisationModel1d2d) parent.getAdapter( IFEDiscretisationModel1d2d.class );
  }

  /**
   * Return the discretisation model for one of its themes (nodes, edges, elements, ...)
   */
  public static IFEDiscretisationModel1d2d modelForTheme( final IKalypsoFeatureTheme theme )
  {
    final FeatureList featureList = theme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    return (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
  }

}
