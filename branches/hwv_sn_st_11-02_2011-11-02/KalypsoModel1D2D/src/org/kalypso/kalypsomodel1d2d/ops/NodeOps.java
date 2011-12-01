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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.sort.FilteredFeatureList;

/**
 * Helper class concerning {@link org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DNode}.
 * 
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class NodeOps
{
  private NodeOps( )
  {
    // do not instantiate
  }

  public static final IFE1D2DNode findeNodeImpl( final GM_Point point, final FE1D2DDiscretisationModel model )
  {
    final FeatureList elementList = model.getElements().getWrappedList();
    final FeatureList element2DList = new FilteredFeatureList( elementList, IPolyElement.QNAME, true );

    // 1. Try: look, if the position is within an element
    final List foundElements = element2DList.query( point.getPosition(), null );
    final IFE1D2DNode nearestNode = nearestNodeOfElements( point, foundElements );
    if( nearestNode != null )
    {
      return nearestNode;
    }

    // 2. try: nearest node, brute force
    return nearestNodeOfElements( point, element2DList );
  }

  private static IFE1D2DNode nearestNodeOfElements( final GM_Point point, final List foundElements )
  {
    double currentDistance = Double.MAX_VALUE;
    IFE1D2DNode nearestNode = null;
    for( final Object object : foundElements )
    {
      final IPolyElement ele = (IPolyElement) ((Feature) object).getAdapter( IPolyElement.class );
      if( ele == null )
        continue;
      final List<IFE1D2DEdge> edges = ele.getEdges();
      for( final IFE1D2DEdge edge : edges )
      {
        final List<IFE1D2DNode> nodes = edge.getNodes();
        for( final IFE1D2DNode node : nodes )
        {
          final GM_Point nodePoint = node.getPoint();
          final double distance = point.distance( nodePoint );

          if( distance < currentDistance )
          {
            currentDistance = distance;
            nearestNode = node;
          }
        }
      }
    }
    return nearestNode;
  }

  public static boolean hasElevation( final IFE1D2DNode node )
  {
    final GM_Point point = node.getPoint();
    final boolean status = false;
    if( point.getCoordinateDimension() <= 2 )
      return false;
    return !Double.isNaN( point.getZ() );
  }
}
