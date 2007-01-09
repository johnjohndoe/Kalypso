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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * (static) helper functions for the {@link org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel}
 * class.
 * 
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class ModelOps
{
  private ModelOps( )
  {
    // never instatiate
  }

  public static IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] routing( final FE1D2DNode startNode, final FE1D2DNode endNode )
  {
    final List<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>> edgeList = new ArrayList<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>>();

    try
    {
      final Point startPoint = (Point) JTSAdapter.export( startNode.getPoint() );
      final Point endPoint = (Point) JTSAdapter.export( endNode.getPoint() );

      final LineString lineSegment = new GeometryFactory().createLineString( new Coordinate[] { startPoint.getCoordinate(), endPoint.getCoordinate() } );

      IFE1D2DNode lastFoundNode = startNode;
      while( !lastFoundNode.equals( endNode ) )
      {
        // alle benachbarten elemente des aktuellen knoten
        final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] elements = lastFoundNode.getElements();

        final Set<IFE1D2DNode<IFE1D2DEdge>> goodNodes = new HashSet<IFE1D2DNode<IFE1D2DEdge>>();

        // alle deren kanten, welche das segment schneiden
        for( final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element : elements )
        {
          final IFeatureWrapperCollection<IFE1D2DEdge> edges = element.getEdges();
          for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
          {
            final GM_Curve curve = edge.getCurve();
            final LineString edgeSegment = (LineString) JTSAdapter.export( curve );

            if( lineSegment.intersects( edgeSegment ) )
            {
              // Handle special case: if the intersection lies exactly at the current point
              // (always happens for the startPoint) ignore this edge
              final Geometry geometry = lineSegment.intersection( edgeSegment );
              final Coordinate[] coordinates = geometry.getCoordinates();
              boolean isGood = true;
              for( final Coordinate coordinate : coordinates )
              {
                final GM_Point point = lastFoundNode.getPoint();
                final Point lastFoundPoint = (Point) JTSAdapter.export( point );
                if( coordinate.equals2D( lastFoundPoint.getCoordinate() ) )
                  isGood = false;
              }

              if( isGood )
              {
                for( final IFE1D2DNode<IFE1D2DEdge> node : edge.getNodes() )
                  goodNodes.add( node );
              }
            }
          }
        }

        // find suitable edge
        IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> shortestFoundEdge = null;
        double minEdgeLength = Double.MAX_VALUE;

        final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] edges = lastFoundNode.getEdges();
        for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
        {
          // get opposite node (not me)
          final IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
          if( nodes.size() == 2 )
          {
            final IFE1D2DNode oppositeNode;
            if( nodes.get( 0 ).equals( lastFoundNode ) )
              oppositeNode = nodes.get( 1 );
            else
              oppositeNode = nodes.get( 0 );

            if( goodNodes.contains( oppositeNode ) )
            {
              // the current best edge is the shortest edge, which is connected to
              // the current point, intersect the line segment and was not encountered yet
              final double length = edge.getCurve().getLength();
              if( length < minEdgeLength )
              {
                shortestFoundEdge = edge;
                minEdgeLength = length;
              }
            }
          }
        }

        // if we have a shortest edge, use it!
        if( shortestFoundEdge != null )
        {
          edgeList.add( shortestFoundEdge );

          final IFeatureWrapperCollection<IFE1D2DNode> nodes = shortestFoundEdge.getNodes();
          if( nodes.size() == 2 )
          {
            if( nodes.get( 0 ).equals( lastFoundNode ) )
              lastFoundNode = nodes.get( 1 );
            else
              lastFoundNode = nodes.get( 0 );
          }
          else
            throw new IllegalArgumentException( "Edge with nodeCount != 2 encountered." );
        }
        else
          throw new IllegalArgumentException( "No elements found for node: " + lastFoundNode );
      }

    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return edgeList.toArray( new FE1D2DEdge[edgeList.size()] );

  }

}
