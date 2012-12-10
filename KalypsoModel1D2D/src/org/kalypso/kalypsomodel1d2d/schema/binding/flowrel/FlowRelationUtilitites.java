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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Helper class for {@link org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship}s.
 * 
 * @author Gernot Belger
 * 
 */
@SuppressWarnings("unchecked")
public class FlowRelationUtilitites
{
  /**
   * Helper class, don't instantiate.
   */
  private FlowRelationUtilitites( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * An 1D-Element is an Teschke Element, if at least one associated flow-relation is teschke. * TODO move this into the
   * TypeInfo class
   */
  public static boolean isTeschkeElement1D( final IElement1D element1D, final IFlowRelationshipModel model )
  {
    final List<IFE1D2DNode> nodes = element1D.getNodes();
    for( final IFE1D2DNode node : nodes )
    {
      final GM_Position nodePos = node.getPoint().getPosition();
      final IFlowRelationship flowRel = model.findFlowrelationship( nodePos, 0.1 );
      if( flowRel instanceof ITeschkeFlowRelation )
        return true;
    }

    return false;
  }

  public static GM_Position getFlowPositionFromElement( final IFeatureWrapper2 modelElement )
  {
    try
    {
      final GM_Point point = getFlowPointFromElement( modelElement );
      if( point == null )
        return null;

      return point.getPosition();
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    return null;
  }

  private static GM_Point getFlowPointFromElement( final IFeatureWrapper2 modelElement ) throws GM_Exception
  {
    /* Node: return its position */
    if( modelElement instanceof IFELine )
    {
      final IFELine element = (IFELine) modelElement;
      final GM_Curve geom = element.getGeometry();
      return geom == null ? null : geom.getCentroid();
    }

    if( modelElement instanceof IFE1D2DNode )
      return ((IFE1D2DNode) modelElement).getPoint();

    if( modelElement instanceof IFE1D2DElement )
    {
      final IFE1D2DElement element = (IFE1D2DElement) modelElement;
      final GM_Object geom = element.recalculateElementGeometry();
      return geom == null ? null : geom.getCentroid();
    }

    return null;
  }

  public static GM_Position getFlowPositionFromElement( final IFeatureWrapper2 modelElement, final int numberOfPositions, final int currentPosition )
  {
    final List<GM_Point> pointList = new ArrayList<GM_Point>();
    if( numberOfPositions < 2 )
      return getFlowPositionFromElement( modelElement );
    if( modelElement instanceof IFELine )
    {
      try
      {
        final GM_Curve geometry = ((IFELine) modelElement).getGeometry();
        final String coordinateSystem = geometry.getCoordinateSystem();
        final GM_Position[] positions = geometry.getAsLineString().getPositions();
        for( final GM_Position element : positions )
          pointList.add( GeometryFactory.createGM_Point( element, coordinateSystem ) );
      }
      catch( final GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
        return null;
      }
    }
    else
      return null;
    double lineLength = 0.0;
    GM_Point point = null;
    GM_Point previousPoint = null;
    for( int i = 0; i < pointList.size(); i++ )
    {
      point = pointList.get( i );
      if( previousPoint == null )
      {
        previousPoint = point;
        continue;
      }
      lineLength += getAbsoluteDistance( previousPoint, point );
      previousPoint = point;
    }
    double distance = lineLength * Math.abs( currentPosition ) / Math.abs( numberOfPositions + 1 );
    previousPoint = null;
    for( int i = 0; i < pointList.size(); i++ )
    {
      point = pointList.get( i );
      if( previousPoint == null )
      {
        previousPoint = point;
        continue;
      }
      final double segmentLength = getAbsoluteDistance( previousPoint, point );
      if( distance > segmentLength )
      {
        distance -= segmentLength;
        previousPoint = point;
      }
      else if( distance == segmentLength )
      {
        return point.getPosition();
      }
      else
      {
        final double xPos = previousPoint.getX() + (point.getX() - previousPoint.getX()) * distance / segmentLength;
        final double yPos = previousPoint.getY() + (point.getY() - previousPoint.getY()) * distance / segmentLength;
        return GeometryFactory.createGM_Position( xPos, yPos );
      }
    }
    return null;
  }

  private static double getAbsoluteDistance( final GM_Point point1, final GM_Point point2 )
  {
    return Math.sqrt( Math.pow( (point1.getX() - point2.getX()), 2 ) + Math.pow( (point1.getY() - point2.getY()), 2 ) );
  }

  /**
   * Checks if a 1D-Element has an associated Building-Flow-Relation.
   */
  public static IBuildingFlowRelation findBuildingElement1D( final IElement1D element, final IFlowRelationshipModel model )
  {
    final GM_Position flowPosition = getFlowPositionFromElement( element );

    final Class<IFlowRelationshipModel>[] flowRelationTypes = new Class[] { IBuildingFlowRelation.class };

    final IFlowRelationship flowRel = model.findFlowrelationship( flowPosition, 0.01, flowRelationTypes );
    if( flowRel instanceof IBuildingFlowRelation )
      return (IBuildingFlowRelation) flowRel;

    return null;
  }

  private static List<IFlowRelationship> findFlowrelationshipsForPosition( final GM_Position pPosition, final IFlowRelationshipModel model )
  {
    List<IFlowRelationship> lListRes = new ArrayList<IFlowRelationship>();

    // final Class<IFlowRelationshipModel>[] flowRelationTypes = new Class[] { IFlowRelationship.class };

    final IFlowRelationship[] flowRels = model.findFlowrelationships( pPosition, 0.01 );
    for( final IFlowRelationship flowRel : flowRels )
      lListRes.add( flowRel );

    return lListRes;
  }

  /**
   * Checks if a 1D-Element has an associated Buildings-Flow-Relation. returns set with found buildings
   */
  public static Set<IFlowRelationship> findBuildingElements1D( final IElement1D element, final IFlowRelationshipModel model )
  {
    Set<IFlowRelationship> lSetRes = new HashSet<IFlowRelationship>();
    List<GM_Position> lListPositions = new ArrayList<GM_Position>();
    final List nodes = element.getNodes();

    if( nodes.size() < 2 )
    {
      lListPositions.add( getFlowPositionFromElement( element ) );
    }
    else
    {
      lListPositions.add( ((IFE1D2DNode) nodes.get( 0 )).getPoint().getPosition() );
      lListPositions.add( ((IFE1D2DNode) nodes.get( 1 )).getPoint().getPosition() );
    }

    for( final GM_Position lPos : lListPositions )
    {
      lSetRes.addAll( findFlowrelationshipsForPosition( lPos, model ) );
    }
    return lSetRes;
  }

  /**
   * Checks if a 2D-Element has an associated Building-Flow-Relation.
   */
  public static IBuildingFlowRelation2D findBuildingElement2D( final IPolyElement element, final IFlowRelationshipModel model )
  {
    final GM_Position flowPosition = getFlowPositionFromElement( element );

    final Class<IFlowRelationshipModel>[] flowRelationTypes = new Class[] { IBuildingFlowRelation2D.class };

    final IFlowRelationship lFlowRel = model.findFlowrelationship( flowPosition, 0.02, flowRelationTypes );
    if( lFlowRel instanceof IBuildingFlowRelation2D )
      return (IBuildingFlowRelation2D) lFlowRel;

    return null;
  }

  /**
   * Checks if on given position is an associated Building-Flow-Relation.
   */
  public static IFeatureWrapper2 findBuildingElementFromPosition( final GM_Point pPoint, final IFlowRelationshipModel model )
  {
    final Class<IFlowRelationshipModel>[] flowRelationTypes = new Class[] { IFlowRelation2D.class, IFlowRelation1D.class };

    final IFlowRelationship lFlowRel = model.findFlowrelationship( pPoint.getPosition(), 0.02, flowRelationTypes );
    if( lFlowRel != null )
      return lFlowRel;

    return null;
  }

  public static IFE1D2DNode findUpstreamNode( final IBuildingFlowRelation buildingRelation, final IFEDiscretisationModel1d2d discModel )
  {
    return findNeighborNode( buildingRelation, discModel, true );
  }

  public static IFE1D2DNode findNeighborNode( final IBuildingFlowRelation buildingRelation, final IFEDiscretisationModel1d2d discModel, final boolean upstreams )
  {
    /* find element */
    final GM_Point buildingLocation = buildingRelation.getPosition();
    if( buildingLocation == null )
      return null;

    final IElement1D element1d = findBuildingElement1D( buildingRelation, discModel );
    if( element1d == null )
      return null;

    /* find neighbour nodes */
    final List nodes = element1d.getNodes();
    if( nodes.size() != 2 )
      return null;

    final IFE1D2DNode startNode = (IFE1D2DNode) nodes.get( 0 );
    final IFE1D2DNode endNode = (IFE1D2DNode) nodes.get( 1 );

    // find node which is in the right direction
    final GM_Position startPos = startNode.getPoint().getPosition();
    final GM_Position endPos = endNode.getPoint().getPosition();
    final GM_Position buildingPos = buildingLocation.getPosition();

    final double startDirection = GeometryUtilities.directionFromPositions( buildingPos, startPos );
    final double endDirection = GeometryUtilities.directionFromPositions( buildingPos, endPos );
    final int buildingDirection = buildingRelation.getDirection();

    /*
     * Upstreams lies the node for which the direction is WORSE than to the other, because the direction points
     * downstreams.
     */
    if( Math.abs( startDirection - buildingDirection ) < Math.abs( endDirection - buildingDirection ) )
      return upstreams ? endNode : startNode;

    return upstreams ? startNode : endNode;
  }

  public static IElement1D findBuildingElement1D( final IBuildingFlowRelation building, final IFEDiscretisationModel1d2d discModel )
  {
    return discModel.find1DElement( building.getPosition(), 0.01 );
  }

  /**
   * this function decides what node is the upstream node from given nodes:
   * 
   * 0Node ----- 1Node | ^ | | | -+--> v1 | | | 3Node --|-- 2Node v2 in this case is the "3Node" the upstream node and
   * the result will be 2(position in the given list). Important is that the 0-3 and 1-2 edges are the side edges
   * 
   */
  public static int findUpstreamNodePolyWeirPositionInNodesRing( final IFlowRelationship pBuilding, final List<IFE1D2DNode> pListNodes, int pIntDirectionOfEdges )
  {
    if( !(pBuilding instanceof IBuildingFlowRelation2D) )
      return -1;

    int lIntSizeListElementNodes = pListNodes.size();
    int lIntDirectionGrad = ((IBuildingFlowRelation2D) pBuilding).getDirection() % 360;

    double lDoubleXVectorRight = (pListNodes.get( lIntSizeListElementNodes / 2 - 1 ).getPoint().getPosition().getX() - pListNodes.get( lIntSizeListElementNodes / 2 ).getPoint().getPosition().getX()) / 2;
    double lDoubleYVectorRight = (pListNodes.get( lIntSizeListElementNodes / 2 - 1 ).getPoint().getPosition().getY() - pListNodes.get( lIntSizeListElementNodes / 2 ).getPoint().getPosition().getY()) / 2;
    double lDoubleXVectorLeft = (pListNodes.get( 0 ).getPoint().getPosition().getX() - pListNodes.get( lIntSizeListElementNodes - 2 ).getPoint().getPosition().getX()) / 2;
    double lDoubleYVectorLeft = (pListNodes.get( 0 ).getPoint().getPosition().getY() - pListNodes.get( lIntSizeListElementNodes - 2 ).getPoint().getPosition().getY()) / 2;

    double lDoubleXWeirDirectionVector = lDoubleXVectorRight - lDoubleXVectorLeft;
    double lDoubleYWeirDirectionVector = lDoubleYVectorRight - lDoubleYVectorLeft;

    // double lDoubleAngleInBetweenACOS = Math.acos( lDoubleXWeir / Math.sqrt( lDoubleXWeir * lDoubleXWeir +
    // lDoubleYWeir * lDoubleYWeir ) ) * 180 / Math.PI;
    double lDoubleAngleInBetweenATAN = (Math.atan2( lDoubleYWeirDirectionVector, lDoubleXWeirDirectionVector ) - Math.atan2( 0, 1 )) * 180 / Math.PI;

    int lIntDiff = Math.abs( (lIntDirectionGrad - ((int) lDoubleAngleInBetweenATAN)) % 360 );

    if( lIntDiff < 180 )
    {
      return pIntDirectionOfEdges == 1 ? 3 : 1;
    }
    else if( lIntDiff > 180 )
    {
      return pIntDirectionOfEdges == 1 ? 1 : 3;
    }
    return -1;
  }

}
