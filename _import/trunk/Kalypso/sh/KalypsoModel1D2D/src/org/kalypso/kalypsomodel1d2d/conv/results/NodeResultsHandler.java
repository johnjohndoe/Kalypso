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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.text.DefaultEditorKit.CutAction;
import javax.xml.namespace.QName;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.conv.EReadError;
import org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.ArcResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.ElementResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.SimpleNodeResult;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.schema.WspmUrlCatalog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 */
public class NodeResultsHandler implements IRMA10SModelElementHandler
{
  private final Map<Integer, GMLNodeResult> m_nodeIndex = new HashMap<Integer, GMLNodeResult>();

  private final HashMap<Integer, ArcResult> m_arcIndex = new HashMap<Integer, ArcResult>();

  private final HashMap<Integer, ElementResult> m_elemIndex = new HashMap<Integer, ElementResult>();

  private final GMLWorkspace m_resultWorkspace;

  private final FeatureList m_resultList;

  private final CS_CoordinateSystem m_crs;

  private ITriangleEater m_triangleEater;

  private final IFlowRelationshipModel m_flowRelationModel;

  public NodeResultsHandler( final GMLWorkspace resultWorkspace, final ITriangleEater triangleEater, final IFlowRelationshipModel flowRelationModel )
  {
    m_resultWorkspace = resultWorkspace;
    m_triangleEater = triangleEater;
    m_flowRelationModel = flowRelationModel;
    m_resultList = (FeatureList) m_resultWorkspace.getRootFeature().getProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "nodeResultMember" ) );

    m_crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#getCreatedFeatures()
   */
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  public void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID )
  {
    // to get the model-geometry, we save all arcs in a Hash-Map
    final ArcResult arcResult = new ArcResult( id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
    m_arcIndex.put( id, arcResult );

    /* store the information of the connection between arcs and elements at the element object */
    /* left element */
    writeArcInfoAtElement( elementLeftID, arcResult );
    /* right element */
    writeArcInfoAtElement( elementRightID, arcResult );

    if( middleNodeID == 0 )
      return; // maybe this is a good place to mark the 1d-nodes....

    // get the information which nodes are mid-side nodes
    final INodeResult result = m_nodeIndex.get( middleNodeID );
    if( middleNodeID != -1 )
    {
      result.setMidSide( true );
    }
    else
      result.setMidSide( false );
  }

  private void writeArcInfoAtElement( int elementID, final ArcResult arcResult )
  {
    /* does the element already exist? */
    if( m_elemIndex.containsKey( elementID ) == true )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( elementID );
      // store the arc
      elementResult.setArc( arcResult );
    }
    else
    {
      // create the element
      final ElementResult elementResult = new ElementResult( elementID, 0, 0, 0 );
      // store the arc
      elementResult.setArc( arcResult );
      // store the element
      m_elemIndex.put( elementID, elementResult );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  public void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber )
  {
    // For each element calculate the geometry (elemID, cornernode1, midsidenode1, cornernode2, midsidenode2,

    // cornernode3, midsidenode3, (cornernode4, midsidenode4), element type)
    /* =========== calculate the geometry ============= */

    /*
     * right here, all element objects should have been created by the arcHandler
     * 
     * IT IS NECESSARY, THAT THE ELEMENTS ARE STANDING BELOW THE ARCS IN THE 2D-RESULT FILE!!
     */
    if( id == 12764 )
      id = id;

    if( m_elemIndex.containsKey( id ) == true )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( id );

      // for completion add the additional parameters
      elementResult.setCurrentRougthnessClassID( currentRougthnessClassID );
      elementResult.setPreviousRoughnessClassID( previousRoughnessClassID );
      elementResult.setEleminationNumber( eleminationNumber );

      /* get the number of the nodes of the arcs */
      final int numArcs = elementResult.getNumArcs();

      /* get the first arc */
      ArcResult currentArc = elementResult.getArc( 0 );
      GMLNodeResult nodeDown = m_nodeIndex.get( currentArc.node1ID );
      GMLNodeResult nodeUp = m_nodeIndex.get( currentArc.node2ID );

      // is it a 1d- or 2d-element
      if( currentRougthnessClassID == 89 ) // 1d
      {
        // TODO:
        // - get the profile Curves of the two nodes defining the current element
        GM_Curve nodeCurve1 = null;
        GM_Curve nodeCurve2 = null;
        try
        {
          nodeCurve1 = getCurveForNode( nodeDown );
          nodeCurve2 = getCurveForNode( nodeUp );
        }
        catch( GM_Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        catch( Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }

        if( nodeCurve1 != null && nodeCurve2 != null )
        {

          //

          // - triangulate the curves
          // alle punkte als rand verbinden => rand polygon => triangle => wsptin (Gescheindigkeit ebenso??)=>
          // triangle-eater

        }

      }
      else
      {
        if( currentArc.elementLeftID == id )
        {
          elementResult.setCornerNodes( nodeDown );
          elementResult.setCornerNodes( nodeUp );
        }

        /* element on the right side of the arc */
        else
        {
          elementResult.setCornerNodes( nodeUp );
          elementResult.setCornerNodes( nodeDown );
        }

        /* midside node of the current arc */
        GMLNodeResult midsideNode = m_nodeIndex.get( currentArc.middleNodeID );
        checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
        elementResult.setMidsideNodes( midsideNode );

        nodeDown.setArc( currentArc );
        currentArc.setNodeDown( nodeDown );

        nodeUp.setArc( currentArc );
        currentArc.setNodeUp( nodeUp );

        /**
         * For the other arcs, just save one corner node.<br>
         * This corner node has to be the opposite node of the connectionNode (last saved corner node)<br>
         * and must lie on the same arc (connected by the same arc).<br>
         * get the current arc of the element (arc)<br>
         * check the orientation of the arc (element is left or right-sided)<br>
         * check the connection. <br>
         */

        for( int pointIndex = 1; pointIndex < numArcs; pointIndex++ )
        {
          final GMLNodeResult connectionNode = (GMLNodeResult) elementResult.getCornerNodes( pointIndex );
          for( int arc = 1; arc < numArcs; arc++ )
          {

            currentArc = elementResult.getArc( arc );

            final int elementLeftID = currentArc.elementLeftID;
            final int elementRightID = currentArc.elementRightID;
            nodeDown = m_nodeIndex.get( currentArc.node1ID );
            nodeUp = m_nodeIndex.get( currentArc.node2ID );

            /* element on the left side of the arc */
            midsideNode = m_nodeIndex.get( currentArc.middleNodeID );
            if( elementLeftID == id && nodeDown.equals( connectionNode ) )
            {
              if( elementResult.getNumCornerNodes() < numArcs )
              {
                elementResult.setCornerNodes( nodeUp );
              }

              nodeUp.setArc( currentArc );
              currentArc.setNodeUp( nodeUp );

              connectionNode.setArc( currentArc );
              currentArc.setNodeDown( connectionNode );

              checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
              elementResult.setMidsideNodes( midsideNode );
              break;
            }
            /* element on the right side of the arc */
            else if( elementRightID == id && nodeUp.equals( connectionNode ) )
            {
              if( elementResult.getNumCornerNodes() < numArcs )
              {
                elementResult.setCornerNodes( nodeDown );
              }
              nodeDown.setArc( currentArc );
              currentArc.setNodeDown( nodeDown );

              connectionNode.setArc( currentArc );
              currentArc.setNodeUp( connectionNode );

              checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
              elementResult.setMidsideNodes( midsideNode );
              break;
            }
          }
        }

        /* check the element (number of arcs, nodes, mid-side nodes) */

        /* check water levels for dry nodes */
        elementResult.checkWaterlevels();

        /* split element into triangles if there is a wet node. */
        if( elementResult.isWet == true )
        {
          /* create the center node */
          elementResult.createCenterNode();

          /* split the element */
          splitElement( elementResult );
        }
      }

// Triangle t;
//      
// m_triangleEater.add(NodeResult n1, n2, n3, elt e);
    }
    else
    {
      System.out.println( "element does not exist in the arc list, check model!" );
    }
    // m_triangleEater.finished();
  }

  /**
   * returns a simplified profile curve of a 1d-node, already cut at the intersection points with the water level
   * 
   * @param nodeResult
   *            1d-node
   * 
   */
  private GM_Curve getCurveForNode( final INodeResult nodeResult ) throws Exception
  {
    final GM_Position nodePos = nodeResult.getPoint().getPosition();
    final double waterlevel = nodeResult.getWaterlevel();
    final IFlowRelationship flowRelationship = m_flowRelationModel.findFlowrelationship( nodePos, 0.0 );
    if( flowRelationship instanceof ITeschkeFlowRelation )
    {
      final ITeschkeFlowRelation teschkeRelation = (ITeschkeFlowRelation) flowRelationship;
// teschkeRelation.getPolynomials();

      final WspmProfile profile = teschkeRelation.getProfile();

      /* cut the profile at the intersection points with the water level */
      // get the intersection points
      final GM_Point[] points = WspmProfileHelper.calculateWspPoints( profile.getProfil(), waterlevel );
      IProfil cutProfile = null;

      if( points != null )
      {
        if( points.length > 1 )
        {
          cutProfile = WspmProfileHelper.cutIProfile( profile.getProfil(), points[0], points[points.length - 1] );
        }
      }
      final CS_CoordinateSystem crs = nodeResult.getPoint().getCoordinateSystem();
      final GM_Curve curve = ProfilUtil.getLine( cutProfile, crs );

      /* simplify the profile */
      final double epsThinning = 0.05;
      GM_Curve thinnedCurve = GeometryUtilities.getThinnedCurve( curve, epsThinning );

      /* set the water level as new z-coordinate of the profile line */
      return GeometryUtilities.setValueZ( thinnedCurve.getAsLineString(), waterlevel );
    }
    // TODO: King

    return null;
  }

  /**
   * Splits the element into triangles.
   * 
   * There are 6 / 8 triangles to be built. For each midside node there are two triangles to be built. <br>
   * We begin with the first corner node, the first midside node and the center node.<br>
   * The second triangle is the second corner node, the first midside node and the center node.<br>
   */
  private void splitElement( ElementResult elementResult )
  {
    final int numMidsideNodes = elementResult.getNumMidsideNodes();

    for( int i = 0; i < numMidsideNodes; i++ )
    {
      // First triangle
      final List<INodeResult> nodeList = new LinkedList<INodeResult>();

      final INodeResult[] nodes = new INodeResult[3];

      nodes[0] = elementResult.getCornerNodes( i );
      nodes[1] = elementResult.getMidsideNodes( i );
      nodes[2] = elementResult.getCenterNode();

      if( checkIfTriangleWet( nodes ) == true )
      {

// int orient = checkOrientation( nodes );

// if( orient == 1 )
// {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );
// }
// else
// {
// nodeList.add( nodes[0] );
// nodeList.add( nodes[2] );
// nodeList.add( nodes[1] );
// }

        /* check, if the triangle is partially flooded */
        if( checkPartiallyFlooded( nodeList ) == true )
        {
          /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
          splitTriangle( nodeList );
          nodeList.clear();
        }
        else
        {
          m_triangleEater.add( nodeList );
          nodeList.clear();
        }
      }

      // second triangle
      nodes[0] = elementResult.getMidsideNodes( i );
      if( i < numMidsideNodes - 1 )
        nodes[1] = elementResult.getCornerNodes( i + 1 );
      else
        nodes[1] = elementResult.getCornerNodes( 0 );
      nodes[2] = elementResult.getCenterNode();

      if( checkIfTriangleWet( nodes ) == true )
      {

//      
// orient = checkOrientation( nodes );
//
// if( orient == 1 )
// {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );
// }
// else
// {
// nodeList.add( nodes[0] );
// nodeList.add( nodes[2] );
// nodeList.add( nodes[1] );
// }

/*
 * nodeList.add( elementResult.getMidsideNodes( i ) ); if( i < numMidsideNodes - 1 ) nodeList.add(
 * elementResult.getCornerNodes( i + 1 ) ); else nodeList.add( elementResult.getCornerNodes( 0 ) );
 * 
 * nodeList.add( elementResult.getCenterNode() );
 */
        /* check, if the triangle is partially flooded */
        if( checkPartiallyFlooded( nodeList ) == true )
        {
          /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
          splitTriangle( nodeList );
          nodeList.clear();
        }
        else
        {
          m_triangleEater.add( nodeList );
          nodeList.clear();
        }
      }
    }
  }

  private boolean checkIfTriangleWet( INodeResult[] nodes )
  {
    for( INodeResult node : nodes )
    {
      if( node.isWet() == true )
        return true;
    }
    return false;
  }

  // determine orientation based on the signed area
  private int checkOrientation( INodeResult[] nodes )
  {
    double signedArea = getSignedArea( nodes );
    if( signedArea > 0.0 )
      return 1;
    if( signedArea < 0.0 )
      return -1;
    return 0;
  }

  private double getSignedArea( INodeResult[] nodes )
  {
    // calculate the signed area
    GM_Point a = nodes[0].getPoint();
    GM_Point b = nodes[1].getPoint();
    GM_Point c = nodes[2].getPoint();

    final double signedArea = 0.5 * (a.getX() * (b.getY() - c.getY()) + b.getX() * (c.getY() - a.getY()) + c.getX() * (a.getY() - b.getY()));

    return signedArea;

  }

  /**
   * checks if the triangle has wet and dry nodes.
   */
  private boolean checkPartiallyFlooded( final List<INodeResult> nodes )
  {
    boolean wet = false;
    boolean dry = false;

    for( final INodeResult node : nodes )
    {
      if( node.getDepth() <= 0 )
        dry = true;
      else
        wet = true;
    }

    if( wet == true && dry == true )
      return true;
    else
      return false;
  }

  /**
   * Splits the triangle into sub-triangles. At first, the arcs to split get identified and the split nodes are added to
   * the node list
   * 
   * @param nodes
   *            node list of the triangle to split. In this list the split nodes are added.
   */
  private void splitTriangle( final List<INodeResult> nodes )
  {

    final List<Integer> splitArcs = new LinkedList<Integer>();

    // check the arcs
    if( checkTriangleArc( nodes.get( 0 ), nodes.get( 1 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 0 ), nodes.get( 1 ) );
      if( addedNode != null )
      {
        splitArcs.add( 0 );
        nodes.add( addedNode );
      }
    }

    if( checkTriangleArc( nodes.get( 1 ), nodes.get( 2 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 1 ), nodes.get( 2 ) );
      if( addedNode != null )
      {
        splitArcs.add( 1 );
        nodes.add( addedNode );
      }
    }
    if( checkTriangleArc( nodes.get( 2 ), nodes.get( 0 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 2 ), nodes.get( 0 ) );
      if( addedNode != null )
      {
        nodes.add( addedNode );
        splitArcs.add( 2 );
      }
    }

    switch( splitArcs.size() )
    {
      /* case1: one arc is split */
      case 1:
        splitIntoTwoTriangles( nodes, splitArcs.get( 0 ) );
        break;

      /* case2: two arcs were split */
      case 2:
        splitIntoThreeTriangles( nodes, splitArcs.get( 0 ), splitArcs.get( 1 ) );
        break;

      default:
        break;
    }
  }

  private void splitIntoThreeTriangles( final List<INodeResult> nodes, final Integer arc1, final Integer arc2 )
  {
    if( arc1 == 0 && arc2 == 1 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node1 and node2
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        if( checkPartiallyFlooded( triNodes ) == true )
          m_triangleEater.add( triNodes );
      }
      else if( nodes.get( 0 ).isWet() == true && nodes.get( 2 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node1, node2, inserted node0 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes );

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, inserted node0, node2 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes );
      }
    }
    else if( arc1 == 0 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 0 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node0 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 0 ) );

        m_triangleEater.add( triNodes );
      }
      else if( nodes.get( 1 ).isWet() == true && nodes.get( 2 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, node1, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes );

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node2, inserted node1, node1 */
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes );
      }
    }
    else if( arc1 == 1 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node1 and node2 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 2 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle0: inserted node0, node2, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes );
      }
      else if( nodes.get( 0 ).isWet() == true && nodes.get( 1 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node1, node1, inserted node0 */
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes );

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, node1, inserted node2 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes );
      }
      else
      {
        System.out.println( "Fehler in splitIntoThreeTriangles: Dreieck wurde nicht gesplittet" );
      }
    }
  }

  /**
   * One corner node is part of the inundation line and one arc is split => two triangles.<br>
   * There are three possibilities depending on the split arc.<br> - the inserted node lies between node0 and node1.<br> -
   * the inserted node lies between node1 and node2.<br> - the inserted node lies between node2 and node0.<br>
   * 
   * @param nodes
   *            list of all nodes of the triangle to split
   * @param splitArcs
   *            list of the split arc numbers (here just one)
   * 
   */
  private void splitIntoTwoTriangles( final List<INodeResult> nodes, final Integer splitArc )
  {

    if( splitArc == 0 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: inserted node0, node1, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes );
      }

      /* triangle2: inserted node0, node2, node0 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 0 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 0 ) );

        m_triangleEater.add( triNodes );
      }

    }
    else if( splitArc == 1 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: node0, node1, inserted node0 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes );
      }

      /* triangle2: node0, inserted node0, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 2 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes );
      }
    }
    else if( splitArc == 2 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: inserted node0, node0, node1 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 0 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes );
      }

      /* triangle2: inserted node0, node1, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 2 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes );
      }
    }
    else
    {
      System.out.println( "Fehler in splitIntoTwoTriangles: Dreieck wurd nicht gesplittet" );
    }
  }

  private INodeResult insertNode( INodeResult node1, INodeResult node2 )
  {
    final INodeResult node3 = new SimpleNodeResult();

    final double wspNode1 = node1.getWaterlevel();
    final double zNode1 = node1.getPoint().getZ();
    final double wspNode2 = node2.getWaterlevel();
    final double zNode2 = node2.getPoint().getZ();

    final double dist12 = node2.getPoint().distance( node1.getPoint() );
    final double dz1;
    final double dz2;

    if( (wspNode1 - zNode1) > 0 ) // the same as node1.isWet() == true;
    {
      if( wspNode1 > zNode2 )
      {
        dz1 = wspNode1 - zNode1;
        dz2 = wspNode2 - zNode2;
      }
      else
      {
        dz1 = wspNode1 - zNode1;
        dz2 = wspNode1 - zNode2;
      }
    }
    else if( (wspNode1 - zNode1) < 0 )
    {
      if( wspNode2 > zNode1 )
      {
        dz1 = zNode1 - wspNode1;
        dz2 = zNode2 - wspNode2;
      }
      else
      {
        dz1 = zNode1 - wspNode2;
        dz2 = zNode2 - wspNode2;
      }
    }
    else
      // dz1 = 0
      return null;

    // => dz1 always > 0 and dz2 always < 0 !!

    // getting the zero point
    final double dist13 = getZeroPoint( dist12, dz1, dz2 );

    // getting the z-value for the zero point
    final double z3;

    if( node1.isWet() == true )
      z3 = node1.getWaterlevel();
    else
      z3 = node2.getWaterlevel();

    // getting the geo-coordinates of the zero-point
    final double dx12 = node2.getPoint().getX() - node1.getPoint().getX();
    final double dy12 = node2.getPoint().getY() - node1.getPoint().getY();

    final double x3 = node1.getPoint().getX() + (dist13 / dist12) * dx12;
    final double y3 = node1.getPoint().getY() + (dist13 / dist12) * dy12;

    final CS_CoordinateSystem crs = node1.getPoint().getCoordinateSystem();

    node3.setLocation( x3, y3, z3, crs );
    node3.setResultValues( 0, 0, 0, z3 );

    return node3;

  }

  /**
   * gets the x-coordinate of the zero point of a line defined by y1 (>0), y2 (<0) and the difference of the
   * x-coordinates (x2-x1) = dx12.
   * 
   * @param dx12
   *            distance between x1 and x2.
   * @param y1
   *            the y-value of point 1 of the line. It has to be always > 0!
   * @param y2
   *            the y-value of point 2 of the line. It has to be always < 0!
   */
  private double getZeroPoint( final double dx12, final double y1, final double y2 )
  {
    final double x3 = y1 / (Math.abs( y1 ) + Math.abs( y2 )) * dx12;

    // check: y3Temp is the y-value of the zero point and should always be zero !!
    final double y3Temp = Math.round( (-(Math.abs( y1 ) + Math.abs( y2 )) / dx12 * x3 + y1) * 100 ) / 100;

    if( y3Temp != 0.00 )
      System.out.println( "error while interpolation node-data, y3Temp != 0 = " + y3Temp );

    return x3;
  }

  private boolean checkTriangleArc( final INodeResult node1, final INodeResult node2 )
  {
    /* get the spit point (inundation point) */
    if( (node1.isWet() == true && node2.isWet() == false) || (node1.isWet() == false && node2.isWet() == true) )
      return true;
    else
      return false;
  }

  /**
   * sets the mid-side node's water level and depth by interpolation between the corner nodes.
   * 
   * @param nodeDown
   *            first node of the corresponding arc.
   * @param nodeUp
   *            second node of the corresponding arc.
   * @param midsideNode
   *            the mid-side node
   */
  private void checkMidsideNodeData( final GMLNodeResult nodeDown, final GMLNodeResult nodeUp, final GMLNodeResult midsideNode )
  {
    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() <= 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
// midsideNode.setResultValues( 0, 0, 0, midsideNode.getPoint().getZ() - 1 );
      return;
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() <= 0 )
    {

      assignMidsideNodeData( nodeDown, midsideNode ); // assignment leads into extrapolation of the water level!!
      // interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }
    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() > 0 )
    {
      assignMidsideNodeData( nodeUp, midsideNode ); // assignment leads into extrapolation of the water level!!
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() > 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }
  }

  /**
   * interpolates the water level for the midside node by using the water levels of the corner nodes. The depth will be
   * calculated as well, using the interpolated water level.
   * 
   * @param nodeDown
   *            first node of the arc on which the corner node lies.
   * @param nodeUp
   *            second node
   * @param midsideNode
   *            the midside node
   */
  private void interpolateMidsideNodeData( GMLNodeResult nodeDown, GMLNodeResult nodeUp, GMLNodeResult midsideNode )
  {
    final List<Double> waterlevels = new LinkedList<Double>();
    final List<Double> depths = new LinkedList<Double>();

    waterlevels.add( nodeDown.getWaterlevel() );
    waterlevels.add( nodeUp.getWaterlevel() );

    depths.add( nodeDown.getDepth() );
    depths.add( nodeUp.getDepth() );

    final double waterlevel = interpolate( waterlevels );
    midsideNode.setWaterlevel( waterlevel );
    // midsideNode.setDepth( interpolate( depths ) );
    final double depth = waterlevel - midsideNode.getPoint().getZ();
    if( depth < 0 )
      midsideNode.setDepth( 0 );
    else
      midsideNode.setDepth( depth );
  }

  private double interpolate( List<Double> values )
  {
    double sum = 0;
    for( int i = 0; i < values.size(); i++ )
    {
      sum = sum + values.get( i );
    }
    return (sum / values.size());
  }

  private void assignMidsideNodeData( final GMLNodeResult node, final GMLNodeResult midsideNode )
  {
    final double waterlevel = node.getWaterlevel();
    midsideNode.setWaterlevel( waterlevel );

    final double depth = waterlevel - midsideNode.getPoint().getZ();
    if( depth < 0 )
      midsideNode.setDepth( 0 );
    else
      midsideNode.setDepth( depth );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation )
  {
    final Feature parentFeature = m_resultList.getParentFeature();
    final IRelationType parentRelation = m_resultList.getParentFeatureTypeProperty();

    try
    {
      /* Create new Node-Result */
      final Feature feature = m_resultWorkspace.createFeature( parentFeature, parentRelation, parentRelation.getTargetFeatureType() );
      m_resultWorkspace.addFeatureAsComposition( parentFeature, parentRelation, -1, feature );

      /* Remember node result for additional result data */
      final GMLNodeResult result = new GMLNodeResult( feature );
      m_nodeIndex.put( id, result );

      /* Fill node result with data */
      result.setName( "" + id );
// if(id == 71332)
// id = id;

// System.out.println( "node id: " + id );

      // TODO: description: beschreibt, welche Rechenvariante und so weiter... oder noch besser an der collection
// result.setDescription( "" + id );

      result.setCalcId( id );
      result.setLocation( easting, northing, elevation, m_crs );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handlerError( String lineString, EReadError errorHints )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( String lineString )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( IModelElementIDProvider modelElementIDProvider ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  public void handleResult( String lineString, int id, double vx, double vy, double virtualDepth, double waterlevel )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      System.out.println( "Result for non-existing node: " + id );
      return;
    }

    result.setResultValues( vx, vy, virtualDepth, waterlevel );
  }

}
