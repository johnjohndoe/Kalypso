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
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.conv.EReadError;
import org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.ArcResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.ElementResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.NodeResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 */
public class NodeResultsHandler implements IRMA10SModelElementHandler
{
  private final Map<Integer, NodeResult> m_nodeIndex = new HashMap<Integer, NodeResult>();

  private final HashMap<Integer, ArcResult> m_arcIndex = new HashMap<Integer, ArcResult>();

  private final HashMap<Integer, ElementResult> m_elemIndex = new HashMap<Integer, ElementResult>();

  private final GMLWorkspace m_resultWorkspace;

  private final FeatureList m_resultList;

  private final CS_CoordinateSystem m_crs;

  public NodeResultsHandler( final GMLWorkspace resultWorkspace /*,final ITriangleEater triangleEater*/ )
  {
    m_resultWorkspace = resultWorkspace;
    m_resultList = (FeatureList) m_resultWorkspace.getRootFeature().getProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "nodeResultMember" ) );

    m_crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    //m_triangleEater = triangleEater;
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
      elementResult.setNewArc( arcResult );
    }
    else
    {
      // create the element
      final ElementResult elementResult = new ElementResult( elementID, 0, 0, 0 );
      // store the arc
      elementResult.setNewArc( arcResult );
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

    /* calculate the geometry */

    // TODO: Unterscheidung Mittseitenknoten und Eckknoten (separate Liste)??
    /* right here, all element objects should have been created by the arcHandler */
    if( m_elemIndex.containsKey( id ) == true )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( id );

      // for completion add the additional parameters
      elementResult.setCurrentRougthnessClassID( currentRougthnessClassID );
      elementResult.setPreviousRoughnessClassID( previousRoughnessClassID );
      elementResult.setEleminationNumber( eleminationNumber );

      /* get the nodes of the arcs */
      final int numArcs = elementResult.getNumArcs();

      /**
       * the first arc of the element: for the f irst arc, both corner nodes are being saved
       */

      /* element on the left side of the arc */
      if( elementResult.getArc( 0 ).elementLeftID == id )
      {
        elementResult.setCornerNodes( m_nodeIndex.get( elementResult.getArc( 0 ).node1ID ) );
        elementResult.setCornerNodes( m_nodeIndex.get( elementResult.getArc( 0 ).node2ID ) );
      }

      /* element on the right side of the arc */
      else
      {
        elementResult.setCornerNodes( m_nodeIndex.get( elementResult.getArc( 0 ).node2ID ) );
        elementResult.setCornerNodes( m_nodeIndex.get( elementResult.getArc( 0 ).node1ID ) );
      }

      /* midside node of the current arc */
      elementResult.setMidsideNodes( m_nodeIndex.get( elementResult.getArc( 0 ).middleNodeID ) );

      /**
       * for the rest of the arcs of the element: for the other arcs, just save one corner.<br>
       * This corner is the opposite corner to the connectionNode<br>
       * 
       * get the current arc of the element (arc)<br>
       * check the orientation of the arc (element is left or right-sided)<br>
       * check the connection. <br>
       */

      for( int pointIndex = 1; pointIndex < numArcs; pointIndex++ )
      {
        final NodeResult connectionNode = (NodeResult) elementResult.getCornerNodes( pointIndex );
        for( int arc = 1; arc < numArcs; arc++ )
        {

          final ArcResult arcdata = elementResult.getArc( arc );

          final int elementLeftID = arcdata.elementLeftID;
          final int elementRightID = arcdata.elementRightID;
          final NodeResult nodeResultDown = m_nodeIndex.get( arcdata.node1ID );
          final NodeResult nodeResultUp = m_nodeIndex.get( arcdata.node2ID );

          /* element on the left side of the arc */
          if( elementLeftID == id && nodeResultDown.equals( connectionNode ) )
          {
            if( elementResult.getNumCornerNodes() < numArcs )
              elementResult.setCornerNodes( nodeResultUp );
            elementResult.setMidsideNodes( m_nodeIndex.get( arcdata.middleNodeID ) );
            break;
          }
          /* element on the right side of the arc */
          else if( elementRightID == id && nodeResultUp.equals( connectionNode ) )
          {
            if( elementResult.getNumCornerNodes() < numArcs )
              elementResult.setCornerNodes( nodeResultDown );
            elementResult.setMidsideNodes( m_nodeIndex.get( arcdata.middleNodeID ) );
            break;
          }
        }
      }

      /* check the element (number of arcs, nodes, midside nodes) */
// System.out.println( "Element: " + id + "#nodes: " + elementResult.getNumCornerNodes() + " " +
// elementResult.getNumMidsideNodes() );
      /* get the center node */
      final NodeResult centerNode = (NodeResult) elementResult.createCenterNode();

      /* split element into triangles. */

//      Triangle t;
//      
//      m_triangleEater.add(NodeResult n1, n2, n3, elt e);
      
    }
    else
    {
      // TODO: error message, that element does not exist in the arc list.
    }

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
      final NodeResult result = new NodeResult( feature );
      m_nodeIndex.put( id, result );

      /* Fill node result with data */
      result.setName( "" + id );
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
  public void handleResult( String lineString, int id, double vx, double vy, double depth, double waterlevel )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      System.out.println( "Result for non-existing node: " + id );
      return;
    }

    result.setResultValues( vx, vy, depth, waterlevel );
  }

}
