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
  
  private final GMLWorkspace m_resultWorkspace;
  private final FeatureList m_resultList;

  private final CS_CoordinateSystem m_crs;

  public NodeResultsHandler( final GMLWorkspace resultWorkspace )
  {
    m_resultWorkspace = resultWorkspace;
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
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int, int, int)
   */
  public void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID )
  {
    //TODO: get the information which nodes are mid-side nodes
    final NodeResult result = m_nodeIndex.get( middleNodeID );
    if ( middleNodeID != -1 )
    {
      result.setMidSide( true );
    }
    else
      result.setMidSide( false );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int, int)
   */
  public void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double, double)
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation )
  {
    final Feature parentFeature = m_resultList.getParentFeature();
    final IRelationType parentRelation = m_resultList.getParentFeatureTypeProperty();
    
    try
    {
      /* Create new Node-Result*/
      final Feature feature = m_resultWorkspace.createFeature( parentFeature, parentRelation, parentRelation.getTargetFeatureType() );
      m_resultWorkspace.addFeatureAsComposition( parentFeature, parentRelation, -1, feature );
      
      
      /* Remember node result for additional result data */
      final NodeResult result = new NodeResult(feature);
      m_nodeIndex.put( id, result );
      
      /* Fill node result with data */
      result.setName( "" + id );
      // TODO: description: beschriebt, welche rechenvariante und so weiter... oder noch besser an fder collection
//      result.setDescription( "" + id );
      
      result.setCalcId( id );
      result.setLocation( easting, northing, elevation, m_crs );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String, org.kalypso.kalypsomodel1d2d.conv.EReadError)
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
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double, double, double, double)
   */
  public void handleResult( String lineString, int id, double vx, double vy, double depth, double waterlevel )
  {
    final NodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      System.out.println( "Result for non-existing node: " + id );
      return;
    }
    
    result.setResultValues( vx, vy, depth, waterlevel );
  }
  
}
