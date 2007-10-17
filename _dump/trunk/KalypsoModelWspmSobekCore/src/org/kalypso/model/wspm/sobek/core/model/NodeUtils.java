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
package org.kalypso.model.wspm.sobek.core.model;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.INodeUtils;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class NodeUtils implements INodeUtils
{
  private final ISobekModelMember m_model;

  public NodeUtils( ISobekModelMember model )
  {
    m_model = model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INodeUtils#switchBoundaryConnectionNode(org.kalypsodeegree.model.feature.Feature)
   */
  public void switchBoundaryConnectionNode( Feature node ) throws Exception
  {
    QName nqn = node.getFeatureType().getQName();

    /* which node type? */
    if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( nqn ) )
      connectionNodeToBoundaryNode( new ConnectionNode( m_model, node ) );
    else if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( nqn ) )
      boundaryNodeToConnectionNode( node );
    else
      throw (new NotImplementedException());
  }

  private void boundaryNodeToConnectionNode( Feature node )
  {
    throw (new NotImplementedException());
  }

  private void connectionNodeToBoundaryNode( IConnectionNode cn ) throws Exception
  {
    /* create new boundary node */
    INode boundaryNode = FNGmlUtils.createNode( m_model, TYPE.eBoundaryNode, cn.getGeometry(), new INode[] {} );

    Map<QName, Object> map = new HashMap<QName, Object>();
    map.put( ISobekConstants.QN_HYDRAULIC_NAME, cn.getName() );
    map.put( ISobekConstants.QN_HYDRAULIC_DESCRIPTION, cn.getDescription() );

    FeatureUtils.updateFeature( boundaryNode.getFeature(), map );

    for( IBranch branch : cn.getInflowingBranches() )
    {
      boundaryNode.addInflowingBranch( branch );
    }

    for( IBranch branch : cn.getOutflowingBranches() )
    {
      boundaryNode.addOutflowingBranch( branch );
    }

    // set node at branches
    IBranch[] inflowing = boundaryNode.getInflowingBranches();
    for( IBranch branch : inflowing )
    {
      updateBranchNode( branch, boundaryNode );
    }

    IBranch[] outflowing = boundaryNode.getOutflowingBranches();
    for( IBranch branch : outflowing )
    {
      updateBranchNode( branch, boundaryNode );
    }

    // delete connection node
    cn.delete();
  }

  private void updateBranchNode( IBranch branch, INode node ) throws Exception
  {
    GM_Curve curve = branch.getGeometryProperty();
    GM_Point pn = node.getGeometry();

    if( curve.getStartPoint().intersects( pn ) )
    {
      branch.setUpperNode( node );
    }
    else if( curve.getEndPoint().intersects( pn ) )
    {
      branch.setLowerNode( node );
    }
  }

}
