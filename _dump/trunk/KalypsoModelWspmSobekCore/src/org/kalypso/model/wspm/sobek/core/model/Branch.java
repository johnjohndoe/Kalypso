/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IAbstractConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IGmlWorkspaces;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.pub.FNNodeUtils;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypso.model.wspm.sobek.core.utils.ILinkFeatureWrapperDelegate;
import org.kalypso.model.wspm.sobek.core.utils.LinkFeatureWrapper;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author kuch
 */
public class Branch implements IBranch
{
  public static String createBranchId( final IModelMember model )
  {
    int count = 0;

    final IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
    {
      final String branchId = branch.getId();
      if( branchId == null )
        continue;

      final String[] split = branchId.split( "_" ); //$NON-NLS-1$
      if( split.length != 2 )
        throw new IllegalStateException( Messages.Branch_1 );

      final Integer iBranch = new Integer( split[1] );
      if( iBranch > count )
        count = iBranch;
    }

    return String.format( "b_%05d", ++count ); //$NON-NLS-1$
  }

  protected final Feature m_branch;

  protected final IModelMember m_model;

  public Branch( final IModelMember model, final Feature branch )
  {
    m_model = model;
    m_branch = branch;
  }

  /**
   * Delete nofdp branch - a branch consists of a branch ;-) and two connection nodes - remark: a connection node can
   * connected to more than one branch!
   */
  public void delete( ) throws Exception
  {
    // getAllLinkageNodes of branch
    // branch its the only branch link of linkage node?!? if yes -> delete linkage node...
    final List<INode> nodes = new ArrayList<INode>();
    nodes.add( getUpperNode() );
    nodes.add( getLowerNode() );

    for( final INode node : nodes )
      if( node instanceof IAbstractConnectionNode )
      {
        final IAbstractConnectionNode n = (IAbstractConnectionNode) node;
        n.removeBranch( this );
      }

    // deletes empty nodes
    FNGmlUtils.cleanUpNodes( m_model, this );

    // delete branch
    FeatureUtils.deleteFeature( m_model.getWorkspace(), m_branch );
  }

  // $ANALYSIS-IGNORE
  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof IBranch )
    {
      final IBranch branch = (IBranch) obj;
      final Feature feature = branch.getFeature();
      final EqualsBuilder builder = new EqualsBuilder();
      builder.append( getFeature(), feature );

      return builder.isEquals();
    }

    return super.equals( obj );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getDescription()
   */
  public String getDescription( )
  {
    final Object description = m_branch.getProperty( ISobekConstants.QN_HYDRAULIC_DESCRIPTION );
    if( description == null )
      return null;
    return (String) description;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getFeature()
   */
  public Feature getFeature( )
  {
    return m_branch;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getGeometryProperty()
   */
  public GM_Curve getGeometryProperty( )
  {
    return (GM_Curve) m_branch.getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getId()
   */
  public String getId( )
  {
    return (String) m_branch.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getLowerNode()
   */
  public INode getLowerNode( )
  {
    return getNode( ISobekConstants.QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getModelMember()
   */
  public IModelMember getModelMember( )
  {
    return m_model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getName()
   */
  public String getName( )
  {
    return (String) m_branch.getProperty( ISobekConstants.QN_HYDRAULIC_NAME );
  }

  private INode getNode( final QName lnkBranch )
  {
    final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
    {

      public Feature getLinkedFeature( String id )
      {
        INode[] nodes = m_model.getNodeMembers();
        for( INode node : nodes )
        {
          String nodeId = node.getFeature().getId();
          if( nodeId.equals( id ) )
            return node.getFeature();
        }

        return null;
      }

      public Object getProperty( )
      {
        return m_branch.getProperty( lnkBranch );
      }
    };

    final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
    return FNNodeUtils.getNode( m_model, wrapper.getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getUpperNode()
   */
  public INode getUpperNode( )
  {
    return getNode( ISobekConstants.QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return HashCodeBuilder.reflectionHashCode( m_branch );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#setLowerNode(org.kalypso.model.wspm.sobek.core.interfaces.INode)
   */
  public void setLowerNode( final INode node ) throws Exception
  {
    FeatureUtils.updateLinkedFeature( m_model.getWorkspace(), m_branch, ISobekConstants.QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE, IGmlWorkspaces.HYDRAUL_MODEL + "#" + node.getFeature().getId() ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#setUpperNode(org.kalypso.model.wspm.sobek.core.interfaces.INode)
   */
  public void setUpperNode( final INode node ) throws Exception
  {
    FeatureUtils.updateLinkedFeature( m_model.getWorkspace(), m_branch, ISobekConstants.QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE, IGmlWorkspaces.HYDRAUL_MODEL + "#" + node.getFeature().getId() ); //$NON-NLS-1$
  }
}
