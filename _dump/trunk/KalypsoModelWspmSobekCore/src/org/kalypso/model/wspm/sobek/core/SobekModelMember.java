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
package org.kalypso.model.wspm.sobek.core;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker;
import org.kalypso.model.wspm.sobek.core.interfaces.ICalculationLink;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.model.wspm.sobek.core.model.BranchMaker;
import org.kalypso.model.wspm.sobek.core.model.Lastfall;
import org.kalypso.model.wspm.sobek.core.pub.FNNodeUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */

public class SobekModelMember implements ISobekModelMember
{

  /**
   * @param workspace
   *            CommandableWorspace instance for posting new feature, updating features, aso
   * @param modelMember
   *            Sobek model member
   * @param reposContainer
   *            Time Series repository container
   */
  public static ISobekModelMember getModel( final CommandableWorkspace workspace, final Feature modelMember ) throws CoreException
  {
    if( workspace == null )
      throw new CoreException( new Status( IStatus.ERROR, SobekModelMember.class.toString(), Messages.SobekModelMember_0 ) );

    return new SobekModelMember( workspace, modelMember );
  }

  private final Feature m_modelMember;

  private final CommandableWorkspace m_workspace;

  protected SobekModelMember( final CommandableWorkspace workspace, final Feature modelMember )
  {
    m_workspace = workspace;

    if( modelMember == null )
      throw new IllegalStateException( Messages.SobekModelMember_2 );

    if( !ISobekConstants.QN_SOBEK_MODEL.equals( modelMember.getFeatureType().getQName() ) )
      throw new IllegalStateException( Messages.SobekModelMember_3 + ISobekConstants.QN_SOBEK_MODEL_MEMBER );

    m_modelMember = modelMember;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#deleteFoo(org.kalypsodeegree.model.feature.Feature)
   */
  public void deleteFoo( final Feature feature ) throws Exception
  {
    final QName qn = feature.getFeatureType().getQName();

    if( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH.equals( qn ) )
      new Branch( this, feature ).delete();
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qn ) )
      FeatureUtils.deleteFeature( m_workspace, feature );
    else
      throw new NotImplementedException();
  }

  public IBoundaryNode[] getBoundaryNodeMembers( )
  {
    final INode[] allNodes = getNodeMembers();
    final List<IBoundaryNode> boundaryCondNodes = new ArrayList<IBoundaryNode>();
    for( final INode node : allNodes )
      if( node instanceof IBoundaryNode )
        boundaryCondNodes.add( (IBoundaryNode) node );
    return boundaryCondNodes.toArray( new IBoundaryNode[] {} );
  }

  public IBoundaryNode[] getBoundaryNodeMembersExceptHQ( )
  {
    final IBoundaryNode[] allNodes = getBoundaryNodeMembers();
    final List<IBoundaryNode> boundaryCondNodes = new ArrayList<IBoundaryNode>();
    for( final IBoundaryNode node : allNodes )
      if( !(node.getBoundaryType().equals( BOUNDARY_TYPE.eWQ )) )
        boundaryCondNodes.add( node );
    return boundaryCondNodes.toArray( new IBoundaryNode[] {} );
  }

  public IBoundaryNode[] getBoundaryNodeMembersHQ( )
  {
    final IBoundaryNode[] allNodes = getBoundaryNodeMembers();
    final List<IBoundaryNode> boundaryCondNodes = new ArrayList<IBoundaryNode>();
    for( final IBoundaryNode node : allNodes )
      if( node.getBoundaryType().equals( BOUNDARY_TYPE.eWQ ) )
        boundaryCondNodes.add( node );
    return boundaryCondNodes.toArray( new IBoundaryNode[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMaker()
   */
  public IBranchMaker getBranchMaker( )
  {
    return new BranchMaker( this );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMembers()
   */
  public IBranch[] getBranchMembers( )
  {
    final List<IBranch> myBranches = new ArrayList<IBranch>();

    final List< ? > branches = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
    for( final Object object : branches )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature branch = (Feature) object;

      final IBranch myBranch = new Branch( this, branch );
      myBranches.add( myBranch );
    }

    return myBranches.toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getCalculationLinkMembers()
   */
  public ICalculationLink[] getCalculationLinkMembers( )
  {
    throw new NotImplementedException();
  }

  /**
   * returns nodes that are somehow connection nodes (connection, linkage, boundary condition)
   */
  public INode[] getConnectionNodeTypeNodeMembers( )
  {

    final INode[] allNodes = getNodeMembers();
    final List<INode> connNodes = new ArrayList<INode>();
    for( final INode node : allNodes )
      if( node instanceof IBoundaryNode || node instanceof IConnectionNode || node instanceof ILinkageNode )
        connNodes.add( node );
    return connNodes.toArray( new INode[] {} );
  }

  /**
   *
   */
  public ICrossSectionNode[] getCrossSectionNodeMembers( )
  {
    final INode[] allNodes = getNodeMembers();
    final List<ICrossSectionNode> crossSectionsNodes = new ArrayList<ICrossSectionNode>();
    for( final INode node : allNodes )
      if( node instanceof ICrossSectionNode )
        crossSectionsNodes.add( (ICrossSectionNode) node );
    return crossSectionsNodes.toArray( new ICrossSectionNode[] {} );
  }

  public Feature getFeature( )
  {
    return m_modelMember;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getLastfallMembers()
   */
  public ILastfall[] getLastfallMembers( )
  {
    final List<ILastfall> myLastfalls = new ArrayList<ILastfall>();

    final List< ? > lastfalls = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_LASTFALL_MEMBER );
    for( final Object object : lastfalls )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature lastfall = (Feature) object;
      myLastfalls.add( new Lastfall( this, lastfall ) );
    }

    return myLastfalls.toArray( new ILastfall[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getNodeMembers()
   */
  public INode[] getNodeMembers( )
  {
    final List<INode> myNodes = new ArrayList<INode>();

    final List< ? > nodes = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    for( final Object object : nodes )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature node = (Feature) object;
      myNodes.add( FNNodeUtils.getNode( this, node ) );
    }

    return myNodes.toArray( new INode[] {} );
  }

  public ISbkStructure[] getSbkStructures( )
  {
    final INode[] allNodes = getNodeMembers();
    final List<ISbkStructure> structureNodes = new ArrayList<ISbkStructure>();
    for( final INode node : allNodes )
      if( node instanceof ISbkStructure )
        structureNodes.add( (ISbkStructure) node );
    return structureNodes.toArray( new ISbkStructure[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getWorkspace()
   */
  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  /**
   *
   */
  public void deleteSbkStructs( ) throws Exception
  {

    final ISbkStructure[] nodes = getSbkStructureTypeNodeMembers();
    for( final ISbkStructure node : nodes )
      node.delete();

    return;
  }

  /**
   * returns all nodes that are somehow SbkStructures
   */
  private ISbkStructure[] getSbkStructureTypeNodeMembers( )
  {

    final INode[] allNodes = getNodeMembers();
    final List<INode> sbkStructNodes = new ArrayList<INode>();
    for( final INode node : allNodes )
      if( node instanceof ISbkStructure )
        sbkStructNodes.add( node );

    return sbkStructNodes.toArray( new ISbkStructure[] {} );
  }

}
