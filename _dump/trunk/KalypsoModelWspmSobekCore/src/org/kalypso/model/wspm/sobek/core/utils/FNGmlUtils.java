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
package org.kalypso.model.wspm.sobek.core.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IGmlWorkspaces;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.model.AbstractNode;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypso.model.wspm.sobek.core.Messages;

/**
 * @author kuch
 */
public class FNGmlUtils
{
  private static void addBranchesToLinkToNodes( final IModelMember model, final INode[] nodes ) throws Exception
  {
    final IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
    {
      final INode branchUpperNode = branch.getUpperNode();
      final INode branchLowerNode = branch.getLowerNode();

      /* set inflowing and outflowing branches */
      if( branchUpperNode instanceof IConnectionNode )
      {
        final IConnectionNode cn = (IConnectionNode) branchUpperNode;

        if( ArrayUtils.contains( nodes, branchUpperNode ) )
          cn.addOutflowingBranch( branch );
      }

      if( branchLowerNode instanceof IConnectionNode )
      {
        final IConnectionNode cn = (IConnectionNode) branchLowerNode;

        if( ArrayUtils.contains( nodes, branchLowerNode ) )
          cn.addInflowingBranch( branch );
      }

    }

    /* node is an linkage node? set linkToBranch (ln lays on branch x - lnk to this branch!) */
    for( final INode node : nodes )
    {
      if( !(node instanceof ILinkageNode) )
        continue;

      final ILinkageNode ln = (ILinkageNode) node;
      ln.setLinkToBranch( branches );

    }
  }

  /**
   * removes all empty nodes
   */
  public static void cleanUpNodes( final IModelMember model, final IBranch branch ) throws Exception
  {
    final INode[] nodes = model.getNodeMembers();
    for( final INode node : nodes )
      if( node.isEmpty() )
        node.delete();
  }

  public static void connectBranches( final IModelMember model, final IBranch[] branches, final GM_Curve curve ) throws Exception
  {
    final List<INode> nodes = new ArrayList<INode>();

    for( final IBranch branch : branches )
    {
      nodes.add( branch.getUpperNode() );
      nodes.add( branch.getLowerNode() );
    }

    FNGmlUtils.createBranch( model, curve, nodes.toArray( new INode[] {} ), TYPE.eConnectionNode, TYPE.eConnectionNode );
  }

  // $ANALYSIS-IGNORE
  /**
   * @param curve
   *            geometry of branch
   * @param nodes
   *            already existing nodes, needed for extending branch (createBranch will extend branches, too!)
   * @param upperNodeType
   *            upper node type of branch
   * @param lowerNodeType
   *            lower node type of branch
   */
  public static INode[] createBranch( final IModelMember model, final GM_Curve curve, final INode[] nodes, final TYPE upperNodeType, final TYPE lowerNodeType ) throws Exception
  {
    if( curve.getAsLineString().getNumberOfPoints() < 2 )
      throw new IllegalStateException( Messages.FNGmlUtils_0 );

    /* create linkage nodes at start and end position of linestring */
    final IGMLSchema schema = model.getFeature().getFeatureType().getGMLSchema();

    // start and end linkage node
    final INode upperNode = FNGmlUtils.createNode( model, upperNodeType, curve.getStartPoint(), nodes );
    final INode lowerNode = FNGmlUtils.createNode( model, lowerNodeType, curve.getEndPoint(), nodes );

    final CommandableWorkspace workspace = model.getWorkspace();
    final IFeatureType ftBranch = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH );

    final IRelationType rtBranchMember = (IRelationType) model.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( workspace, ftBranch, model.getFeature(), rtBranchMember, -1, null, selectionManager );
    workspace.postCommand( command );

    try
    {
      final Feature branch = command.getNewFeature();
      final String id = Branch.createBranchId( model );

      final Map<QName, Object> values = new HashMap<QName, Object>();
      values.put( ISobekConstants.QN_HYDRAULIC_BRANCH_RIVER_LINE, curve );
      values.put( ISobekConstants.QN_HYDRAULIC_BRANCH_LENGTH, curve.getLength() );
      values.put( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID, id );
      values.put( ISobekConstants.QN_HYDRAULIC_NAME, id );

      FeatureUtils.updateFeature( workspace, branch, values );

      final IBranch myBranch = new Branch( model, branch );
      myBranch.setUpperNode( upperNode );
      myBranch.setLowerNode( lowerNode );

      FNGmlUtils.addBranchesToLinkToNodes( model, new INode[] { upperNode, lowerNode } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return new INode[] { upperNode, lowerNode };
  }

  public static void createInflowBranch( final IModelMember model, final IBranch branch, final GM_Curve curve ) throws Exception
  {
    final INode[] nodes = new INode[] { branch.getUpperNode(), branch.getLowerNode() };
    FNGmlUtils.createBranch( model, curve, nodes, TYPE.eConnectionNode, TYPE.eLinkageNode );
  }

  public static INode createNode( final IModelMember model, final TYPE nodeType, final GM_Point point, final INode[] nodes ) throws Exception
  {
    // a new node must be created?!?
    for( final INode node : nodes )
    {
      final GM_Point pNode = node.getLocation();
      if( pNode.intersects( point ) )
        return node;
    }

    final INode node = AbstractNode.createNode( model, nodeType, point );
    return node;
  }

  public static void createOutflowBranch( final IModelMember model, final IBranch branch, final GM_Curve curve ) throws Exception
  {
    final INode[] nodes = new INode[] { branch.getUpperNode(), branch.getLowerNode() };
    FNGmlUtils.createBranch( model, curve, nodes, TYPE.eLinkageNode, TYPE.eConnectionNode );
  }

  public static void createProfileNode( final ISobekModelMember model, final IBranch branch, final GM_Point pointOnBranch, final Feature profile ) throws Exception
  {
    if( branch == null || pointOnBranch == null || profile == null )
      return;

    /* create new profile node */
    final INode node = FNGmlUtils.createNode( model, TYPE.eCrossSectionNode, pointOnBranch, new INode[] {} );

    /* link branch and profile */
    FeatureUtils.updateLinkedFeature( model.getWorkspace(), node.getFeature(), ISobekConstants.QN_LN_LINKS_TO_BRANCH, IGmlWorkspaces.HYDRAUL_MODEL + "#" + branch.getFeature().getId() ); //$NON-NLS-1$
    FeatureUtils.updateLinkedFeature( model.getWorkspace(), node.getFeature(), ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE, "#" + profile.getId() ); //$NON-NLS-1$

    node.getFeature().invalidEnvelope();
  }

  public static void extendBranch( final IModelMember model, final IBranch branch, final GM_Curve curve ) throws Exception
  {
    final INode upperNode = branch.getUpperNode();
    final INode lowerNode = branch.getLowerNode();

    FNGmlUtils.createBranch( model, curve, new INode[] { upperNode, lowerNode }, TYPE.eConnectionNode, TYPE.eConnectionNode );
  }
}
