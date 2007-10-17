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

/**
 * @author kuch
 */
public class FNGmlUtils
{
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
      throw new IllegalStateException( "Geometry is not a line!" );

    /* create linkage nodes at start and end position of linestring */
    final IGMLSchema schema = model.getFeature().getFeatureType().getGMLSchema();

    // start and end linkage node
    final INode upperNode = createNode( model, upperNodeType, curve.getStartPoint(), nodes );
    final INode lowerNode = createNode( model, lowerNodeType, curve.getEndPoint(), nodes );

    CommandableWorkspace workspace = FeatureUtils.getWorkspace( model.getFeature() );
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

      FeatureUtils.updateFeature( branch, values );

      IBranch myBranch = new Branch( model, branch );
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

  public static INode createNode( final IModelMember model, final TYPE nodeType, final GM_Point point, final INode[] nodes ) throws Exception
  {
    // a new node must be created?!?
    for( final INode node : nodes )
    {
      final GM_Point pNode = node.getLocation();
      if( pNode.intersects( point ) )
        return node;
    }

    INode node = AbstractNode.createNode( model, nodeType, point );
    return node;
  }

  /**
   * removes all empty nodes
   */
  public static void cleanUpNodes( final IModelMember model, final IBranch branch ) throws Exception
  {
    INode[] nodes = model.getNodeMembers();
    for( INode node : nodes )
    {
      if( node.isEmpty() )
      {
        node.delete();
      }
    }
  }

  private static void addBranchesToLinkToNodes( IModelMember model, final INode[] nodes ) throws Exception
  {
    IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
    {
      final IConnectionNode branchUpperNode = branch.getUpperNode();
      final IConnectionNode branchLowerNode = branch.getLowerNode();

      /* set inflowing and outflowing branches */
      if( ArrayUtils.contains( nodes, branchUpperNode ) )
        branchUpperNode.addOutflowingBranch( branch );

      if( ArrayUtils.contains( nodes, branchLowerNode ) )
        branchLowerNode.addInflowingBranch( branch );
    }

    /* node is an linkage node? set linkToBranch (ln lays on branch x - lnk to this branch!) */
    for( INode node : nodes )
    {
      if( !(node instanceof ILinkageNode) )
        continue;

      ILinkageNode ln = (ILinkageNode) node;
      ln.setLinkToBranch( branches );

    }
  }

  public static void createInflowBranch( IModelMember model, IBranch branch, GM_Curve curve ) throws Exception
  {
    INode[] nodes = new INode[] { branch.getUpperNode(), branch.getLowerNode() };
    createBranch( model, curve, nodes, TYPE.eConnectionNode, TYPE.eLinkageNode );
  }

  public static void createOutflowBranch( IModelMember model, IBranch branch, GM_Curve curve ) throws Exception
  {
    INode[] nodes = new INode[] { branch.getUpperNode(), branch.getLowerNode() };
    createBranch( model, curve, nodes, TYPE.eLinkageNode, TYPE.eConnectionNode );
  }

  public static void extendBranch( IModelMember model, final IBranch branch, final GM_Curve curve ) throws Exception
  {
    INode upperNode = branch.getUpperNode();
    INode lowerNode = branch.getLowerNode();

    createBranch( model, curve, new INode[] { upperNode, lowerNode }, TYPE.eConnectionNode, TYPE.eConnectionNode );
  }

  public static void connectBranches( final IModelMember model, final IBranch[] branches, final GM_Curve curve ) throws Exception
  {
    final List<INode> nodes = new ArrayList<INode>();

    for( final IBranch branch : branches )
    {
      nodes.add( branch.getUpperNode() );
      nodes.add( branch.getLowerNode() );
    }

    createBranch( model, curve, nodes.toArray( new INode[] {} ), TYPE.eConnectionNode, TYPE.eConnectionNode );
  }

  public static void createProfileNode( ISobekModelMember model, IBranch branch, GM_Point pointOnBranch, Feature profile ) throws Exception
  {
    if( (branch == null) || (pointOnBranch == null) || (profile == null) )
      return;

    /* create new profile node */
    INode node = createNode( model, TYPE.eCrossSectionNode, pointOnBranch, new INode[] {} );

    /* link branch and profile */
    FeatureUtils.updateLinkedFeature( node.getFeature(), ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_BRANCH, "#" + branch.getFeature().getId() );
    FeatureUtils.updateLinkedFeature( node.getFeature(), ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE, "#" + profile.getId() );
  }
}
