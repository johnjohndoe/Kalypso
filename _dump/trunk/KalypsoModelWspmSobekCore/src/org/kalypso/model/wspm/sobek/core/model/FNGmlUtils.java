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

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.NODE_BRANCH_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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
  public static INode[] createBranch( final SobekModelMember model, final GM_Curve curve, final Feature[] nodes, final TYPE upperNodeType, final TYPE lowerNodeType ) throws Exception
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

      FeatureUtils.updateLinkedFeature( branch, ISobekConstants.QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE, "#" + upperNode.getFeature().getId() );
      FeatureUtils.updateLinkedFeature( branch, ISobekConstants.QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE, "#" + lowerNode.getFeature().getId() );

      FNGmlUtils.addBranchesToLinkToNodes( model, new INode[] { upperNode, lowerNode } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return new INode[] { upperNode, lowerNode };
  }

  public static INode createNode( final SobekModelMember model, final TYPE nodeType, final GM_Point point, final Feature[] nodes ) throws Exception
  {
    // a new node must be created?!?
    for( final Feature node : nodes )
    {
      final GM_Point pNode = (GM_Point) node.getDefaultGeometryProperty();
      if( pNode.intersects( point ) )
        return AbstractNode.getNode( node );
    }

    INode node = AbstractNode.createNode( model, nodeType, point );

    return node;
  }

  private static String createBranchId( final GMLWorkspace workspace )
  {
// final Feature root = workspace.getRootFeature();
// final List< ? > branches = (List< ? >) root.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
//
// int count = 0;
//
// for( final Object object : branches )
// {
// if( !(object instanceof Feature) )
// continue;
//
// final Feature branch = (Feature) object;
// final String id = (String) branch.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
// if( id == null )
// continue;
//
// final String[] split = id.split( "b_" );
// if( split.length != 2 )
// throw new IllegalStateException();
//
// final Integer iBranch = new Integer( split[1] );
//
// if( iBranch > count )
// count = iBranch;
// }
//
// return String.format( "b_%05d", ++count );

    throw (new NotImplementedException());
  }

  public static void deleteFoo( final GMLWorkspace workspace, final Feature feature ) throws Exception
  {
// if( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH.equals( feature.getFeatureType().getQName() ) )
// {
// FNGmlUtils.deleteBranch( workspace, feature );
// return;
// }
//
// final TYPE type = TYPE.getFeatureType( feature );
// switch( type )
// {
// case eCrossSectionNode:
// FeatureUtils.deleteFeature( feature );
// break;
//
// case eLinkageNode:
// throw new IllegalStateException(); // delete branch?!? no - delete branch and its linkage nodes!
//
// default:
// throw (new NotImplementedException());
// }
  }

  /**
   * Delete nofdp branch - a branch consists of a branch ;-) and two connection nodes - remark: a connection node can
   * connected to more than one branch!
   */

  private static void deleteBranch( final GMLWorkspace workspace, final Feature branch ) throws Exception
  {
// // getAllLinkageNodes of branch
// // branch its the only branch link of linkage node?!? if yes -> delete linkage node...
// final List<Feature> nodes = new ArrayList<Feature>();
//
// final QName[] qn = new QName[] { ISobekConstants.QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE,
// ISobekConstants.QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE };
//
// for( final QName q : qn )
// {
// final Object lnkNode = branch.getProperty( q );
// final Feature upperLnkNode = FNGmlUtils.getLinkedNodeFeature( workspace, lnkNode );
// nodes.add( upperLnkNode );
// }
//
// FNGmlUtils.removeNodeBranchLinks( workspace, branch, nodes.toArray( new Feature[] {} ) );
//
// // deletes empty nodes
// FNGmlUtils.cleanUpNodes( workspace, branch );
//
// // delete branch
// FeatureUtils.deleteFeature( branch );
  }

  /**
   * removes all empty nodes
   */
  private static void cleanUpNodes( final GMLWorkspace workspace, final Feature branch ) throws Exception
  {
// final Feature root = workspace.getRootFeature();
// final List< ? > nodes = (List< ? >) root.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
// final Object[] myNodes = nodes.toArray();
//
// for( final Object object : myNodes )
// {
// if( !(object instanceof Feature) )
// continue;
//
// final Feature node = (Feature) object;
//
// final TYPE type = TYPE.getFeatureType( node );
// switch( type )
// {
// case eCrossSectionNode:
// FNGmlUtils.deleteCrossSectionNode( workspace, branch, node );
// break;
//
// case eLinkageNode:
// FNGmlUtils.deleteConnectionNode( node );
// break;
//
// case eConnectionNode:
// FNGmlUtils.deleteConnectionNode( node );
// break;
//
// default:
// throw new NotImplementedException();
// }
// }
  }

  /** deletes an connection node if the node has no more linked branches! */
  private static void deleteConnectionNode( final Feature node ) throws Exception
  {
// final List< ? > inflowing = (List< ? >) node.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES
// );
// final List< ? > outflowing = (List< ? >) node.getProperty(
// ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
//
// if( (inflowing.size() == 0) && (outflowing.size() == 0) )
// FeatureUtils.deleteFeature( node );
  }

  /**
   * deletes an structure or crosssection if branch fits linked branch of node (remove branch -> remove all subnodes
   * snapped on branch!)
   */
  private static void deleteCrossSectionNode( final GMLWorkspace workspace, final Feature branch, final Feature node ) throws Exception
  {
// final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( final String id )
// {
// return FNGmlUtils.getBranch( workspace, id );
// }
//
// public Object getProperty( )
// {
// return node.getProperty( ISobekConstants.QN_HYDRAULICNODE_LINKED_BRANCH );
// }
// };
//
// final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
//
// if( branch.equals( wrapper.getFeature() ) )
// FeatureUtils.deleteFeature( node );
  }

  /**
   * return feature instance of a node link
   */
  protected static Feature getLinkedNodeFeature( final GMLWorkspace workspace, final Object lnkNode )
  {
// final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( String id )
// {
// final Feature[] nodes = FNGmlUtils.getLinkedNodes( workspace, new String[] { (String) lnkNode } );
// for( final Feature node : nodes )
// return node;
//
// return null;
// }
//
// public Object getProperty( )
// {
// return lnkNode;
// }
//
// };
//
// final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
//
// return wrapper.getFeature();

    throw (new NotImplementedException());
  }

  /**
   * resolves node features from node ids
   */
  protected static Feature[] getLinkedNodes( final GMLWorkspace workspace, final String[] nodesId )
  {
// final List<Feature> myNodes = new ArrayList<Feature>();
// final Feature root = workspace.getRootFeature();
//
// final List< ? > nodes = (List< ? >) root.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
// for( final Object object : nodes )
// {
// if( !(object instanceof Feature) )
// continue;
//
// final Feature node = (Feature) object;
//
// if( ArrayUtils.contains( nodesId, node.getId() ) )
// myNodes.add( node );
// }
//
// return myNodes.toArray( new Feature[] {} );

    throw (new NotImplementedException());
  }

  /**
   * remove linked branch from nodes. connection nodes are ConnectionNodes, LinkageNodes and BoundaryNodes
   */
  private static void removeNodeBranchLinks( final GMLWorkspace workspace, final Feature branch, final Feature[] nodes ) throws Exception
  {
// for( final Feature node : nodes )
// {
// if( node == null )
// continue;
//
// final List< ? > inflowing = (List< ? >) node.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES
// );
// final List< ? > outflowing = (List< ? >) node.getProperty(
// ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
//
// if( (inflowing.size() == 1) && (outflowing.size() == 1) )
// FeatureUtils.deleteFeature( node );
// else
// {
//
// // inflowing branches of node
// final Object[] inflow = inflowing.toArray();
// for( final Object object : inflow )
// {
// final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( String id )
// {
// return FNGmlUtils.getBranch( workspace, id );
// }
//
// public Object getProperty( )
// {
// return object;
// }
// };
//
// final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
//
// final Feature feature = wrapper.getFeature();
// if( feature.equals( branch ) )
// inflowing.remove( feature.getId() );
// }
//
// final Object[] outflow = outflowing.toArray();
// for( final Object object : outflow )
// {
// final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( final String id )
// {
// return FNGmlUtils.getBranch( workspace, id );
// }
//
// public Object getProperty( )
// {
// return object;
// }
// };
//
// final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
//
// final Feature feature = wrapper.getFeature();
// if( feature.equals( branch ) )
// outflowing.remove( feature.getId() );
// }
// }
//
// }

    throw (new NotImplementedException());
  }

  protected static Feature getBranch( GMLWorkspace workspace, final String id )
  {
// final Feature root = workspace.getRootFeature();
//
// final List< ? > branches = (List< ? >) root.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
// for( final Object object : branches )
// {
// if( !(object instanceof Feature) )
// continue;
//
// final Feature branch = (Feature) object;
//
// if( branch.getId().equals( id ) )
// return branch;
//
// }
// return null;

    throw (new NotImplementedException());
  }

  private static void addBranchesToLinkToNodes( SobekModelMember model, final INode[] nodes )
  {
    IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
    {
      final INode branchUpperNode = branch.getUpperNode();
      final INode branchLowerNode = branch.getLowerNode();

      if( ArrayUtils.contains( nodes, branchUpperNode ) )
        FNGmlUtils.addBranchToNode( branchUpperNode, branch, NODE_BRANCH_TYPE.eInflowingBranch );

      if( ArrayUtils.contains( nodes, branchLowerNode ) )
        FNGmlUtils.addBranchToNode( branchLowerNode, branch, NODE_BRANCH_TYPE.eOutflowingBranch );
    }
  }

  private static void addBranchToNode( final INode node, final IBranch branch, final NODE_BRANCH_TYPE direction )
  {
// FeatureList myList;
// if( NODE_BRANCH_TYPE.eInflowingBranch.equals( direction ) )
// myList = (FeatureList) node.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );
// else if( NODE_BRANCH_TYPE.eOutflowingBranch.equals( direction ) )
// myList = (FeatureList) node.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
// else
// throw new IllegalStateException();
//
// for( final Object object : myList )
// {
//
// final ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( String id )
// {
// Feature root = branch.getWorkspace().getRootFeature();
// List< ? > branches = (List< ? >) root.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
// for( Object b : branches )
// {
// if( !(b instanceof Feature) )
// continue;
//
// Feature fb = (Feature) b;
//
// if( fb.getId().equals( id ) )
// return fb;
// }
//
// return null;
// }
//
// // $ANALYSIS-IGNORE
// public Object getProperty( )
// {
// return object;
// }
// };
//
// final LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
// if( branch.equals( wrapper.getFeature() ) )
// return;
// }
//
// myList.add( branch.getId() );
  }

  public static void createInflowBranch( GMLWorkspace workspace, Feature feature, GM_Curve curve )
  {
// throw (new NotImplementedException());
  }

}
