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
package org.kalypso.model.wspm.sobek.core.pub;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.model.BoundaryNode;
import org.kalypso.model.wspm.sobek.core.model.ConnectionNode;
import org.kalypso.model.wspm.sobek.core.model.CrossSectionNode;
import org.kalypso.model.wspm.sobek.core.model.LinkageNode;
import org.kalypso.model.wspm.sobek.core.model.SbkStructCompoundStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructDatabaseStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructGeneralStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructPump;
import org.kalypso.model.wspm.sobek.core.model.SbkStructRiverWeir;
import org.kalypso.model.wspm.sobek.core.model.SbkStructWeir;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNNodeUtils
{
  public static INode createNode( final IModelMember model, final IFeatureType targetFeatureType, final GM_Point point, final TYPE nodeType ) throws Exception
  {
    final IRelationType targetPropertyType = (IRelationType) model.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
    final String nodeId = FNNodeUtils.createNodeId( model, targetFeatureType );

    final Map<IPropertyType, Object> values = new HashMap<IPropertyType, Object>();
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LOCATION ), point );
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID ), nodeId );
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_NAME ), nodeId );

    if( (nodeType != null) && !TYPE.eCrossSectionNode.equals( nodeType ) )
      values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_CONNECTION_TYPE ), nodeType.getTypeOfConnectionNode() );

    CommandableWorkspace cw;
    final GMLWorkspace workspace = model.getFeature().getWorkspace();
    if( workspace instanceof CommandableWorkspace )
      cw = (CommandableWorkspace) workspace;
    else
      cw = new CommandableWorkspace( workspace );

    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( cw, targetFeatureType, model.getFeature(), targetPropertyType, -1, values, selectionManager );
    cw.postCommand( command );

    return FNNodeUtils.getNode( model, command.getNewFeature() );
  }

  private static String createNodeId( final IModelMember model, final IFeatureType targetFeatureType )
  {
    int count = 0;

    final INode[] nodes = model.getNodeMembers();
    for( final INode node : nodes )
      if( targetFeatureType.equals( node.getFeature().getFeatureType() ) )
      {
        final String nodeId = node.getId();
        if( nodeId == null )
          continue;

        final String[] split = nodeId.split( "_" );
        if( split.length != 2 )
          throw new IllegalStateException( "Node id is incorrect" );

        final Integer iBranch = new Integer( split[1] );
        if( iBranch > count )
          count = iBranch;
      }

    return String.format( "%s%05d", FNNodeUtils.getDelimiter( targetFeatureType ), ++count );
  }

  private static String getDelimiter( final IFeatureType targetFeatureType )
  {
    final QName qn = targetFeatureType.getQName();

    if( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE.equals( qn ) )
      return "ln_";
    else if( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE.equals( qn ) )
      return "bn_";
    else if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( qn ) )
      return "cn_";
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qn ) )
      return "csn_";
    else if( ISobekConstants.QN_NOFDP_POLDER_NODE.equals( qn ) )
      return "pn_";
    else if( ISobekConstants.QN_NOFDP_RETARDIN_BASIN_NODE.equals( qn ) )
      return "rbn_";
    else if( ISobekConstants.QN_NOFDP_WEIR_NODE.equals( qn ) )
      return "wn_";
    else
      throw new NotImplementedException();
  }

  public static INode getNode( final IModelMember model, final Feature node )
  {
    if( node == null )
      return null;

    final QName qname = node.getFeatureType().getQName();
    if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( qname ) )
      return new ConnectionNode( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE.equals( qname ) )
      return new LinkageNode( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE.equals( qname ) )
      return new BoundaryNode( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qname ) )
      return new CrossSectionNode( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE.equals( qname ) )
      return new SbkStructCompoundStructure( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR.equals( qname ) )
      return new SbkStructRiverWeir( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE.equals( qname ) )
      return new SbkStructGeneralStructure( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE.equals( qname ) )
      return new SbkStructDatabaseStructure( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR.equals( qname ) )
      return new SbkStructWeir( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP.equals( qname ) )
      return new SbkStructPump( model, node );

    return new EmptyNodeImplementation( model, node );
  }
}
