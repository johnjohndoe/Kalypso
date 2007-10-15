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
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public abstract class AbstractNode implements INode
{
  private final Feature m_node;

  public AbstractNode( Feature node )
  {
    m_node = node;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getId()
   */
  public String getId( )
  {
    return (String) m_node.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
  }

  private static String getDelimiter( TYPE nodeType )
  {
    switch( nodeType )
    {
      case eLinkageNode:
        return "ln_";

      case eConnectionNode:
        return "cn_";

      case eCrossSectionNode:
        return "csn_";

      default:
        throw (new NotImplementedException());
    }
  }

// //
// switch( nodeType )
// {
// case eLinkageNode:
// return schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE );
//
// case eCrossSectionNode:
// return schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE );
//
// case eConnectionNode:
// return schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE );
//
// default:
// throw (new IllegalStateException());
// }
// }

  public static INode createNode( SobekModelMember model, TYPE nodeType, GM_Point point ) throws Exception
  {
    final IRelationType targetPropertyType = (IRelationType) model.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    IGMLSchema schema = model.getFeature().getFeatureType().getGMLSchema();

    IFeatureType targetFeatureType;
    if( TYPE.eBoundaryNode.equals( nodeType ) )
    {
      throw (new NotImplementedException());
    }
    else if( TYPE.eConnectionNode.equals( nodeType ) )
    {
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE );
    }
    else if( TYPE.eCrossSectionNode.equals( nodeType ) )
    {
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE );
    }
    else if( TYPE.eLinkageNode.equals( nodeType ) )
    {
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE );
    }
    else
      throw (new IllegalStateException());

    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
    final String nodeId = createNodeId( model, nodeType );

    final Map<IPropertyType, Object> values = new HashMap<IPropertyType, Object>();
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LOCATION ), point );
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID ), nodeId );
    values.put( targetFeatureType.getProperty( ISobekConstants.QN_HYDRAULIC_NAME ), nodeId );

    CommandableWorkspace cw;
    GMLWorkspace workspace = model.getFeature().getWorkspace();
    if( workspace instanceof CommandableWorkspace )
      cw = (CommandableWorkspace) workspace;
    else
      cw = new CommandableWorkspace( workspace );

    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( cw, targetFeatureType, model.getFeature(), targetPropertyType, -1, values, selectionManager );
    cw.postCommand( command );

    return getNode( command.getNewFeature() );
  }

  public static INode getNode( Feature node )
  {
    QName qname = node.getFeatureType().getQName();
    if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( qname ) )
      return new ConnectionNode( node );

    throw (new NotImplementedException());
  }

  private static String createNodeId( final SobekModelMember model, final TYPE nodeType )
  {
    int count = 0;

    INode[] nodes = model.getNodeMembers();
    for( final INode node : nodes )
    {

      if( nodeType.equals( node.getType() ) )
      {
        String nodeId = node.getId();
        if( nodeId == null )
          continue;

        final String[] split = nodeId.split( "_" );
        if( split.length != 2 )
          throw new IllegalStateException();

        final Integer iBranch = new Integer( split[1] );
        if( iBranch > count )
          count = iBranch;
      }
    }

    return String.format( "%s%05d", getDelimiter( nodeType ), ++count );
  }
}
