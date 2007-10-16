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
import org.kalypso.model.wspm.sobek.core.model.ConnectionNode;
import org.kalypso.model.wspm.sobek.core.model.LinkageNode;
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
  public static INode createNode( IModelMember model, IFeatureType targetFeatureType, GM_Point point ) throws Exception
  {
    final IRelationType targetPropertyType = (IRelationType) model.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
    final String nodeId = createNodeId( model, targetFeatureType );

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

    return getNode( model, command.getNewFeature() );
  }

  private static String createNodeId( final IModelMember model, final IFeatureType targetFeatureType )
  {
    int count = 0;

    INode[] nodes = model.getNodeMembers();
    for( final INode node : nodes )
    {

      if( targetFeatureType.equals( node.getFeature().getFeatureType() ) )
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

    return String.format( "%s%05d", getDelimiter( targetFeatureType ), ++count );
  }

  private static String getDelimiter( IFeatureType targetFeatureType )
  {
    QName qn = targetFeatureType.getQName();

    if( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE.equals( qn ) )
      return "ln_";
    else if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( qn ) )
      return "cn_";
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qn ) )
      return "csn_";
    else if( ISobekConstants.QN_NOFDP_POLDER_NODE.equals( qn ) )
      return "pn_";
    else
      throw (new NotImplementedException());
  }

  public static INode getNode( IModelMember model, Feature node )
  {
    QName qname = node.getFeatureType().getQName();
    if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( qname ) )
      return new ConnectionNode( model, node );
    else if( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE.equals( qname ) )
      return new LinkageNode( model, node );

    return new EmptyNode( node );
  }
}
