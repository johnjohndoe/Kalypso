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

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class NodeUtils
{
  public static Feature createBoundaryNodeLastfallCondition( final ILastfall lastfall, final IBoundaryNode boundaryNode ) throws Exception
  {
    final IRelationType prop = (IRelationType) boundaryNode.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER );
    final IFeatureType targetType = prop.getTargetFeatureType();
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final CommandableWorkspace cw = boundaryNode.getModelMember().getWorkspace();
    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( cw, targetType, boundaryNode.getFeature(), prop, -1, null, selectionManager );
    cw.postCommand( command );

    /* set linked lastfall! */
    final Feature condition = command.getNewFeature();
    FeatureUtils.setInternalLinkedFeature( cw, condition, ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL, lastfall.getFeature() );

    return command.getNewFeature();
  }

  public static void convertLinkageNodeToConnectionNode( final LinkageNode node ) throws Exception
  {
    final IBranch[] inflowingBranches = node.getInflowingBranches();
    final IBranch[] outflowingBranches = node.getOutflowingBranches();

    final IConnectionNode cn = (IConnectionNode) FNGmlUtils.createNode( node.getModelMember(), TYPE.eConnectionNode, node.getLocation(), new INode[] {} );

    for( final IBranch branch : inflowingBranches )
    {
      cn.addInflowingBranch( branch );
      branch.setLowerNode( cn ); // update the branch too!
    }

    for( final IBranch branch : outflowingBranches )
    {
      cn.addOutflowingBranch( branch );
      branch.setUpperNode( cn );// update the branch too!
    }

    node.delete();
  }
}
