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
package org.kalypso.ui.rrm.internal.map.editRelation;

import java.util.ArrayList;
import java.util.Collection;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Grundwasserabfluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.IChannel;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class EditRelationInput
{
  private final CommandableWorkspace m_workspace;

  private final IEditRelationType[] m_elements;

  public EditRelationInput( final CommandableWorkspace workspace )
  {
    m_workspace = workspace;

    final Collection<IEditRelationType> elements = new ArrayList<>();

    elements.add( addLightRelationType( INode.FEATURE_NODE, INode.PROPERTY_LINKED_DOWNSTREAMCHANNEL, IChannel.FEATURE_CHANNEL ) );
    elements.add( addLightRelationType( INode.FEATURE_NODE, INode.PROPERTY_QQ_RELATED_NODE, INode.FEATURE_NODE ) );
    elements.add( addHeavyRelationType( INode.FEATURE_NODE, INode.MEMBER_BRANCHING, BranchingWithNode.FEATURE_BRANCHING_WITH_NODE, BranchingWithNode.QN_BRANCHING_NODE_MEMBER, INode.FEATURE_NODE ) );

    elements.add( addLightRelationType( IChannel.FEATURE_CHANNEL, IChannel.PROPERTY_LINKED_DOWNSTREAM_NODE, INode.FEATURE_NODE ) );

    elements.add( addLightRelationType( IStorageChannel.FEATURE_STORAGE_CHANNEL, IStorageChannel.PROPERTY_DOWNSTREAM_NODE, INode.FEATURE_NODE ) );
    elements.add( addLightRelationType( IStorageChannel.FEATURE_STORAGE_CHANNEL, IStorageChannel.PROPERTY_DOWNSTREAM_NODE_2, INode.FEATURE_NODE ) );
    elements.add( addLightRelationType( IStorageChannel.FEATURE_STORAGE_CHANNEL, IStorageChannel.PROPERTY_DOWNSTREAM_NODE_3, INode.FEATURE_NODE ) );

    elements.add( addLightRelationType( Catchment.FEATURE_CATCHMENT, Catchment.LINK_CHANNEL, IChannel.FEATURE_CHANNEL ) );
    elements.add( addLightRelationType( Catchment.FEATURE_CATCHMENT, Catchment.LINK_OVERFLOW_NODE, INode.FEATURE_NODE ) );
    elements.add( addHeavyRelationType( Catchment.FEATURE_CATCHMENT, Catchment.PROPLIST_GRUNDWASSERABFLUSS_MEMBER, Grundwasserabfluss.FEATURE_GRUNDWASSERABFLUSS, Grundwasserabfluss.LINK_NGWZU, Catchment.FEATURE_CATCHMENT ) );
    elements.add( addLightRelationType( Catchment.FEATURE_CATCHMENT, Catchment.LINK_IZKN_NODE, INode.FEATURE_NODE ) );

    m_elements = elements.toArray( new IEditRelationType[elements.size()] );
  }

  private LightRelationType addLightRelationType( final QName source, final QName relation, final QName target )
  {
    final IFeatureType sourceType = GMLSchemaUtilities.getFeatureTypeQuiet( source );
    final IRelationType relationType = (IRelationType) sourceType.getProperty( relation );
    final IFeatureType targetType = GMLSchemaUtilities.getFeatureTypeQuiet( target );

    return new LightRelationType( sourceType, relationType, targetType );
  }

  private HeavyRelationType addHeavyRelationType( final QName source, final QName relation1, final QName middle, final QName relation2, final QName target )
  {
    final IFeatureType sourceType = GMLSchemaUtilities.getFeatureTypeQuiet( source );
    final IRelationType relationType1 = (IRelationType) sourceType.getProperty( relation1 );
    final IFeatureType middleType = GMLSchemaUtilities.getFeatureTypeQuiet( middle );
    final IRelationType relationType2 = (IRelationType) middleType.getProperty( relation2 );
    final IFeatureType targetType = GMLSchemaUtilities.getFeatureTypeQuiet( target );

    return new HeavyRelationType( sourceType, relationType1, middleType, relationType2, targetType );
  }

  public IEditRelationType[] getElements( )
  {
    return m_elements;
  }

  public NaModell getNaModel( )
  {
    return (NaModell) m_workspace.getRootFeature();
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }
}