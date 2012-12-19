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
package org.kalypso.model.hydrology.binding.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

import de.renew.workflow.connector.cases.IModel;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}NaModell.
 *
 * @author Gernot Belger
 */
public class NaModell extends Feature_Impl implements IModel
{
  public static final String NS_NAMODELL = NaModelConstants.NS_NAMODELL;

  public static final QName MEMBER_CATCHMENT_COLLECTION = new QName( NS_NAMODELL, "CatchmentCollectionMember" ); //$NON-NLS-1$

  public static final QName MEMBER_CATCHMENT = new QName( NS_NAMODELL, "catchmentMember" ); //$NON-NLS-1$

  public static final QName MEMBER_CHANNEL_COLLECTION = new QName( NS_NAMODELL, "ChannelCollectionMember" ); //$NON-NLS-1$

  public static final QName MEMBER_CHANNEL = new QName( NS_NAMODELL, "channelMember" ); //$NON-NLS-1$

  public static final QName MEMBER_NODE_COLLECTION = new QName( NS_NAMODELL, "NodeCollectionMember" ); //$NON-NLS-1$

  public static final QName MEMBER_NODE = new QName( NS_NAMODELL, "nodeMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_LOCATION = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  // <element name="SwaleAndTrenchCollectionMember" type="na:SwaleAndTrenchCollectionAssociationType"/>

  private IFeatureBindingCollection<Catchment> m_catchments = null;

  private IFeatureBindingCollection<Channel> m_channels = null;

  private IFeatureBindingCollection<Node> m_nodes = null;

  public NaModell( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public String getVersion( )
  {
    return NO_VERSION;
  }

  public synchronized IFeatureBindingCollection<Catchment> getCatchments( )
  {
    if( m_catchments == null )
    {
      final Feature catchmentCollection = getProperty( MEMBER_CATCHMENT_COLLECTION, Feature.class );
      m_catchments = new FeatureBindingCollection<>( catchmentCollection, Catchment.class, MEMBER_CATCHMENT );
    }

    return m_catchments;
  }

  public synchronized IFeatureBindingCollection<Channel> getChannels( )
  {
    if( m_channels == null )
    {
      final Feature channelCollection = getProperty( MEMBER_CHANNEL_COLLECTION, Feature.class );
      m_channels = new FeatureBindingCollection<>( channelCollection, Channel.class, MEMBER_CHANNEL );
    }

    return m_channels;
  }

  public KMChannel[] getKMChannels( )
  {
    final List<KMChannel> result = new ArrayList<>();
    final IFeatureBindingCollection<Channel> channels = getChannels();
    for( final Channel channel : channels )
    {
      if( channel instanceof KMChannel )
        result.add( (KMChannel) channel );
    }

    return result.toArray( new KMChannel[result.size()] );
  }

  public synchronized IFeatureBindingCollection<Node> getNodes( )
  {
    if( m_nodes == null )
    {
      final Feature nodeCollection = getProperty( MEMBER_NODE_COLLECTION, Feature.class );
      m_nodes = new FeatureBindingCollection<>( nodeCollection, Node.class, MEMBER_NODE );
    }

    return m_nodes;
  }
}