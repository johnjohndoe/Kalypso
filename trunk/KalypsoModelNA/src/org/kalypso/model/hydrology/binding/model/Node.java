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
import java.util.Collection;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}Node.
 * 
 * @author Gernot Belger
 */
public class Node extends AbstractNaModelElement
{
  public static final QName FEATURE_NODE = new QName( NaModelConstants.NS_NAMODELL, "Node" ); //$NON-NLS-1$

  /** @deprecated Do not use directly, use accessor methods instead. */
  @Deprecated
  public static final QName PROP_ZUFLUSS_ZR = new QName( NS_NAMODELL, "zuflussZR" ); //$NON-NLS-1$

  private static final QName PROP_PEGEL_ZR = new QName( NS_NAMODELL, "pegelZR" ); //$NON-NLS-1$

  /** @deprecated Do not use directly, use accessor methods instead. */
  @Deprecated
  public static final QName PROP_RESULT_TIMESERIESLINK = new QName( NS_NAMODELL, "qberechnetZR" ); //$NON-NLS-1$

  private static final QName PROP_SYNTHETIC_ZUFLUSS_ZR = new QName( NS_NAMODELL, "syntheticZuflussZR" ); //$NON-NLS-1$

  private static final QName MEMBER_BRANCHING = new QName( NS_NAMODELL, "branchingMember" ); //$NON-NLS-1$

  private static final QName LINK_DOWNSTREAMCHANNEL = new QName( NS_NAMODELL, "downStreamChannelMember" ); //$NON-NLS-1$

  private static final QName GENERATE_RESULT_PROP = new QName( NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  private static final QName NODE_QQRELATED_NODE_PROP = new QName( NS_NAMODELL, "qqRelatedNode" ); //$NON-NLS-1$

  public Node( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public TimeseriesLinkType getPegelLink( )
  {
    return getProperty( PROP_PEGEL_ZR, TimeseriesLinkType.class );
  }

  public TimeseriesLinkType getZuflussLink( )
  {
    return getProperty( PROP_ZUFLUSS_ZR, TimeseriesLinkType.class );
  }

  public void setZuflussLink( final TimeseriesLinkType zuflussLink )
  {
    setProperty( PROP_ZUFLUSS_ZR, zuflussLink );
  }

  public TimeseriesLinkType getResultLink( )
  {
    return getProperty( PROP_RESULT_TIMESERIESLINK, TimeseriesLinkType.class );
  }

  public void setResultLink( final TimeseriesLinkType zuflussLink )
  {
    setProperty( PROP_RESULT_TIMESERIESLINK, zuflussLink );
  }

  /**
   * Returns all channels of this na modell that have this node as downstream node.<br/>
   * Use with care, as this method involves a linear search through all existing channels.
   */
  public Channel[] findUpstreamChannels( )
  {
    final Collection<Channel> upstreamChannels = new ArrayList<Channel>();

    final NaModell naModel = getNaModel();

    final IFeatureBindingCollection<Channel> channels = naModel.getChannels();
    for( final Channel channel : channels )
    {
      final Node downstreamNode = channel.getDownstreamNode();
      if( downstreamNode == this )
        upstreamChannels.add( channel );
    }

    return upstreamChannels.toArray( new Channel[upstreamChannels.size()] );
  }

  public Branching getBranching( )
  {
    return (Branching) FeatureHelper.resolveLink( this, MEMBER_BRANCHING, true );
  }

  public void setBranching( final Branching branching )
  {
    setProperty( MEMBER_BRANCHING, branching );
  }

  public Boolean isSynteticZufluss( )
  {
    return getProperty( PROP_SYNTHETIC_ZUFLUSS_ZR, Boolean.class );
  }

  public void setIsSynteticZufluss( final Boolean isSynteticZufluss )
  {
    setProperty( PROP_SYNTHETIC_ZUFLUSS_ZR, isSynteticZufluss );
  }

  public void setDownstreamChannel( final Channel downstreamChannel )
  {
    FeatureHelper.setAsLink( this, LINK_DOWNSTREAMCHANNEL, downstreamChannel );
  }

  public Channel getDownstreamChannel( )
  {
    return (Channel) FeatureHelper.resolveLink( this, LINK_DOWNSTREAMCHANNEL, true );
  }

  public boolean isGenerateResults( )
  {
    return getBoolean( GENERATE_RESULT_PROP, false );
  }

  public void setGenerateResults( final boolean value )
  {
    setProperty( GENERATE_RESULT_PROP, value );
  }

  public Node getQQRelatedNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, NODE_QQRELATED_NODE_PROP, true );
  }

}
