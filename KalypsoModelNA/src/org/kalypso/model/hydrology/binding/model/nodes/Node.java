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
package org.kalypso.model.hydrology.binding.model.nodes;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.model.AbstractNaModelElement;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}Node.
 *
 * @author Gernot Belger
 */
public class Node extends AbstractNaModelElement implements INode
{

  public Node( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * Returns all channels of this na modell that have this node as downstream node.<br/>
   * Use with care, as this method involves a linear search through all existing channels.
   */
  @Override
  public Channel[] findUpstreamChannels( )
  {
    final Collection<Channel> upstreamChannels = new ArrayList<>();

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

  @Override
  public Branching getBranching( )
  {
    return (Branching) FeatureHelper.resolveLink( this, MEMBER_BRANCHING, true );
  }

  @Override
  public Channel getDownstreamChannel( )
  {
    return (Channel) FeatureHelper.resolveLink( this, PROPERTY_LINKED_DOWNSTREAMCHANNEL, true );
  }

  @Override
  public ZmlLink getPegelLink( )
  {
    return new ZmlLink( this, PROPERTY_PEGEL_ZR );
  }

  @Override
  public Node getQQRelatedNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, PROPERTY_QQ_RELATED_NODE, true );
  }

  @Override
  public ZmlLink getResultAsInflowLink( )
  {
    return new ZmlLink( this, PROPERTY_RESULT_AS_INFLOW_ZR );
  }

  /**
   * Returns the link that should be used as inflow. Checks if it really should be used, else <code>null</code> is
   * returned.
   */
  @Override
  public ZmlLink getResultAsInflowLinkChecked( final Node rootNode )
  {
    if( !isUseResultAsInflow() )
      return null;

    if( this == rootNode )
      return null;

    if( rootNode == null && isGenerateResults() )
      return null;

    final ZmlLink resultAsInflowLink = getResultAsInflowLink();
    if( !resultAsInflowLink.isLinkExisting() )
      return null;

    return resultAsInflowLink;
  }

  @Deprecated
  @Override
  public ZmlLink getResultLink( )
  {
    return new ZmlLink( this, PROPERTY_RESULT_TIMESERIESLINK );
  }

  @Override
  public ZmlLink getZuflussLink( )
  {
    return new ZmlLink( this, PROPERTY_ZUFLUSS_ZR );
  }

  @Override
  public boolean isGenerateResults( )
  {
    return getBooleanProperty( PROPERTY_GENERATE_RESULT, false );
  }

  @Override
  public Boolean isSynteticZufluss( )
  {
    return getProperty( PROPERTY_SYNTHETIC_ZUFLUSS_ZR, Boolean.class );
  }

  @Override
  public boolean isUseResultAsInflow( )
  {
    return getBooleanProperty( PROPERTY_USE_RESULT_AS_INFLOW, false );
  }

  @Override
  public void setBranching( final Branching branching )
  {
    setProperty( MEMBER_BRANCHING, branching );
  }

  @Override
  public void setDownstreamChannel( final Channel downstreamChannel )
  {
    FeatureHelper.setAsLink( this, PROPERTY_LINKED_DOWNSTREAMCHANNEL, downstreamChannel );
  }

  @Override
  public void setGenerateResults( final boolean value )
  {
    setProperty( PROPERTY_GENERATE_RESULT, value );
  }

  @Override
  public void setIsSynteticZufluss( final Boolean isSynteticZufluss )
  {
    setProperty( PROPERTY_SYNTHETIC_ZUFLUSS_ZR, isSynteticZufluss );
  }

  @Override
  public void setZuflussLink( final TimeseriesLinkType zuflussLink )
  {
    setProperty( PROPERTY_ZUFLUSS_ZR, zuflussLink );
  }

  @Override
  public GM_Point getPosition( )
  {
    final Object property = getProperty( PROPERTY_ORT );
    if( property instanceof GM_Point )
      return (GM_Point) property;

    return null;
  }

  @Override
  public Point getJtsPosition( ) throws GM_Exception
  {
    final GM_Point point = getPosition();
    if( Objects.isNotNull( point ) )
      return (Point) JTSAdapter.export( point );

    return null;
  }

  @Override
  public String getRiverCode( )
  {
    final Object property = getProperty( PROPERTY_RIVER_CODE );
    if( property instanceof String )
      return (String) property;

    return null;
  }

  @Override
  public Double getRiverKm( )
  {
    final Object property = getProperty( PROPERTY_RIVER_KM );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  @Override
  public String getResultCategory( )
  {
    return getProperty( PROPERTY_RESULT_CATEGORY, String.class );
  }
}