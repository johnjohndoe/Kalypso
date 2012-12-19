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
package org.kalypso.model.hydrology.binding.model.channels;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}StorageChannel.
 *
 * @author Gernot Belger
 */
public class StorageChannel extends Channel implements IStorageChannel
{
  public StorageChannel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IObservation getWVQObservation( )
  {
    return (IObservation) getProperty( PROPERTY_HVVSQD );
  }

  @Override
  public boolean isGenerateResults( )
  {
    return getBooleanProperty( PROPERTY_GENERATE_RESULT, false );
  }

  @Override
  public void setGenerateResults( final boolean value )
  {
    setProperty( PROPERTY_GENERATE_RESULT, value );
  }

  @Override
  public INode getOverflowNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, PROPERTY_DOWNSTREAM_NODE, true );
  }

  @Override
  public INode getOutletNode1( )
  {
    return (INode) FeatureHelper.resolveLink( this, PROPERTY_DOWNSTREAM_NODE_2, true );
  }

  @Override
  public INode getOutletNode2( )
  {
    return (INode) FeatureHelper.resolveLink( this, PROPERTY_DOWNSTREAM_NODE_3, true );
  }

  @Override
  public ZmlLink getSeaEvaporationTimeseriesLink( )
  {
    return new ZmlLink( this, PROPERTY_SEA_EVAPORATION_ZMLLINK );
  }

  @Override
  public double getSeaEvaporationFactor( )
  {
    return getDoubleProperty( PROPERTY_SEA_EVAPORATION_FACTOR, 1.0 );
  }

  @Override
  public double getInitialCapacity( )
  {
    return getDoubleProperty( PROPERTY_INITIAL_CAPACITY, 0.0 );
  }

  @Override
  public double getVolumeMax( )
  {
    return getDoubleProperty( PROPERTY_VOLUME_MAX, 0.0 );
  }

  @Override
  public double getVolumeMin( )
  {
    return getDoubleProperty( PROPERTY_VOLUME_MIN, 0.0 );
  }

  @Override
  public String getResultCategory( )
  {
    return getProperty( PROPERTY_RESULT_CATEGORY, String.class );
  }
}
