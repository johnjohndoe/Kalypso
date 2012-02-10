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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}StorageChannel.
 * 
 * @author Gernot Belger
 */
public class StorageChannel extends Channel
{
  public static final QName FEATURE_STORAGE_CHANNEL = new QName( NaModelConstants.NS_NAMODELL, "StorageChannel" ); //$NON-NLS-1$

  private static final QName PROP_HVVSQD = new QName( NS_NAMODELL, "hvvsqd" ); //$NON-NLS-1$

  private static final QName GENERATE_RESULT_PROP = new QName( NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  private static final QName PROP_DOWNSTREAM_NODE = new QName( NS_NAMODELL, "iknotNodeMember" ); //$NON-NLS-1$

  private static final QName PROP_DOWNSTREAM_NODE_2 = new QName( NS_NAMODELL, "downStreamNodeMember_2nd" ); //$NON-NLS-1$

  private static final QName PROP_DOWNSTREAM_NODE_3 = new QName( NS_NAMODELL, "downStreamNodeMember_3rd" ); //$NON-NLS-1$

  private static final QName PROP_SEA_EVAPORATION_ZMLLINK = new QName( NS_NAMODELL, "zmlLinkSeaEvaporation" ); //$NON-NLS-1$

  private static final QName PROP_SEA_EVAPORATION_FACTOR = new QName( NS_NAMODELL, "faktorSeaEvaporation" ); //$NON-NLS-1$

  private static final QName PROP_INITIAL_CAPACITY = new QName( NS_NAMODELL, "sv" ); //$NON-NLS-1$

  private static final QName PROP_VOLUME_MAX = new QName( NS_NAMODELL, "vmax" ); //$NON-NLS-1$

  private static final QName PROP_VOLUME_MIN = new QName( NS_NAMODELL, "vmin" ); //$NON-NLS-1$

  public StorageChannel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public IObservation getWVQObservation( )
  {
    return (IObservation) getProperty( PROP_HVVSQD );
  }

  public boolean isGenerateResults( )
  {
    return getBoolean( GENERATE_RESULT_PROP, false );
  }

  public void setGenerateResults( final boolean value )
  {
    setProperty( GENERATE_RESULT_PROP, value );
  }

  public Node getOverflowNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, PROP_DOWNSTREAM_NODE, true );
  }

  public Node getOverflowNode2( )
  {
    return (Node) FeatureHelper.resolveLink( this, PROP_DOWNSTREAM_NODE_2, true );
  }

  public Node getOverflowNode3( )
  {
    return (Node) FeatureHelper.resolveLink( this, PROP_DOWNSTREAM_NODE_3, true );
  }

  public TimeseriesLinkType getSeaEvaporationTimeseriesLink( )
  {
    return getProperty( PROP_SEA_EVAPORATION_ZMLLINK, TimeseriesLinkType.class );
  }

  public double getSeaEvaporationFactor( )
  {
    return getDoubleProperty( PROP_SEA_EVAPORATION_FACTOR, 1.0 );
  }

  public double getInitialCapacity( )
  {
    return getDoubleProperty( PROP_INITIAL_CAPACITY, 0.0 );
  }

  public double getVolumeMax( )
  {
    return getDoubleProperty( PROP_VOLUME_MAX, 0.0 );
  }

  public double getVolumeMin( )
  {
    return getDoubleProperty( PROP_VOLUME_MIN, 0.0 );
  }
}
