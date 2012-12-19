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
package org.kalypso.model.hydrology.binding.model.channels;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;

/**
 * @author Dirk Kuch
 */
public interface IStorageChannel extends IChannel
{
  QName FEATURE_STORAGE_CHANNEL = new QName( NaModelConstants.NS_NAMODELL, "StorageChannel" ); //$NON-NLS-1$

  QName PROPERTY_DOWNSTREAM_NODE = new QName( NaModelConstants.NS_NAMODELL, "iknotNodeMember" ); //$NON-NLS-1$

  QName PROPERTY_DOWNSTREAM_NODE_2 = new QName( NaModelConstants.NS_NAMODELL, "downStreamNodeMember_2nd" ); //$NON-NLS-1$

  QName PROPERTY_DOWNSTREAM_NODE_3 = new QName( NaModelConstants.NS_NAMODELL, "downStreamNodeMember_3rd" ); //$NON-NLS-1$

  QName PROPERTY_GENERATE_RESULT = new QName( NaModelConstants.NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  QName PROPERTY_RESULT_CATEGORY = new QName( NaModelConstants.NS_NAMODELL, "resultCategory" ); //$NON-NLS-1$

  /**
   * wvq-relationship
   */
  QName PROPERTY_HVVSQD = new QName( NaModelConstants.NS_NAMODELL, "hvvsqd" ); //$NON-NLS-1$

  QName PROPERTY_INITIAL_CAPACITY = new QName( NaModelConstants.NS_NAMODELL, "sv" ); //$NON-NLS-1$

  QName PROPERTY_SEA_EVAPORATION_FACTOR = new QName( NaModelConstants.NS_NAMODELL, "faktorSeaEvaporation" ); //$NON-NLS-1$

  QName PROPERTY_SEA_EVAPORATION_ZMLLINK = new QName( NaModelConstants.NS_NAMODELL, "zmlLinkSeaEvaporation" ); //$NON-NLS-1$

  QName PROPERTY_VOLUME_MAX = new QName( NaModelConstants.NS_NAMODELL, "vmax" ); //$NON-NLS-1$

  QName PROPERTY_VOLUME_MIN = new QName( NaModelConstants.NS_NAMODELL, "vmin" ); //$NON-NLS-1$

  double getInitialCapacity( );

  INode getOverflowNode( );

  INode getOutletNode1( );

  INode getOutletNode2( );

  double getSeaEvaporationFactor( );

  ZmlLink getSeaEvaporationTimeseriesLink( );

  double getVolumeMax( );

  double getVolumeMin( );

  /**
   * @return IObservation stored in "hvvsqd" feature type property
   */
  IObservation getWVQObservation( );

  boolean isGenerateResults( );

  void setGenerateResults( boolean value );

  String getResultCategory( );
}
