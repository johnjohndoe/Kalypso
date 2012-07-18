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
package org.kalypso.model.hydrology.binding.model.nodes;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.INaModelFeature;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;

import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public interface INode extends INaModelFeature
{
  QName FEATURE_NODE = new QName( NaModelConstants.NS_NAMODELL, "Node" ); //$NON-NLS-1$

  QName MEMBER_BRANCHING = new QName( NaModelConstants.NS_NAMODELL, "branchingMember" ); //$NON-NLS-1$

  QName PROPERTY_GENERATE_RESULT = new QName( NaModelConstants.NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  QName PROPERTY_RESULT_CATEGORY = new QName( NaModelConstants.NS_NAMODELL, "resultCategory" ); //$NON-NLS-1$

  QName PROPERTY_LINKED_DOWNSTREAMCHANNEL = new QName( NaModelConstants.NS_NAMODELL, "downStreamChannelMember" ); //$NON-NLS-1$

  QName PROPERTY_MEMBER_VERZWEIGUNG = new QName( NaModelConstants.NS_NAMODELL, "verzweigungNodeMember" ); //$NON-NLS-1$

  QName PROPERTY_NODE_COLLECTION = new QName( NaModelConstants.NS_NAMODELL, "NodeCollection" ); //$NON-NLS-1$

  QName PROPERTY_PEGEL_ZR = new QName( NaModelConstants.NS_NAMODELL, "pegelZR" ); //$NON-NLS-1$

  QName PROPERTY_QQ_RELATED_NODE = new QName( NaModelConstants.NS_NAMODELL, "qqRelatedNode" ); //$NON-NLS-1$

  QName PROPERTY_QQ_RELATION = new QName( NaModelConstants.NS_NAMODELL, "qqRelation" ); //$NON-NLS-1$

  QName PROPERTY_RESULT_AS_INFLOW_ZR = new QName( NaModelConstants.NS_NAMODELL, "resultAsInflowZR" ); //$NON-NLS-1$

  @Deprecated
  QName PROPERTY_RESULT_TIMESERIESLINK = new QName( NaModelConstants.NS_NAMODELL, "qberechnetZR" ); //$NON-NLS-1$

  QName PROPERTY_RIVER_CODE = new QName( NaModelConstants.NS_NAMODELL, "riverCode" ); //$NON-NLS-1$

  QName PROPERTY_RIVER_KM = new QName( NaModelConstants.NS_NAMODELL, "riverKilometer" ); //$NON-NLS-1$

  QName PROPERTY_SYNTHETIC_ZUFLUSS_ZR = new QName( NaModelConstants.NS_NAMODELL, "syntheticZuflussZR" ); //$NON-NLS-1$

  QName PROPERTY_USE_RESULT_AS_INFLOW = new QName( NaModelConstants.NS_NAMODELL, "useResultAsInflow" ); //$NON-NLS-1$

  QName PROPERTY_VERZWEIGUNG = new QName( NaModelConstants.NS_NAMODELL, "Verzweigung" ); //$NON-NLS-1$

  QName PROPERTY_VERZWEIGUNG_ENTNAHME = new QName( NaModelConstants.NS_NAMODELL, "KontEntnahme" ); //$NON-NLS-1$

  QName PROPERTY_VERZWEIGUNG_UEBERLAUF = new QName( NaModelConstants.NS_NAMODELL, "Ueberlauf" ); //$NON-NLS-1$

  QName PROPERTY_VERZWEIGUNG_ZUFLUSS = new QName( NaModelConstants.NS_NAMODELL, "KontZufluss" ); //$NON-NLS-1$

  QName PROPERTY_ZUFLUSS_ZR = new QName( NaModelConstants.NS_NAMODELL, "zuflussZR" ); //$NON-NLS-1$

  QName PROPERTY_ZUFLUSS_ZR_REPOSITORY = new QName( NaModelConstants.NS_NAMODELL, "zuflussZRRepository" ); //$NON-NLS-1$

  Channel[] findUpstreamChannels( );

  Branching getBranching( );

  Channel getDownstreamChannel( );

  ZmlLink getPegelLink( );

  Node getQQRelatedNode( );

  ZmlLink getResultAsInflowLink( );

  ZmlLink getResultAsInflowLinkChecked( Node rootNode );

  @Deprecated
  ZmlLink getResultLink( );

  ZmlLink getZuflussLink( );

  boolean isGenerateResults( );

  Boolean isSynteticZufluss( );

  boolean isUseResultAsInflow( );

  void setBranching( Branching branching );

  void setDownstreamChannel( Channel downstreamChannel );

  void setGenerateResults( boolean value );

  void setIsSynteticZufluss( Boolean isSynteticZufluss );

  void setZuflussLink( TimeseriesLinkType zuflussLink );

  GM_Point getPosition( );

  Point getJtsPosition( ) throws GM_Exception;

  String getRiverCode( );

  Double getRiverKm( );

  String getResultCategory( );
}
