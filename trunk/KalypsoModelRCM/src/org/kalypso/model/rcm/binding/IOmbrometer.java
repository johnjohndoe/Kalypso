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
package org.kalypso.model.rcm.binding;

import java.net.MalformedURLException;

import javax.xml.namespace.QName;

import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * GML-binding for the rcm-ombrometer
 * 
 * @author Gernot Belger
 */
public interface IOmbrometer extends Feature
{
  final static QName QNAME_FEATURE_OMBROMETER = new QName( UrlCatalogRcm.NS_OMBROMETER, "Ombrometer" );

  final static QName QNAME_PROP_SHORTNAME = new QName( UrlCatalogRcm.NS_OMBROMETER, "shortName" );

  final static QName QNAME_PROP_EXTERNALID = new QName( UrlCatalogRcm.NS_OMBROMETER, "externalId" );

  final static QName QNAME_PROP_ISUSED = new QName( UrlCatalogRcm.NS_OMBROMETER, "isUsed" );

  final static QName QNAME_PROP_STATIONLOCATION = new QName( UrlCatalogRcm.NS_OMBROMETER, "stationLocation" );

  final static QName QNAME_PROP_AFFECTEDAREA = new QName( UrlCatalogRcm.NS_OMBROMETER, "affectedArea" );

  static final QName QNAME_PROP_PRECIPITATION1 = new QName( UrlCatalogRcm.NS_OMBROMETER, "precipitationLink1" );

  String getShortName( );

  void setShortName( String shortName );

  String getExternalId( );

  void setExternalId( String externalId );

  boolean isUsed( );

  void setUsed( boolean used );

  FeatureChange changeIsUsed( Boolean isUsed );

  GM_Point getStationLocation( );

  void setStationLocation( GM_Point location );

  GM_Polygon getAffectedArea( );

  void setAffectedArea( GM_Polygon area );

  IObservation getTimeserie( ) throws MalformedURLException, SensorException;

  ZmlLink getTimeserieLink( );
}
