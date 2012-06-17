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
package org.kalypso.model.hydrology.binding;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * @author Gernot Belger
 */
public interface IHydrotope extends Feature
{
  String NS_NAHYDROTOP = NaModelConstants.NS_NAHYDROTOP;

  QName FEATURE_HYDROTOPE = new QName( NS_NAHYDROTOP, "Hydrotope" ); //$NON-NLS-1$

  QName PROPERTY_GEOMETRY = new QName( NS_NAHYDROTOP, "position" ); //$NON-NLS-1$

  QName PROPERTY_LANDUSE = new QName( NS_NAHYDROTOP, "landuse" ); //$NON-NLS-1$

  QName PROPERTY_SOILTYPE = new QName( NS_NAHYDROTOP, "soiltype" ); //$NON-NLS-1$

  QName PROPERTY_CORR_SEALING = new QName( NS_NAHYDROTOP, "corrSealing" ); //$NON-NLS-1$

  QName PROPERTY_MAX_PERCOLATION = new QName( NS_NAHYDROTOP, "maxPerc" ); //$NON-NLS-1$

  QName PROPERTY_GW_INFLOW_RATE = new QName( NS_NAHYDROTOP, "gwInflowRate" ); //$NON-NLS-1$

  QName PROPERTY_AREA = new QName( NS_NAHYDROTOP, "area" ); //$NON-NLS-1$

  QName LINK_CATCHMENT = new QName( NaModelConstants.NS_NAMODELL, "catchmentLinkMember" ); //$NON-NLS-1$

  QName LINK_LANDUSE = new QName( NaModelConstants.NS_NAHYDROTOP, "landuseLink" ); //$NON-NLS-1$

  QName LINK_PEDOLOGY = new QName( NaModelConstants.NS_NAHYDROTOP, "pedologyLink" ); //$NON-NLS-1$

  QName LINK_GEOLOGY = new QName( NaModelConstants.NS_NAHYDROTOP, "geologyLink" ); //$NON-NLS-1$

  QName LINK_OVERLAY = new QName( NaModelConstants.NS_NAHYDROTOP, "overlayLink" ); //$NON-NLS-1$

  GM_MultiSurface getGeometry( );

  void setGeometry( final GM_MultiSurface geometry );

  String getLanduse( );

  void setLanduse( final String value );

  String getSoilType( );

  void setSoilType( final String value );

  Double getCorrSealing( );

  void setCorrSealing( final double value );

  Double getMaxPerkolationRate( );

  void setMaxPerkolationRate( final double value );

  Double getGWFactor( );

  void setGWFactor( final double value );

  IXLinkedFeature getCatchmentLink( );

  void setCatchmentLink( final String href );

  IXLinkedFeature getLanduseLink( );

  void setLanduseLink( final String href );

  IXLinkedFeature getPedologyLink( );

  void setPedologyLink( final String href );

  IXLinkedFeature getGeologyLink( );

  void setGeologyLink( final String href );

  IXLinkedFeature getOverlayLink( );

  void setOverlayLink( final String href );

  Double getArea( );
}