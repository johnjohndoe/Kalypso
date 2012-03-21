/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * @author Gernot Belger
 */
public interface IHydrotope extends Feature
{
  String NS_NAHYDROTOP = NaModelConstants.NS_NAHYDROTOP;

  QName QNAME = new QName( NS_NAHYDROTOP, "Hydrotop" ); //$NON-NLS-1$

  QName PROPERTY_GEOMETRY = new QName( NS_NAHYDROTOP, "position" ); //$NON-NLS-1$

  QName PROPERTY_LANDUSE = new QName( NS_NAHYDROTOP, "landuse" ); //$NON-NLS-1$

  QName PROPERTY_SOILTYPE = new QName( NS_NAHYDROTOP, "soiltype" ); //$NON-NLS-1$

  QName PROPERTY_CORR_SEALING = new QName( NS_NAHYDROTOP, "corrSealing" ); //$NON-NLS-1$

  QName PROPERTY_M_PERKM = new QName( NS_NAHYDROTOP, "m_perkm" ); //$NON-NLS-1$

  QName PROPERTY_M_F1GWS = new QName( NS_NAHYDROTOP, "m_f1gws" ); //$NON-NLS-1$

  QName LINK_SUD = new QName( NaModelConstants.NS_NASUDS, "sudLinkMember" ); //$NON-NLS-1$

  QName LINK_CATCHMENT = new QName( NaModelConstants.NS_NAMODELL, "catchmentLinkMember" ); //$NON-NLS-1$

  IFeatureBindingCollection<ISuds> getSudCollection( );

  Feature[] getSuds( );

  GM_MultiSurface getGeometry( );

  void setGeometry( final GM_MultiSurface geometry );

  String getLanduse( );

  void setLanduse( final String value );

  String getSoilType( );

  void setSoilType( final String value );

  double getCorrSealing( );

  void setCorrSealing( final double value );

  double getMaxPerkolationRate( );

  void setMaxPerkolationRate( final double value );

  double getGWFactor( );

  void setGWFactor( final double value );

  IXLinkedFeature getCatchmentMember( );

  void setCatchmentMember( final String href );

  boolean isEqualByPropertiesWith( final Feature feature );
}