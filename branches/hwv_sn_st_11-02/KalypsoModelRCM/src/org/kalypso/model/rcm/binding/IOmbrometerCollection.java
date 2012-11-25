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

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Collection of ombrometers, binding for ombr:OmbrometerCollection.<br>
 * We do not put the ombrometer processing code (like Thiessen) into this interface or it's implementation, as we want
 * the code to be used with any collection of ombrometers. Use this one just for its special parameters.
 * 
 * @author Gernot Belger
 */
public interface IOmbrometerCollection extends Feature
{
  enum GenerationStrategy
  {
    thiessen,
    thiessen_idw
  }

  QName FEATURE_OMBROMETERCollection = new QName( UrlCatalogRcm.NS_OMBROMETER, "OmbrometerCollection" ); //$NON-NLS-1$

  QName PROPERTY_HASBEENPROCESSED = new QName( UrlCatalogRcm.NS_OMBROMETER, "hasBeenProcessed" ); //$NON-NLS-1$

  QName PROPERTY_GENERATION_STRATEG = new QName( UrlCatalogRcm.NS_OMBROMETER, "generationStrategy" ); //$NON-NLS-1$

  QName MEMBER_OMBROMETER = new QName( UrlCatalogRcm.NS_OMBROMETER, "ombrometerMember" );

  boolean hasBeenProcessed( );

  void setHasBeenProcessed( final boolean hasBeenProcessed );

  FeatureChange[] changeIsUsed( final Boolean isUsed );

  List<IOmbrometer> getOmbrometers( );

  GenerationStrategy getGenerationStrategy( );
}