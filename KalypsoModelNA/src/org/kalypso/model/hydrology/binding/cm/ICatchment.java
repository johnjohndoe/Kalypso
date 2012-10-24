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
package org.kalypso.model.hydrology.binding.cm;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * The catchment.
 * 
 * @author Holger Albert
 */
public interface ICatchment extends Feature
{
  /**
   * The qname of the catchment.
   */
  QName FEATURE_CATCHMENT = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "Catchment" ); //$NON-NLS-1$

  /**
   * The qname of the area link.
   */
  QName PROPERTY_AREA_LINK = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "areaLink" ); //$NON-NLS-1$

  /**
   * The qname of the factorized timeseries member.
   */
  QName MEMBER_FACTORIZED_TIMESERIES = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "factorizedTimeseriesMember" ); //$NON-NLS-1$

  /**
   * This function returns the area link.
   * 
   * @return The area link.
   */
  Feature getAreaLink( );

  /**
   * Sets the referenced area.
   */
  void setAreaLink( String href );

  /**
   * This function returns factorized timeseries.
   * 
   * @return All factorized timeseries.
   */
  IFeatureBindingCollection<IFactorizedTimeseries> getFactorizedTimeseries( );

  /**
   * Returns the area from the underlying linked feature via the area property of the generator.
   */
  GM_Polygon resolveArea( );

  /**
   * Returns the name from the underlying linked feature via the name property of the generator.
   */
  String resolveName( );

  /**
   * Returns the description from the underlying linked feature via the description property of the generator.
   */
  String resolveDescription( );

}