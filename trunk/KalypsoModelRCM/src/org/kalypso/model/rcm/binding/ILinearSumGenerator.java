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
package org.kalypso.model.rcm.binding;

import javax.xml.namespace.QName;

import org.kalypso.model.rcm.RcmConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * The linear sum generator.
 *
 * @author Holger Albert
 */
public interface ILinearSumGenerator extends IRainfallGenerator
{
  /**
   * The qname of the linear sum generator.
   */
  QName FEATURE_LINEAR_SUM_GENERATOR = new QName( RcmConstants.NS_CM, "LinearSumGenerator" );

  /**
   * The qname of the comment.
   */
  QName PROPERTY_COMMENT = new QName( RcmConstants.NS_CM, "comment" );

  /**
   * The qname of the area name property.
   */
  QName PROPERTY_AREA_NAME = new QName( RcmConstants.NS_CM, "areaNameProperty" );

  /**
   * The qname of the area description property.
   */
  QName PROPERTY_AREA_DESCRIPTION = new QName( RcmConstants.NS_CM, "areaDescriptionProperty" );

  /**
   * The qname of the area property.
   */
  QName PROPERTY_AREA = new QName( RcmConstants.NS_CM, "areaProperty" );

  /**
   * The qname of the catchment member.
   */
  QName MEMBER_CATCHMENT = new QName( RcmConstants.NS_CM, "catchmentMember" );

  /**
   * This function returns the comment.
   *
   * @return The comment.
   */
  String getComment( );

  void setComment( String comment );

  /**
   * This function returns the area name property.
   *
   * @return The area name property.
   */
  String getAreaNameProperty( );

  /**
   * This function returns the area description property.
   *
   * @return The area description property.
   */
  String getAreaDescriptionProperty( );

  /**
   * This function returns the area property.
   *
   * @return The area property.
   */
  String getAreaProperty( );

  /**
   * This function returns all catchments.
   *
   * @return All catchments.
   */
  IFeatureBindingCollection<ICatchment> getCatchments( );
}