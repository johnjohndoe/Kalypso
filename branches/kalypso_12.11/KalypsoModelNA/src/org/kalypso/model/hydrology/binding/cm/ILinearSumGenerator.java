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

import org.joda.time.LocalTime;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

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
  QName FEATURE_LINEAR_SUM_GENERATOR = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "LinearSumGenerator" ); //$NON-NLS-1$

  /**
   * The qname of the comment.
   */
  QName PROPERTY_COMMENT = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "comment" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the timestep.
   */
  QName PROPERTY_TIMESTEP = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "timestep" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the timestamp.
   */
  QName PROPERTY_TIMESTAMP = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "timestamp" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the area name property.
   */
  QName PROPERTY_AREA_NAME = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "areaNameProperty" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the area description property.
   */
  QName PROPERTY_AREA_DESCRIPTION = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "areaDescriptionProperty" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the area property.
   */
  QName PROPERTY_AREA = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "areaProperty" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the catchment member.
   */
  QName MEMBER_CATCHMENT = new QName( NaModelConstants.NS_CATCHMENT_MODEL, "catchmentMember" ); // $NON-NLS-1$ //$NON-NLS-1$

  void setValidityRange( DateRange validityRange );

  /**
   * This function returns the comment.
   * 
   * @return The comment.
   */
  String getComment( );

  /**
   * This function sets the comment.
   * 
   * @param comment
   *          The comment.
   */
  void setComment( String comment );

  /**
   * This function returns the timestep.
   * 
   * @return The timestep.
   */
  Integer getTimestep( );

  /**
   * This function sets the timestep.
   * 
   * @param timestep
   *          The timestep.
   */
  void setTimestep( Integer timestep );

  /**
   * This function returns the timestamp for daily values.
   * 
   * @return The timestamp in UTC.
   */
  LocalTime getTimestamp( );

  /**
   * This function sets the timestamp for daily values.
   * 
   * @param timestamp
   *          The timestamp in UTC.
   */
  void setTimestamp( LocalTime timestamp );

  /**
   * This function returns the area name property.
   * 
   * @return The area name property.
   */
  GMLXPath getAreaNamePath( );

  /**
   * This function sets the area name property.
   * 
   * @param path
   *          The area name property.
   */
  void setAreaNamePath( GMLXPath path );

  /**
   * This function returns the area description property.
   * 
   * @return The area description property.
   */
  GMLXPath getAreaDescriptionPath( );

  /**
   * This function sets the area description property.
   * 
   * @param path
   *          The area description property.
   */
  void setAreaDescriptionPath( GMLXPath path );

  /**
   * This function returns the area property.
   * 
   * @return The area property.
   */
  GMLXPath getAreaPath( );

  /**
   * This function sets the area property.
   * 
   * @param path
   *          The area property.
   */
  void setAreaPath( GMLXPath path );

  /**
   * This function returns all catchments.
   * 
   * @return All catchments.
   */
  IFeatureBindingCollection<ICatchment> getCatchments( );

  /**
   * This function adjusts the valdity range using the timestamp.
   */
  void adjustValidities( );

  /**
   * This function returns the last modified timestamp for all last modified values available.<br/>
   * {@link #getLastModified()}<br/>
   * {@link #getLastModifiedTimeseries()}<br/>
   * {@link #getLastModifiedCatchments()}
   * 
   * @return The last modified timestamp.
   */
  long getLastModifiedInput( );

  long getLastModifiedTimeseries( );

  long getLastModifiedCatchments( );
}