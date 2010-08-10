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
package org.kalypso.model.hydrology.binding.suds;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public interface IAbstractSwale extends Feature
{
  public static QName QN_PROPERTY_NATURAL_AREA_PERCENTAGE = new QName( NaModelConstants.NS_NASUDS, "naturalAreaPercentage" ); // double //$NON-NLS-1$

  public static QName QN_PROPERTY_DRAINED_SEALED_AREA_PERCENTAGE = new QName( NaModelConstants.NS_NASUDS, "drainedSealedAreaPercentage" ); // double //$NON-NLS-1$

  public static QName QN_PROPERTY_DRAINAGE_NODE = new QName( NaModelConstants.NS_NASUDS, "drainageNode" ); // feature //$NON-NLS-1$

  public static QName QN_PROPERTY_PIPE_DIAMETER = new QName( NaModelConstants.NS_NASUDS, "drainagePipeDiameter" ); // suds:EnumPipeDiameter //$NON-NLS-1$

  public static QName QN_PROPERTY_PIPE_KF_VALUE = new QName( NaModelConstants.NS_NASUDS, "drainagePipeKFvalue" ); // integer //$NON-NLS-1$

  public static QName QN_PROPERTY_PIPE_SLOPE = new QName( NaModelConstants.NS_NASUDS, "drainagePipeSlope" ); // integer //$NON-NLS-1$

  public static QName QN_PROPERTY_PIPE_ROUGHNESS = new QName( NaModelConstants.NS_NASUDS, "drainagePipeRoughness" ); // double //$NON-NLS-1$

  public static QName QN_PROPERTY_PROFILE_THICKNESS = new QName( NaModelConstants.NS_NASUDS, "layerProfileThickness" ); // suds:EnumLayerProfileThicknessType //$NON-NLS-1$

  public static QName QN_PROPERTY_MAX_CAP_EMERGENCY_SPILL = new QName( NaModelConstants.NS_NASUDS, "maxCapacityOfEmergencySpill" ); // double //$NON-NLS-1$

  public double getWidth( );

  public double getNaturalAreaPercentage( );

  public double getDrainedPercentageOfSealedArea( );

  public Feature getDrainageNode( );

  public int getPipeDiameter( );

  public int getPipeKfValue( );

  public int getPipeSlope( );

  public double getPipeRoughness( );

  public Double getProfileThickness( );

  public double getMaxCapacityEmergencySpill( );

  public String getIdealLanduseName( );

}
