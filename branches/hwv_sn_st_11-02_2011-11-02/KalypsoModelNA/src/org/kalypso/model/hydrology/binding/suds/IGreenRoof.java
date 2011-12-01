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

/**
 * @author Dirk Kuch
 */
public interface IGreenRoof extends ISudsWithElementType
{
  public static final String IDEAL_LANDUSE_EXTENSIVE = "GRext_N"; //$NON-NLS-1$

  public static final String IDEAL_LANDUSE_INTENSIVE = "GRint_N"; //$NON-NLS-1$

  public static enum EUsageType
  {
    EXTENSIVE("grs_ex"), //$NON-NLS-1$
    INTENSIVE("grs_int"); //$NON-NLS-1$

    private final String m_soilTypeID;

    EUsageType( final String soilTypeID )
    {
      m_soilTypeID = soilTypeID;
    }

    public String value( )
    {
      return name();
    }

    public String getSoilTypeID( )
    {
      return m_soilTypeID;
    }
  }

  public QName QN_PROPERTY_ELEMENT_TYPE = new QName( NaModelConstants.NS_NASUDS, "elementType" ); // suds:EnumGreenRoofType //$NON-NLS-1$

  public QName QN_PROPERTY_AREA_PERCENTAGE = new QName( NaModelConstants.NS_NASUDS, "areaPercentage" ); // double //$NON-NLS-1$

  public QName QN_PROPERTY_SLOPE = new QName( NaModelConstants.NS_NASUDS, "slope" ); // double //$NON-NLS-1$

  public QName QN_PROPERTY_USAGE_TYPE = new QName( NaModelConstants.NS_NASUDS, "usageType" ); // suds:EnumGreenRoofLanduseType //$NON-NLS-1$

  public QName QN_PROPERTY_RAINWATER_PIPE_DIAMETER = new QName( NaModelConstants.NS_NASUDS, "rainwaterPipeDiameter" ); // suds:EnumPipeDiameter //$NON-NLS-1$

  public QName QN_PROPERTY_RAINWATER_PIPE_ROUGHNESS = new QName( NaModelConstants.NS_NASUDS, "rainwaterPipeRoughness" ); // double //$NON-NLS-1$

  public QName QN_PROPERTY_DRAINAGE_LAYER_POROSITY = new QName( NaModelConstants.NS_NASUDS, "drainageLayerPorosity" ); // double //$NON-NLS-1$

  public QName QN_PROPERTY_EMERGENCY_SPILL_PIPE_DIAMETER = new QName( NaModelConstants.NS_NASUDS, "emergencySpillPipeDiameter" ); // suds:EnumPipeDiameter //$NON-NLS-1$

  public QName QN_PROPERTY_EMERGENCY_SPILL_PIPE_ROUGHNESS = new QName( NaModelConstants.NS_NASUDS, "emergencySpillPipeRoughness" ); // double //$NON-NLS-1$

  public QName QN_PROPERTY_EMERGENCY_SPILL_HEIGHT = new QName( NaModelConstants.NS_NASUDS, "emergencySpillHeight" ); // double //$NON-NLS-1$

  public Double getAreaPercentage( );

  public Double getSlope( );

  public EUsageType getUsageType( );

  public String getIdealLanduseName( );

  public Object getRainwaterPipeDiameter( );

  public Double getRainwaterPipeRoughness( );

  public Double getDrainageLayerPorosity( );

  public Object getEmergencySpillPipeDiameter( );

  public Double getEmergencySpillPipeRoughness( );

  public Double getEmergencySpillHeight( );

}
