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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;

/**
 * @author Dirk Kuch
 */
public class Greenroof extends AbstractSud implements IGreenRoof
{
  public Greenroof( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getAreaPercentage()
   */
  @Override
  public Double getAreaPercentage( )
  {
    final Object property = getProperty( QN_PROPERTY_AREA_PERCENTAGE );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getDrainageLayerPorosity()
   */
  @Override
  public Double getDrainageLayerPorosity( )
  {
    final Object property = getProperty( QN_PROPERTY_DRAINAGE_LAYER_POROSITY );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getElementType()
   */
  @Override
  public String getElementType( )
  {
    return getProperty( QN_PROPERTY_ELEMENT_TYPE, String.class );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getEmergencySpillHeight()
   */
  @Override
  public Double getEmergencySpillHeight( )
  {
    final Object property = getProperty( QN_PROPERTY_EMERGENCY_SPILL_HEIGHT );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getEmergencySpillPipeDiameter()
   */
  @Override
  public Object getEmergencySpillPipeDiameter( )
  {
    return getProperty( QN_PROPERTY_EMERGENCY_SPILL_PIPE_DIAMETER );

  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getEmergencySpillPipeRoughness()
   */
  @Override
  public Double getEmergencySpillPipeRoughness( )
  {
    final Object property = getProperty( QN_PROPERTY_EMERGENCY_SPILL_PIPE_ROUGHNESS );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getRainwaterPipeDiameter()
   */
  @Override
  public Object getRainwaterPipeDiameter( )
  {
    return getProperty( QN_PROPERTY_RAINWATER_PIPE_DIAMETER );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getRainwaterPipeRoughness()
   */
  @Override
  public Double getRainwaterPipeRoughness( )
  {
    final Object property = getProperty( QN_PROPERTY_RAINWATER_PIPE_ROUGHNESS );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getSlope()
   */
  @Override
  public Double getSlope( )
  {
    final Object property = getProperty( QN_PROPERTY_SLOPE );
    if( property instanceof Double )
      return (Double) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getUsageType()
   */
  @Override
  public EUsageType getUsageType( )
  {
    final String property = getProperty( QN_PROPERTY_USAGE_TYPE ).toString();
    if( EUsageType.INTENSIVE.value().equals( property ) )
      return EUsageType.INTENSIVE;
    return EUsageType.EXTENSIVE;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof#getIdealLanduseName()
   */
  @Override
  public String getIdealLanduseName( )
  {
    return isExtensiveUsageType() ? IDEAL_LANDUSE_EXTENSIVE : IDEAL_LANDUSE_INTENSIVE;
  }

  public boolean isExtensiveUsageType( )
  {
    return EUsageType.EXTENSIVE == getUsageType();
  }

}
