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
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractSwale extends AbstractSud implements IAbstractSwale
{
  public AbstractSwale( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getTotalAreaPercentage()
   */
  @Override
  public double getNaturalAreaPercentage( )
  {
    return (Double) getProperty( QN_PROPERTY_NATURAL_AREA_PERCENTAGE );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getSealedAreaPercentage()
   */
  @Override
  public double getDrainedPercentageOfSealedArea( )
  {
    return (Double) getProperty( QN_PROPERTY_DRAINED_SEALED_AREA_PERCENTAGE );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getDrainageNode()
   */
  @Override
  public Feature getDrainageNode( )
  {
    final Object property = getProperty( QN_PROPERTY_DRAINAGE_NODE );
    if( property instanceof Feature )
      return (Feature) property;

    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getMaxCapacityEmergencySpill()
   */
  @Override
  public double getMaxCapacityEmergencySpill( )
  {
    final Object property = getProperty( QN_PROPERTY_MAX_CAP_EMERGENCY_SPILL );
    if( property instanceof Double )
      return (Double) property;

    // default value is 0.0 (check schema)
    return 0.0;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getPipeDiameter()
   */
  @Override
  public int getPipeDiameter( )
  {
    final Object property = getProperty( QN_PROPERTY_PIPE_DIAMETER );
    if( property instanceof Integer )
      return (Integer) property;
    return 200;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getPipeKfValue()
   */
  @Override
  public int getPipeKfValue( )
  {
    final Object property = getProperty( QN_PROPERTY_PIPE_KF_VALUE );
    if( property instanceof Integer )
      return (Integer) property;

    return 8000;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getPipeRoughness()
   */
  @Override
  public double getPipeRoughness( )
  {
    final Object property = getProperty( QN_PROPERTY_PIPE_ROUGHNESS );
    if( property instanceof Double )
      return (Double) property;

    return 2.0;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getPipeSlope()
   */
  @Override
  public int getPipeSlope( )
  {
    final Object property = getProperty( QN_PROPERTY_PIPE_SLOPE );
    if( property instanceof Integer )
      return (Integer) property;

    return 3;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getProfileThickness()
   */
  @Override
  public Double getProfileThickness( )
  {
    final Object property = getProperty( QN_PROPERTY_PROFILE_THICKNESS );
    if( property instanceof Double )
      return (Double) property;
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getWidth()
   */
  @Override
  public double getWidth( )
  {
    // width parameter fixed in NA core to 1.8
    return 1.8;
  }

}
