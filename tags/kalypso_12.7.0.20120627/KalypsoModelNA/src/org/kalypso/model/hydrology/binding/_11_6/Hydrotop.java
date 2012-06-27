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
package org.kalypso.model.hydrology.binding._11_6;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for {http://www.tuhh.de/hydrotop}Hydrotop<br/>
 * 
 * @author Dejan Antanaskovic
 */
@Deprecated
public class Hydrotop extends Feature_Impl implements IHydrotope
{
  public Hydrotop( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public GM_MultiSurface getGeometry( )
  {
    return getProperty( PROPERTY_GEOMETRY, GM_MultiSurface.class );
  }

  @Override
  public void setGeometry( final GM_MultiSurface geometry )
  {
    setProperty( PROPERTY_GEOMETRY, geometry );
  }

  @Override
  public String getLanduse( )
  {
    return getProperty( PROPERTY_LANDUSE, String.class );
  }

  @Override
  public void setLanduse( final String href )
  {
    setProperty( PROPERTY_LANDUSE, href );
  }

  @Override
  public String getSoilType( )
  {
    return getProperty( PROPERTY_SOILTYPE, String.class );
  }

  @Override
  public void setSoilType( final String value )
  {
    setProperty( PROPERTY_SOILTYPE, value );
  }

  @Override
  public double getCorrSealing( )
  {
    return getProperty( PROPERTY_CORR_SEALING, Double.class );
  }

  @Override
  public void setCorrSealing( final double value )
  {
    setProperty( PROPERTY_CORR_SEALING, value );
  }

  @Override
  public double getMaxPerkolationRate( )
  {
    return getProperty( PROPERTY_M_PERKM, Double.class );
  }

  @Override
  public void setMaxPerkolationRate( final double value )
  {
    setProperty( PROPERTY_M_PERKM, value );
  }

  @Override
  public double getGWFactor( )
  {
    return getProperty( PROPERTY_M_F1GWS, Double.class );
  }

  @Override
  public void setGWFactor( final double value )
  {
    setProperty( PROPERTY_M_F1GWS, value );
  }

  @Override
  public IXLinkedFeature getCatchmentMember( )
  {
    return getProperty( LINK_CATCHMENT, IXLinkedFeature.class );
  }

  @Override
  public void setCatchmentMember( final String href )
  {
    setLink( LINK_CATCHMENT, href );
  }

  @Override
  public void setDRWBMDefinition( final String href )
  {
    setLink( LINK_DRWBM_DEFINITION, href );
  }

  @Override
  public IXLinkedFeature getDRWBMDefinition( )
  {
    return (IXLinkedFeature) getMember( LINK_DRWBM_DEFINITION );
  }

  @Override
  public Double getArea( )
  {
    return getProperty( PROPERTY_AREA, Double.class );
  }
}