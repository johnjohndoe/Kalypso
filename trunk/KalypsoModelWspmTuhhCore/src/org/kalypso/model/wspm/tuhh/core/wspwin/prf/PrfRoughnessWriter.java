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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.lang.Strings;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.Buildings;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * @author Dirk Kuch
 */
public class PrfRoughnessWriter
{
  private boolean m_preferClasses;

  private final String m_defaultRoughnessType;

  private final IProfil m_profile;

  private final PrfWriter m_prfWriter;

  private final DataBlockWriter m_dbWriter;

  public PrfRoughnessWriter( final PrfWriter prfWriter, final DataBlockWriter dbWriter, final IProfil profile, final String defaultRoughnessType )
  {
    m_prfWriter = prfWriter;
    m_dbWriter = dbWriter;
    m_profile = profile;
    m_defaultRoughnessType = defaultRoughnessType;
  }

  public void setPreferClasses( final boolean prefereClasses )
  {
    m_preferClasses = prefereClasses;

  }

  private IComponent[] getRoughness( )
  {
    final IComponent defaultRoughness = m_profile.hasPointProperty( m_defaultRoughnessType );
    if( Objects.isNotNull( defaultRoughness ) )
      return new IComponent[] { defaultRoughness };

    final IComponent ks = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent kst = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );

    if( Objects.allNull( ks, kst ) )
      return new IComponent[] {};
    else if( Objects.allNotNull( ks, kst ) )
      return new IComponent[] { ks, kst };

    return new IComponent[] { (IComponent) Objects.firstNonNull( ks, kst ) };
  }

  private Double getRoughnessFromBuilding( )
  {
    final IProfileBuilding[] buildings = m_profile.getProfileObjects( IProfileBuilding.class );
    if( !isDurchlass( buildings ) )
      return null;

    return Buildings.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, buildings[0] );
  }

  private boolean isDurchlass( final IProfileObject[] objects )
  {
    if( ArrayUtils.isEmpty( objects ) )
      return false;

    for( final IProfileObject object : objects )
    {
      final String identifier = object.getId();

      if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( identifier ) )
        return true;
      else if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( identifier ) )
        return true;
      else if( IWspmTuhhConstants.BUILDING_TYP_KREIS.equals( identifier ) )
        return true;
      else if( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ.equals( identifier ) )
        return true;
    }

    return false;
  }

  void writeRauheit( )
  {
    final IComponent[] components = getRoughness();
    if( ArrayUtils.isEmpty( components ) )
    {
      writeEmptyRauheit();
    }
    else
    {
      for( final IComponent component : components )
      {
        final Double building = getRoughnessFromBuilding();

        final DataBlockHeader dbhr = PrfHeaders.createHeader( component.getId() ); //$NON-NLS-1$
        final CoordDataBlock dbr = new CoordDataBlock( dbhr );
        writeCoords( component, dbr, (Double) Objects.firstNonNull( building, 0.0 ) );
        if( Objects.isNotNull( building ) )
        {
          dbr.getY()[0] = building;
        }

        m_dbWriter.addDataBlock( dbr );
      }

    }
  }

  void writeCoords( final IComponent component, final CoordDataBlock db, final Double nullValue )
  {
    final IRecord[] points = m_profile.getPoints();

    final List<Double> arrX = new ArrayList<Double>( points.length );
    final List<Double> arrY = new ArrayList<Double>( points.length );

    final int indexWidth = m_profile.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );

    for( final IRecord point : points )
    {
      final Double x = (Double) point.getValue( indexWidth );
      final Double roughness = applyFactor( point, getValue( point, component ) );

      arrX.add( x );
      arrY.add( (Double) Objects.firstNonNull( roughness, nullValue ) );
    }

    final Double[] xArray = arrX.toArray( new Double[arrX.size()] );
    final Double[] yArray = arrY.toArray( new Double[arrY.size()] );
    db.setCoords( xArray, yArray );
  }

  private Double applyFactor( final IRecord point, final Double value )
  {
    if( Objects.isNull( value ) )
      return null;

    final IComponent componentFactor = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR );
    if( Objects.isNull( componentFactor ) )
      return value;

    final Double factor = (Double) point.getValue( componentFactor );
    if( Objects.isNull( factor ) )
      return value;

    return value * factor;
  }

  private Double getValue( final IRecord point, final IComponent component )
  {
    final Double plainValue = (Double) point.getValue( component );
    if( !m_preferClasses )
      return plainValue;

    final IComponent componentRoughnessClass = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );
    if( Objects.isNull( componentRoughnessClass ) )
      return plainValue;

    final IWspmClassification classification = WspmClassifications.getClassification( m_profile );
    if( Objects.isNull( classification ) )
      return plainValue;

    final String clazzName = (String) point.getValue( componentRoughnessClass );
    if( Strings.isEmpty( clazzName ) )
      return plainValue;

    final IRoughnessClass clazz = classification.findRoughnessClass( clazzName );
    if( Objects.isNull( clazz ) )
      return plainValue;

    final BigDecimal value = clazz.getValue( component.getId() );
    if( Objects.isNotNull( value ) )
      return value.doubleValue();

    return plainValue;
  }

  private void writeEmptyRauheit( )
  {
    final Double building = getRoughnessFromBuilding();

    final DataBlockHeader dbhr = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ); //$NON-NLS-1$
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    m_prfWriter.writeCoords( null, dbr, (Double) Objects.firstNonNull( building, 0.0 ) );
    if( Objects.isNotNull( building ) )
    {
      dbr.getY()[0] = building;
    }

    m_dbWriter.addDataBlock( dbr );
  }

}
