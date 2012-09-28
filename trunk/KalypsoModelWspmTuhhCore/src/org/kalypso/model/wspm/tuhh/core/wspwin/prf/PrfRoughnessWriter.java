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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
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

  private final IProfile m_profile;

  private final PrfWriter m_prfWriter;

  private final DataBlockWriter m_dbWriter;

  public PrfRoughnessWriter( final PrfWriter prfWriter, final DataBlockWriter dbWriter, final IProfile profile, final String defaultRoughnessType )
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

    return new IComponent[] { Objects.firstNonNull( ks, kst ) };
  }

  private Double getRoughnessFromBuilding( )
  {
    final IProfileBuilding[] buildings = m_profile.getProfileObjects( IProfileBuilding.class );
    return getRoughnessFromDurchlass( buildings );
  }

  private Double getRoughnessFromDurchlass( final IProfileObject[] objects )
  {
    if( ArrayUtils.isEmpty( objects ) )
      return null;

    for( final IProfileObject object : objects )
    {
      final String identifier = object.getId();

      if( BuildingEi.ID.equals( identifier ) )
        return ((BuildingEi)object).getRauheit();

      if( BuildingMaul.ID.equals( identifier ) )
        return ((BuildingMaul)object).getRauheit();

      if( BuildingKreis.ID.equals( identifier ) )
        return ((BuildingKreis)object).getRauheit();

      if( BuildingTrapez.ID.equals( identifier ) )
        return ((BuildingTrapez)object).getRauheit();
    }

    return null;
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
        writeCoords( component, dbr, Objects.firstNonNull( building, 0.0 ) );
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
    final IProfileRecord[] points = m_profile.getPoints();

    final List<Double> arrX = new ArrayList<>( points.length );
    final List<Double> arrY = new ArrayList<>( points.length );

    final int indexWidth = m_profile.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );

    for( final IProfileRecord point : points )
    {
      final Double x = (Double)point.getValue( indexWidth );
      final Double roughness = applyFactor( point, getValue( point, component ) );

      arrX.add( x );
      arrY.add( Objects.firstNonNull( roughness, nullValue ) );
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

    final Double factor = (Double)point.getValue( componentFactor );
    if( Objects.isNull( factor ) )
      return value;

    return value * factor;
  }

  private Double getValue( final IProfileRecord point, final IComponent component )
  {
    final Double plainValue = (Double)point.getValue( component );
    if( !m_preferClasses )
      return plainValue;

    return WspmClassifications.findRoughnessValue( point, component, plainValue );
  }

  private void writeEmptyRauheit( )
  {
    final Double building = getRoughnessFromBuilding();

    final DataBlockHeader dbhr = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ); //$NON-NLS-1$
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    m_prfWriter.writeCoords( null, dbr, Objects.firstNonNull( building, 0.0 ) );
    if( Objects.isNotNull( building ) )
    {
      dbr.getY()[0] = building;
    }

    m_dbWriter.addDataBlock( dbr );
  }

}
