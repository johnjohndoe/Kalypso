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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.Buildings;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * @author Dirk Kuch
 */
public class PrfRoughnessWriter
{
  private final String m_defaultRoughnessType;

  private final IProfil m_profile;

  // private final PrfWriter m_prfWriter;

  private final DataBlockWriter m_dbWriter;

  private final boolean m_preferClasses;

  public PrfRoughnessWriter( final DataBlockWriter dbWriter, final IProfil profile, final String defaultRoughnessType, final boolean preferRoughnessClasses )
  {
// m_prfWriter = prfWriter;
    m_dbWriter = dbWriter;
    m_profile = profile;
    m_defaultRoughnessType = defaultRoughnessType;
    m_preferClasses = preferRoughnessClasses;
  }

  private String[] getRoughnessComponents( )
  {
    final boolean hasClasses = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) != null;
    final boolean hasKs = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ) != null;
    final boolean hasKst = m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST ) != null;

    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( m_defaultRoughnessType ) )
    {
      if( hasClasses || hasKs )
        return new String[] { IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS };
      else
        return new String[] {};
    }

    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( m_defaultRoughnessType ) )
    {
      if( hasClasses || hasKst )
        return new String[] { IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST };
      else
        return new String[] {};
    }

    if( m_profile.indexOfProperty( m_defaultRoughnessType ) != -1 )
      return new String[] { m_defaultRoughnessType };

    // REMARK: if defaultRoughness is not specified, we return what we got (i.e. both, if both is present)

    final Set<String> components = new LinkedHashSet<>();
    if( hasKs || hasClasses )
      components.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );

    if( hasKst || hasClasses )
      components.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );

    return components.toArray( new String[components.size()] );
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
    final String[] componentIDs = getRoughnessComponents();
    if( ArrayUtils.isEmpty( componentIDs ) )
    {
      // makes no sense, if ordered roughness does not exist, do not write a datablock!,
      // writeEmptyRauheit();
    }
    else
    {
      for( final String component : componentIDs )
      {
        final Double building = getRoughnessFromBuilding();

        final DataBlockHeader dbhr = PrfHeaders.createHeader( component ); //$NON-NLS-1$
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

  private void writeCoords( final String componentID, final CoordDataBlock db, final Double nullValue )
  {
    final IProfileRecord[] points = m_profile.getPoints();

    final List<Double> arrX = new ArrayList<Double>( points.length );
    final List<Double> arrY = new ArrayList<Double>( points.length );

    final int indexWidth = m_profile.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );

    for( final IProfileRecord point : points )
    {
      final Double x = (Double) point.getValue( indexWidth );
      final BigDecimal roughness = WspmClassifications.getRoughnessValue( point, componentID, m_preferClasses );

      arrX.add( x );
      arrY.add( ((Number) Objects.firstNonNull( roughness, nullValue )).doubleValue() );
    }

    final Double[] xArray = arrX.toArray( new Double[arrX.size()] );
    final Double[] yArray = arrY.toArray( new Double[arrY.size()] );
    db.setCoords( xArray, yArray );
  }

// private void writeEmptyRauheit( )
// {
// final Double building = getRoughnessFromBuilding();
//
//    final DataBlockHeader dbhr = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ); //$NON-NLS-1$
// final CoordDataBlock dbr = new CoordDataBlock( dbhr );
// m_prfWriter.writeCoords( null, dbr, Objects.firstNonNull( building, 0.0 ) );
// if( Objects.isNotNull( building ) )
// {
// dbr.getY()[0] = building;
// }
//
// m_dbWriter.addDataBlock( dbr );
// }
}