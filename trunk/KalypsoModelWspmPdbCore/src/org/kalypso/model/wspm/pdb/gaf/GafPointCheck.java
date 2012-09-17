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
package org.kalypso.model.wspm.pdb.gaf;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.internal.gaf.GafLine;

/**
 * Checks various attributes of a {@link org.kalypso.model.wspm.pdb.internal.gaf.GafPoint}.
 *
 * @author Gernot Belger
 */
public class GafPointCheck
{
  private final Set<String> m_unknownCodes = new HashSet<>();

  private final Set<String> m_unknownHyks = new HashSet<>();

  private final Set<String> m_unknownRoughnesses = new HashSet<>();

  private final Set<String> m_unknownVegetations = new HashSet<>();

  private final Map<String, GafCode> m_codeMapping = new HashMap<>();

  private final Map<String, GafCode> m_hykMapping = new HashMap<>();

  private final Map<String, Roughness> m_roughnesMapping = new HashMap<>();

  private final Map<String, Vegetation> m_vegetationMapping = new HashMap<>();

  private final GafCodes m_codes;

  private final ICoefficients m_coefficients;

  public GafPointCheck( final GafCodes codes, final ICoefficients coefficients )
  {
    m_codes = codes;
    m_coefficients = coefficients;
  }

  /**
   * Resets this checker, but keeps already assigned mappings.
   */
  public void cleanup( )
  {
    /* Code */
    for( final Iterator<Entry<String, GafCode>> iterator = m_codeMapping.entrySet().iterator(); iterator.hasNext(); )
    {
      final Entry<String, GafCode> entry = iterator.next();
      if( entry.getValue() == null )
        iterator.remove();
    }
    final GafCode[] availableCodes = getAvailableCodes();
    for( final GafCode availableCode : availableCodes )
      setCodeMapping( availableCode.getCode(), availableCode );

    /* Hyk */
    for( final Iterator<Entry<String, GafCode>> iterator = m_hykMapping.entrySet().iterator(); iterator.hasNext(); )
    {
      final Entry<String, GafCode> entry = iterator.next();
      if( entry.getValue() == null )
        iterator.remove();
    }
    final GafCode[] availableHyks = getAvailableHyks();
    for( final GafCode availableHyk : availableHyks )
      setHykMapping( availableHyk.getHyk(), availableHyk );

    /* Roughness */
    for( final Iterator<Entry<String, Roughness>> iterator = m_roughnesMapping.entrySet().iterator(); iterator.hasNext(); )
    {
      final Entry<String, Roughness> entry = iterator.next();
      if( entry.getValue() == null )
        iterator.remove();
    }
    final Roughness[] availableRoughness = getAvailableRoughness();
    for( final Roughness availableRoughnes : availableRoughness )
      setRoughnessMapping( availableRoughnes.getId().getName(), availableRoughnes );

    /* Vegetation */
    for( final Iterator<Entry<String, Vegetation>> iterator = m_vegetationMapping.entrySet().iterator(); iterator.hasNext(); )
    {
      final Entry<String, Vegetation> entry = iterator.next();
      if( entry.getValue() == null )
        iterator.remove();
    }
    final Vegetation[] availableVegetations = getAvailableVegetation();
    for( final Vegetation availableVegetation : availableVegetations )
      setVegetationMapping( availableVegetation.getId().getName(), availableVegetation );

    m_unknownCodes.clear();
    m_unknownHyks.clear();
    m_unknownRoughnesses.clear();
    m_unknownVegetations.clear();
  }

  public void check( final GafLine line )
  {
    checkCode( line.getCode() );
    checkHyk( line.getHyk() );
    checkRoughness( line.getRoughnessClass() );
    checkVegetation( line.getVegetationClass() );
  }

  private void checkCode( final String code )
  {
    final GafCode gafCode = m_codes.getCode( code );
    if( gafCode == null )
      m_unknownCodes.add( code );
  }

  private void checkHyk( final String hyk )
  {
    /* Empty string is allowed: no-code */
    if( StringUtils.isBlank( hyk ) )
      return;

    final GafCode hykCode = m_codes.getHykCode( hyk );
    if( hykCode == null )
      m_unknownHyks.add( hyk );
  }

  private void checkRoughness( final String roughnessClass )
  {
    final Roughness roughness = m_coefficients.getRoughness( roughnessClass );
    if( roughness == null )
      m_unknownRoughnesses.add( roughnessClass );
  }

  private void checkVegetation( final String vegetationClass )
  {
    final Vegetation vegetation = m_coefficients.getVegetation( vegetationClass );
    if( vegetation == null )
      m_unknownVegetations.add( vegetationClass );
  }

  public GafCode translateCode( final String code )
  {
    return m_codeMapping.get( code );
  }

  public GafCode translateHyk( final String hyk )
  {
    if( StringUtils.isBlank( hyk ) )
      return null;

    return m_hykMapping.get( hyk );
  }

  public Roughness translateRoughness( final String roughnessClass )
  {
    return m_roughnesMapping.get( roughnessClass );
  }

  public Vegetation translateVegetation( final String vegetationClass )
  {
    return m_vegetationMapping.get( vegetationClass );
  }

  public String[] getUnknownCodes( )
  {
    return m_unknownCodes.toArray( new String[m_unknownCodes.size()] );
  }

  public String[] getUnknownHyks( )
  {
    return m_unknownHyks.toArray( new String[m_unknownHyks.size()] );
  }

  public String[] getUnknownRoughnes( )
  {
    return m_unknownRoughnesses.toArray( new String[m_unknownRoughnesses.size()] );
  }

  public String[] getUnknownVegetation( )
  {
    return m_unknownVegetations.toArray( new String[m_unknownVegetations.size()] );
  }

  public GafCode[] getAvailableCodes( )
  {
    return m_codes.getAllCodes();
  }

  public GafCode[] getAvailableHyks( )
  {
    return m_codes.getAllHyks();
  }

  public Roughness[] getAvailableRoughness( )
  {
    return m_coefficients.getAllRoughness();
  }

  public Vegetation[] getAvailableVegetation( )
  {
    return m_coefficients.getAllVegetation();
  }

  public GafCode getCodeMapping( final String code )
  {
    return m_codeMapping.get( code );
  }

  public void setCodeMapping( final String code, final GafCode value )
  {
    m_codeMapping.put( code, value );
  }

  public GafCode getHykMapping( final String code )
  {
    return m_hykMapping.get( code );
  }

  public void setHykMapping( final String code, final GafCode value )
  {
    m_hykMapping.put( code, value );
  }

  public Roughness getRoughnessMapping( final String code )
  {
    return m_roughnesMapping.get( code );
  }

  public void setRoughnessMapping( final String code, final Roughness value )
  {
    m_roughnesMapping.put( code, value );
  }

  public Vegetation getVegetationMapping( final String code )
  {
    return m_vegetationMapping.get( code );
  }

  public void setVegetationMapping( final String code, final Vegetation value )
  {
    m_vegetationMapping.put( code, value );
  }
}