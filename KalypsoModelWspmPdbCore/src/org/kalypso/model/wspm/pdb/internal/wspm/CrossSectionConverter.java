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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.utils.ConsecutiveNumComparator;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class CrossSectionConverter
{
  private final CrossSection m_section;

  private final IProfil m_profile;

  private final IProfilPointPropertyProvider m_provider;

  public CrossSectionConverter( final CrossSection section, final IProfil profile )
  {
    m_section = section;
    m_profile = profile;

    m_provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
  }

  public void execute( )
  {
    m_profile.setName( m_section.getName() );
    m_profile.setDescription( m_section.getDescription() );

    final BigDecimal station = m_section.getStation();
    /* [m] -> [km] */
    m_profile.setStation( station.movePointLeft( 3 ).doubleValue() );

    convertP();
    convertBuilding();
    // TODO: other kinds...
    // Other building types (tubes, ...)
  }

  private void convertP( )
  {
    final CrossSectionPart part = m_section.findPartByCategory( IGafConstants.KZ_CATEGORY_PROFILE );
    if( part == null )
      return;

    // TODO: put part's name, description etc. into profile objects

    final TupleResult result = m_profile.getResult();

    /* Add points in their natural order into the profile */
    final Set<Point> points = part.getPoints();
    final List<Point> sortedPoints = sortPoints( points );
    for( final Point point : sortedPoints )
    {
      final IRecord record = result.createRecord();
      result.add( record );

      setValue( record, IWspmPointProperties.POINT_PROPERTY_BREITE, asDouble( point.getWidth() ) );
      setValue( record, IWspmPointProperties.POINT_PROPERTY_HOEHE, asDouble( point.getHeight() ) );

      convertStandardProperties( point, record );

      // REMARK: we do not add hyk as separate component, it is redundant in any way..., isnt' it?
      // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getHyk() );
      final String hyk = point.getHyk();
      final String markerType = toMarkerType( hyk );
      if( markerType != null )
        createMarker( record, markerType );
    }
  }

  private String toMarkerType( final String hyk )
  {
    if( IGafConstants.HYK_PA.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE;
    if( IGafConstants.HYK_PE.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE;

    if( IGafConstants.HYK_LBOK.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_BORDVOLL;
    if( IGafConstants.HYK_RBOK.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_BORDVOLL;

    if( IGafConstants.HYK_LU.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE;
    if( IGafConstants.HYK_RU.equals( hyk ) )
      return IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE;

    return null;
  }

  protected void createMarker( final IRecord point, final String markerType )
  {
    final IProfilPointMarker marker = m_profile.createPointMarker( markerType, point );
    final Object defaultValue = m_provider.getDefaultValue( markerType );
    marker.setValue( defaultValue );
  }

  private void convertBuilding( )
  {
    final CrossSectionPart ukPart = m_section.findPartByCategory( IGafConstants.KZ_CATEGORY_UK );
    final CrossSectionPart okPart = m_section.findPartByCategory( IGafConstants.KZ_CATEGORY_OK );

    /* Can we do anything with a OK without UK? Maybe this is always a weir? */
    if( ukPart == null && okPart == null )
      return;

    if( ukPart == null )
    {
      insertPointsAs( okPart, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );

      /* We consider this situation as a weir */
      final BuildingWehr weir = new BuildingWehr( m_profile );
      // weir.setValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, new Double( 0.0 ) );
      m_profile.addProfileObjects( new IProfileObject[] { weir } );
    }
    else
    {
      insertPointsAs( ukPart, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
      insertPointsAs( okPart, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );

      final BuildingBruecke bridge = new BuildingBruecke( m_profile );
      bridge.setValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, new Double( 0.0 ) );
      // setUWheight( profile, bridge, widthPoint );
      m_profile.addProfileObjects( new IProfileObject[] { bridge } );
    }
  }

  private Double asDouble( final BigDecimal decimal )
  {
    if( decimal == null )
      return null;

    return decimal.doubleValue();
  }

  private void setValue( final IRecord record, final String componentID, final Object value )
  {
    if( value == null )
      return;

    final int component = ensureComponent( record, componentID );
    record.setValue( component, value );
  }

  private int ensureComponent( final IRecord record, final String componentID )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentID );
    if( index != -1 )
      return index;

    owner.addComponent( m_provider.getPointProperty( componentID ) );
    return owner.indexOfComponent( componentID );
  }

  private List<Point> sortPoints( final Set<Point> points )
  {
    final ArrayList<Point> sortedPoints = new ArrayList<Point>( points );
    Collections.sort( sortedPoints, new ConsecutiveNumComparator() );
    return sortedPoints;
  }

  private void insertPointsAs( final CrossSectionPart part, final String asComponent )
  {
    if( part == null )
      return;

    // TODO: put part's name, description etc. into profile objects

    /* Add points in their natural order into the profile */
    final Set<Point> points = part.getPoints();
    final List<Point> sortedPoints = sortPoints( points );
    for( final Point point : sortedPoints )
    {
      final BigDecimal width = point.getWidth();

      /* Find or insert point at 'width' */
      final IRecord record;
      final IRecord nearestPoint = ProfilUtil.findPointAt( m_profile, width );
      if( nearestPoint == null )
        record = ProfilUtil.insertPointAt( m_profile, width );
      else
        record = nearestPoint;
      final boolean isInserted = nearestPoint == null;

      setValue( record, asComponent, asDouble( point.getHeight() ) );

      // TODO: check: if we have the same width, but different rw/hw we just forget the old rw/hw here, which is bad...
      // TODO: same holds for ID, Code, etc.
      if( isInserted )
        convertStandardProperties( point, record );
    }
  }

  private void convertStandardProperties( final Point point, final IRecord record )
  {
    setValue( record, IWspmPointProperties.POINT_PROPERTY_ID, point.getName() );
    setValue( record, IWspmPointProperties.POINT_PROPERTY_CODE, point.getCode() );
    setValue( record, IWspmPointProperties.POINT_PROPERTY_COMMENT, point.getDescription() );

    final com.vividsolutions.jts.geom.Point location = point.getLocation();
    if( location != null )
    {
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RECHTSWERT, location.getX() );
      setValue( record, IWspmPointProperties.POINT_PROPERTY_HOCHWERT, location.getY() );
    }

    // REMARK: the checkout operation makes sure that all necessary classes are present
    final Roughness roughness = point.getRoughness();
    if( roughness != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS, roughness.getId().getName() );

    final BigDecimal roughnessKst = point.getRoughnessKstValue();
    if( roughnessKst != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, roughnessKst.doubleValue() );

    final BigDecimal roughnessK = point.getRoughnessKValue();
    if( roughnessK != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, roughnessK.doubleValue() );

    // REMARK: the checkout operation makes sure that all necessary classes are present
    final Vegetation vegetation = point.getVegetation();
    if( vegetation != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS, vegetation.getId().getName() );

    final BigDecimal vegetationAx = point.getVegetationAx();
    if( vegetationAx != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX, vegetationAx.doubleValue() );

    final BigDecimal vegetationAy = point.getVegetationAy();
    if( vegetationAy != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY, vegetationAy.doubleValue() );

    final BigDecimal vegetationDp = point.getVegetationDp();
    if( vegetationDp != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP, vegetationDp.doubleValue() );
  }
}