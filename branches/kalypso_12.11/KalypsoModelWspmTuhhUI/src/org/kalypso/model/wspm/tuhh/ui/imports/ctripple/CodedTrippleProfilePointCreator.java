/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleHorizonMapper;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfilePoint;
import org.kalypso.observation.result.IComponent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfilePointCreator
{
  private final CodedTrippleHorizonMapper m_mapper;

  private final CodedTrippleProfileHorizon m_baseHorizon;

  private final IProfileFeature m_profileFeature;

  public CodedTrippleProfilePointCreator( final CodedTrippleHorizonMapper mapper, final CodedTrippleProfileHorizon baseHorizon, final IProfileFeature profileFeature )
  {
    m_mapper = mapper;
    m_baseHorizon = baseHorizon;
    m_profileFeature = profileFeature;
  }

  public void createProfilePoints( )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    final IComponent idComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_ID );
    final IComponent commentComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT );
    final IComponent rechtswertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hochwertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent codeComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_CODE );
    final IComponent roughnessComponent = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );
    final IComponent bewuchsComponent = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS );

    final IProfile profil = m_profileFeature.getProfile();
    profil.addPointProperty( idComponent );
    profil.addPointProperty( commentComponent );
    profil.addPointProperty( rechtswertComponent );
    profil.addPointProperty( hochwertComponent );
    profil.addPointProperty( breiteComponent );
    profil.addPointProperty( hoeheComponent );
    profil.addPointProperty( codeComponent );
    profil.addPointProperty( roughnessComponent );
    profil.addPointProperty( bewuchsComponent );

    final IWspmClassification classification = WspmClassifications.getClassification( m_profileFeature.getProfile() );
    final IRoughnessClass unknownRoughnessClass = classification.findUnknownRoughnessClass();
    final IVegetationClass unknownVegetationClass = classification.findUnknownVegetationClass();

    final String unknownRoughness = unknownRoughnessClass == null ? null : unknownRoughnessClass.getName();
    final String unknownVegetation = unknownVegetationClass == null ? null : unknownVegetationClass.getName();

    final CodedTrippleProfilePoint[] points = m_baseHorizon.getProfilePoints();
    for( int i = 0; i < points.length; i++ )
    {
      final CodedTrippleProfilePoint point = points[i];

      final String id = String.format( "%d", i ); //$NON-NLS-1$
      final String comment = m_mapper.getCodeDescription( point.getCode() );
      final double rechtswert = point.getEasting();
      final double hochwert = point.getNorthing();
      final double breite = calculateWidth( i, points, profil.getLastPoint() );
      final double hoehe = point.getHeight();
      final String code = point.getCode();

      final IProfileRecord record = profil.createProfilPoint();
      record.setValue( record.indexOfProperty( idComponent ), id );
      record.setValue( record.indexOfProperty( commentComponent ), comment );
      record.setValue( record.indexOfProperty( rechtswertComponent ), rechtswert );
      record.setValue( record.indexOfProperty( hochwertComponent ), hochwert );
      record.setValue( record.indexOfProperty( breiteComponent ), breite );
      record.setValue( record.indexOfProperty( hoeheComponent ), hoehe );
      record.setValue( record.indexOfProperty( codeComponent ), code );
      record.setValue( record.indexOfProperty( roughnessComponent ), unknownRoughness );
      record.setValue( record.indexOfProperty( bewuchsComponent ), unknownVegetation );

      profil.addPoint( record );
    }

    final int shiftRecordIndex = findShiftRecordIndex( profil.getPoints() );
    if( shiftRecordIndex >= 0 )
    {
      final double shift = getShift( profil.getPoints(), shiftRecordIndex );
      doShift( profil.getPoints(), shift );
    }
  }

  /**
   * @param lastRecord
   *          The last inserted record should be the record for the previous point.
   */
  private double calculateWidth( final int i, final CodedTrippleProfilePoint[] points, final IProfileRecord lastRecord )
  {
    /* If it is the first point, it has a width of 0.0. */
    if( i == 0 )
      return 0.0;

    /* Get the previous and the current point. */
    final CodedTrippleProfilePoint previousPoint = points[i - 1];
    final CodedTrippleProfilePoint currentPoint = points[i];

    /* Create geographic points. */
    final GM_Point previousGMPoint = GeometryFactory.createGM_Point( previousPoint.getEasting(), previousPoint.getNorthing(), m_profileFeature.getSrsName() );
    final GM_Point currentGMPoint = GeometryFactory.createGM_Point( currentPoint.getEasting(), currentPoint.getNorthing(), m_profileFeature.getSrsName() );

    /* Calculate the distance between these to points. */
    final double distance = currentGMPoint.distance( previousGMPoint );

    /* Get the width of the previus point. */
    final Double breite = lastRecord.getBreite();

    /* Add them to the distance of the previous point. */
    final double width = breite.doubleValue() + distance;

    return width;
  }

  private int findShiftRecordIndex( final IProfileRecord[] records )
  {
    final String zeroWidthPointCode = m_mapper.getZeroWidthPointCode();
    if( !StringUtils.isBlank( zeroWidthPointCode ) )
    {
      final int shiftRecordIndex = findZeroWidthRecordIndex( records, zeroWidthPointCode );
      if( shiftRecordIndex >= 0 )
        return shiftRecordIndex;

      return -1;
    }

    return findLowestHeightRecordIndex( records );
  }

  private int findZeroWidthRecordIndex( final IProfileRecord[] records, final String zeroWidthPointCode )
  {
    for( int i = 0; i < records.length; i++ )
    {
      final IProfileRecord record = records[i];

      final String code = record.getCode();
      if( zeroWidthPointCode.equals( code ) )
        return i;
    }

    return -1;
  }

  private int findLowestHeightRecordIndex( final IProfileRecord[] records )
  {
    double currentMinimumHoehe = Double.MAX_VALUE;
    int lowestHeightRecordIndex = -1;

    for( int i = 0; i < records.length; i++ )
    {
      final IProfileRecord record = records[i];

      final Double hoehe = record.getHoehe();
      if( hoehe.doubleValue() < currentMinimumHoehe )
      {
        currentMinimumHoehe = hoehe.doubleValue();
        lowestHeightRecordIndex = i;
      }
    }

    return lowestHeightRecordIndex;
  }

  private double getShift( final IProfileRecord[] records, final int shiftRecordIndex )
  {
    final IProfileRecord record = records[shiftRecordIndex];
    final Double breite = record.getBreite();
    return breite.doubleValue();
  }

  private void doShift( final IProfileRecord[] records, final double shift )
  {
    for( final IProfileRecord record : records )
    {
      final Double breite = record.getBreite();
      final double newBreite = breite.doubleValue() - shift;
      record.setBreite( new Double( newBreite ) );
    }
  }
}