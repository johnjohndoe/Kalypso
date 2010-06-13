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
package org.kalypso.model.wspm.tuhh.core.results;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * TODO: move into separate package
 * 
 * @author belger
 */
public class ProfileInterpolation
{
  private final IProfil m_previousProfile;

  private final IProfil m_nextProfile;

  public ProfileInterpolation( final IProfil previousProfile, final IProfil nextProfile )
  {
    m_previousProfile = previousProfile;
    m_nextProfile = nextProfile;
  }

  /**
   * Interpolates a new profile between two existing ones at the given station.
   */
  public IProfil createProfileAt( final BigDecimal station, final String profileType )
  {
    return interpolate( station, profileType );
  }

  /**
   * Interpolates between the two existing profiles and fills the result into the given feature, which should be empty
   * by preference.
   */
  public IProfil interpolate( final BigDecimal station, final String profileType )
  {
    final IProfil profile = ProfilFactory.createProfil( profileType );

    profile.setStation( station.doubleValue() );

    final Object prevCrs = m_previousProfile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    profile.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, prevCrs );

    interpolateProfile( profile );
    /* update profile: add durchstroemte bereiche, trennflaechen */
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IRecord[] points = profile.getPoints();
    if( points.length > 1 )
    {
      final Object defaultValue = provider.getDefaultValue( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
      profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, points[0] ).setValue( defaultValue );
      profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, points[points.length - 1] ).setValue( defaultValue );
    }

    return profile;
  }

  private IProfil interpolateProfile( final IProfil profile )
  {
    try
    {
      if( m_previousProfile == null )
      {
        profile.setDescription( String.format( "Interpolation not possible: downstream profile %s not found.", m_previousProfile.getStation() ) );
        return null;
      }

      if( m_nextProfile == null )
      {
        profile.setDescription( String.format( "Interpolation not possible: upstream profile %s not found.", m_nextProfile.getStation() ) );
        return null;
      }

      final String description = String.format( "Interpolated profile %s - %s", m_previousProfile.getStation(), m_nextProfile.getStation() );
      profile.setName( description );

      return doInterpolation( profile );
    }
    catch( final SameXValuesException e )
    {
      e.printStackTrace();
      profile.setDescription( String.format( "Interpolation failed: %s", e.toString() ) );
      return null;
    }
  }

  private IProfil doInterpolation( final IProfil profil ) throws SameXValuesException
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
    final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    profil.addPointProperty( breiteComponent );

    createInterpolationPoints( profil );

    final IComponent[] prevComponents = m_previousProfile.getPointProperties();
    for( final IComponent prevComponent : prevComponents )
      interpolateComponent( m_previousProfile, m_nextProfile, profil, prevComponent );

    return profil;
  }

  private void createInterpolationPoints( final IProfil profil ) throws SameXValuesException
  {
    final Double[] prevWidths = getPoints( m_previousProfile );
    final Double[] nextWidths = getPoints( m_nextProfile );
    if( prevWidths.length < 2 )
    {
      profil.setComment( "Interpolation not possible: downstream profile doesn not contain enough points." );
      return;
    }
    if( nextWidths.length < 2 )
    {
      profil.setComment( "Interpolation not possible: upstream profile does not contain enough points." );
      return;
    }

    final IComponent widthComponent = profil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_BREITE );

    final Set<BigDecimal> newXValues = collectNewXValues( profil, prevWidths, nextWidths, widthComponent );

    final int widthComponentIndex = profil.indexOfProperty( widthComponent );
    for( final BigDecimal newXValue : newXValues )
    {
      final IRecord newPoint = profil.createProfilPoint();
      newPoint.setValue( widthComponentIndex, newXValue.doubleValue() );
      profil.addPoint( newPoint );
    }
  }

  private Set<BigDecimal> collectNewXValues( final IProfil profil, final Double[] prevWidths, final Double[] nextWidths, final IComponent widthComponent ) throws SameXValuesException
  {
    final double prevStation = m_previousProfile.getStation();
    final double nextStation = m_nextProfile.getStation();
    final double station = profil.getStation();

    final Double startPrevWidth = prevWidths[0];
    final Double startNextWidth = nextWidths[0];
    final Double endPrevWidth = prevWidths[prevWidths.length - 1];
    final Double endNextWidth = nextWidths[nextWidths.length - 1];

    /* Start and end x of new profile */
    final double startWidth = new LinearEquation( prevStation, startPrevWidth, nextStation, startNextWidth ).computeY( station );
    final double endWidth = new LinearEquation( prevStation, endPrevWidth, nextStation, endNextWidth ).computeY( station );

    final int xScale = ComponentUtilities.getScale( widthComponent );

    final Set<BigDecimal> newXValues = new TreeSet<BigDecimal>();
    for( final Double prevWidth : prevWidths )
    {
      final double newx = new LinearEquation( startPrevWidth, startWidth, endPrevWidth, endWidth ).computeY( prevWidth );
      newXValues.add( new BigDecimal( newx ).setScale( xScale, BigDecimal.ROUND_HALF_UP ) );
    }

    for( final Double nextWidth : nextWidths )
    {
      final double newx = new LinearEquation( startNextWidth, startWidth, endNextWidth, endWidth ).computeY( nextWidth );
      newXValues.add( new BigDecimal( newx ).setScale( xScale, BigDecimal.ROUND_HALF_UP ) );
    }
    return newXValues;
  }

  private Double[] getPoints( final IProfil profil )
  {
    // TODO: do we always want to have trennflaechen here?
    final IComponent marker = profil.getPointPropertyFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final List<IRecord> innerPoints = ProfilUtil.getInnerPoints( profil, marker );
    if( innerPoints == null )
      return new Double[0];

    final IRecord[] points = innerPoints.toArray( new IRecord[innerPoints.size()] );
    return ProfilUtil.getValuesFor( points, IWspmConstants.POINT_PROPERTY_BREITE, Double.class );
  }

  private void interpolateComponent( final IProfil prevProfil, final IProfil nextProfil, final IProfil profil, final IComponent component ) throws SameXValuesException
  {
    final QName valueTypeName = component.getValueTypeName();
    if( !XmlTypes.XS_DOUBLE.equals( valueTypeName ) )
      return;

    final int prevComponentIndex = prevProfil.indexOfProperty( component.getId() );
    final int nextComponentIndex = nextProfil.indexOfProperty( component.getId() );
    // we only interpolate components that exist in both profiles
    if( prevComponentIndex == -1 || nextComponentIndex == -1 )
      return;

    final Double[] prevWidths = getPoints( prevProfil );
    final Double[] nextWidths = getPoints( nextProfil );

    final IRecord[] points = profil.getPoints();
    final int widthComponentIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    final Double startPrevWidth = prevWidths[0];
    final Double startNextWidth = nextWidths[0];
    final Double endPrevWidth = prevWidths[prevWidths.length - 1];
    final Double endNextWidth = nextWidths[nextWidths.length - 1];
    final Double startWidth = (Double) points[0].getValue( widthComponentIndex );
    final Double endWidth = (Double) points[points.length - 1].getValue( widthComponentIndex );

    final double prevStation = prevProfil.getStation();
    final double nextStation = nextProfil.getStation();
    final double station = profil.getStation();

    final LinearEquation prevEquation = new LinearEquation( startPrevWidth, startWidth, endPrevWidth, endWidth );
    final LinearEquation nextEquation = new LinearEquation( startNextWidth, startWidth, endNextWidth, endWidth );

    profil.addPointProperty( component );
    final int componentIndex = profil.indexOfProperty( component );

    for( final IRecord record : points )
    {
      final Double width = (Double) record.getValue( widthComponentIndex );
      final Double prevWidth = prevEquation.computeX( width );
      final Double nextWidth = nextEquation.computeX( width );

      final Double prevValue = WspmProfileHelper.interpolateValue( prevProfil, prevWidth, prevComponentIndex );
      final Double nextValue = WspmProfileHelper.interpolateValue( nextProfil, nextWidth, nextComponentIndex );

      final double interpolatedValue = new LinearEquation( prevStation, prevValue, nextStation, nextValue ).computeY( station );
      record.setValue( componentIndex, interpolatedValue );
    }
  }
}
