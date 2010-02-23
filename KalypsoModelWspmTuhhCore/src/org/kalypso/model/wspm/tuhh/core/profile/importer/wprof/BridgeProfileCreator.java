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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
class BridgeProfileCreator extends GelaendeProfileCreator
{
  private final String m_ukPointsID;

  private final String m_okPointsID;

  private final String m_bridgeWidthPointsID;

  public BridgeProfileCreator( final ProfileData data, final String soilPolygon, final String ukPoints, final String okPoints, final String bridgeWidthPoints, final String description )
  {
    super( description, data, soilPolygon );

    m_ukPointsID = ukPoints;
    m_okPointsID = okPoints;
    m_bridgeWidthPointsID = bridgeWidthPoints;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.GelaendeProfileCreator#configure(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  protected void configure( final IProfil profile ) throws CoreException
  {
    super.configure( profile );

    addUK( profile );

    addOK( profile );

    addBridgeObject( profile );

    cleanup( profile );
  }

  private void addBridgeObject( final IProfil profile )
  {
    final BuildingBruecke bridge = new BuildingBruecke( profile );
    final IWProfPoint widthPoint = findBridgetWidth();
    if( widthPoint != null )
      setWidth( profile, bridge, widthPoint );
    setUWheight( profile, bridge, widthPoint );

    profile.addProfileObjects( new IProfileObject[] { bridge } );
  }

  private void setUWheight( final IProfil profile, final BuildingBruecke bridge, final IWProfPoint widthPoint )
  {
    final double widthPointZ = widthPoint == null ? Double.MAX_VALUE : widthPoint.getValue();
    final IComponent heightComponent = profile.getPointPropertyFor( POINT_PROPERTY_HOEHE );
    final int heightIndex = profile.indexOfProperty( heightComponent );
    final IRecord lowestSoilPoint = ProfilUtil.getMinPoint( profile, heightComponent );
    final double lowestSoilZ = lowestSoilPoint == null ? Double.MAX_VALUE : (Double) lowestSoilPoint.getValue( heightIndex );
    // FIXME: 5cm runter ist zu fix, besser noch mal gegen das uw-Profil prüfen
    final double uwZ = Math.min( lowestSoilZ - 0.02, widthPointZ );
    if( uwZ < Double.MAX_VALUE )
    {
      final BigDecimal uwScaled = new BigDecimal( uwZ ).setScale( 2, BigDecimal.ROUND_HALF_UP );
      bridge.setValueFor( BUILDING_PROPERTY_UNTERWASSER, uwScaled.doubleValue() );
      return;
    }
  }

  private void setWidth( final IProfil profile, final BuildingBruecke bridge, final IWProfPoint widthPoint )
  {
    final GM_Point location = widthPoint.getLocation();

    final GM_Curve profileLine = ProfileFeatureBinding.createProfileSegment( profile, KalypsoDeegreePlugin.getDefault().getCoordinateSystem(), null );
    final double width = location.distance( profileLine );

    final BigDecimal widthScaled = new BigDecimal( width ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    bridge.setValueFor( BUILDING_PROPERTY_BREITE, widthScaled.doubleValue() );
  }

  private IWProfPoint findBridgetWidth( )
  {
    final IWProfPoint[] bridgeWidthPoints = getPoints( m_bridgeWidthPointsID );
    if( bridgeWidthPoints == null || bridgeWidthPoints.length == 0 )
      return null;

    return bridgeWidthPoints[0];
  }

  private void cleanup( final IProfil profile )
  {
    final int heightComponent = profile.indexOfProperty( POINT_PROPERTY_HOEHE );
    final int ukComponent = profile.indexOfProperty( POINT_PROPERTY_UNTERKANTEBRUECKE );
    final int okComponent = profile.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE );

    ProfilUtil.interpolateProperty( profile, heightComponent );
    ProfilUtil.interpolateProperty( profile, ukComponent );
    ProfilUtil.interpolateProperty( profile, okComponent );

    final IRecord[] trennflaechenPoints = cleanupHeights( profile );
    createMarkers( profile, trennflaechenPoints, MARKER_TYP_TRENNFLAECHE );
  }

  private IRecord[] cleanupHeights( final IProfil profile )
  {
    final int ukIndex = profile.indexOfProperty( POINT_PROPERTY_UNTERKANTEBRUECKE );
    final int okIndex = profile.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE );
    final int heightIndex = profile.indexOfProperty( POINT_PROPERTY_HOEHE );
    final double precision = profile.getPointPropertyFor( POINT_PROPERTY_HOEHE ).getPrecision();

    final IRecord[] points = profile.getPoints();
    final List<IRecord> pointsList = Arrays.asList( points );
    final IRecord firstUK = snapFirstToHeight( heightIndex, ukIndex, points, precision );
    snapFirstToHeight( heightIndex, okIndex, points, precision );

    Collections.reverse( pointsList );
    final IRecord lastUK = snapFirstToHeight( heightIndex, ukIndex, points, precision );
    snapFirstToHeight( heightIndex, okIndex, points, precision );

    moveAllAbove( heightIndex, ukIndex, points );
    moveAllAbove( ukIndex, okIndex, points );

    final Collection<IRecord> ukFirstLast = new ArrayList<IRecord>( Arrays.asList( firstUK, lastUK ) );
    CollectionUtils.removeAll( ukFirstLast, Arrays.asList( (IRecord) null ) );
    return ukFirstLast.toArray( new IRecord[ukFirstLast.size()] );
  }

  private IRecord snapFirstToHeight( final int heightIndex, final int toSnapIndex, final IRecord[] points, final double precision )
  {
    IRecord lastPoint = null;
    for( final IRecord point : points )
    {
      final Double toSnap = (Double) point.getValue( toSnapIndex );
      final Double height = (Double) point.getValue( heightIndex );
      if( toSnap != null && height != null )
      {
        final double dist = Math.abs( toSnap - height );
        if( dist > precision && dist < 0.05 )
          point.setValue( toSnapIndex, height );
        else if( dist >= 0.05 )
          return lastPoint;
      }

      lastPoint = point;
    }

    return lastPoint;
  }

  private void moveAllAbove( final int lowerIndex, final int higherIndex, final IRecord[] points )
  {
    for( final IRecord point : points )
    {
      final Double lower = (Double) point.getValue( lowerIndex );
      final Double higher = (Double) point.getValue( higherIndex );
      if( lower != null && higher != null )
      {
        if( (higher - lower) < 0.05 )
          point.setValue( lowerIndex, higher );
      }
    }
  }

  private void addUK( final IProfil profile ) throws CoreException
  {
    final IWProfPoint[] ukPoints = getUkPoints();
    if( ukPoints.length < 2 )
      addDefaultProperty( profile, POINT_PROPERTY_UNTERKANTEBRUECKE, POINT_PROPERTY_HOEHE );
    else
      addBridgeProperty( profile, ukPoints, POINT_PROPERTY_UNTERKANTEBRUECKE );
  }

  protected IWProfPoint[] getUkPoints( )
  {
    final IWProfPoint[] ukPoints = getPoints( m_ukPointsID );
    return ukPoints;
  }

  private void addOK( final IProfil profile ) throws CoreException
  {
    final IWProfPoint[] okPoints = getOkPoints();
    if( okPoints.length < 2 )
      addDefaultProperty( profile, POINT_PROPERTY_OBERKANTEBRUECKE, POINT_PROPERTY_UNTERKANTEBRUECKE );
    else
      addBridgeProperty( profile, okPoints, POINT_PROPERTY_OBERKANTEBRUECKE );
  }

  protected IWProfPoint[] getOkPoints( )
  {
    return getPoints( m_okPointsID );
  }

  private void addDefaultProperty( final IProfil profile, final String propertyToSet, final String propertyToCopy )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    final IComponent componentToSet = provider.getPointProperty( propertyToSet );
    final IComponent componentToCopy = provider.getPointProperty( propertyToCopy );

    profile.addPointProperty( componentToSet, componentToCopy );
  }

  private void addBridgeProperty( final IProfil profile, final IWProfPoint[] bridgePoints, final String bridgeProperty ) throws CoreException
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent component = provider.getPointProperty( bridgeProperty );

    profile.addPointProperty( component );

    // FIXME: Rücksprünge in Brücken sind bei WSPM nicht möglich... es sollte daher irgendwas korrigiert werden...

    try
    {
      final int bridgeIndex = profile.indexOfProperty( bridgeProperty );
      final int commentIndex = profile.indexOfProperty( POINT_PROPERTY_COMMENT );

      for( final IWProfPoint wprofPoint : bridgePoints )
      {
        final BigDecimal distance = wprofPoint.getDistance();
        final double value = wprofPoint.getValue();
        final String comment = wprofPoint.getComment();

        final IRecord point = findOrInsertPointAt( profile, distance, bridgeIndex );

        point.setValue( bridgeIndex, value );

        // for( final String markerID : markerIDs )
        // {
        // final IProfilPointMarker marker = profil.createPointMarker( markerID, point );
        // final Object defaultValue = provider.getDefaultValue( markerID );
        // marker.setValue( defaultValue );
        // }

        if( comment != null && !comment.isEmpty() )
        {
          final String existingComment = (String) point.getValue( commentIndex );
          final String commentToSet = existingComment == null ? comment : existingComment + " - " + comment; //$NON-NLS-1$
          point.setValue( commentIndex, commentToSet );
        }
      }
    }
    catch( final Exception e )
    {
      final String message = String.format( "Unable to create profile at %.4f", profile.getStation() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private IRecord findOrInsertPointAt( final IProfil profile, final BigDecimal distance, final int buildPropertyIndex )
  {
    final int indexOfDistance = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    final IRecord existingPoint = ProfilUtil.findPoint( profile, distance.doubleValue(), 0.00001 );
    if( existingPoint != null )
    {
      final Object buildingValue = existingPoint.getValue( buildPropertyIndex );
      if( buildingValue == null )
        return existingPoint;
    }

    // If no point with this width exist or if it already has the buildingProperty, create a new one:
    final IRecord newPoint = profile.createProfilPoint();
    newPoint.setValue( indexOfDistance, new Double( distance.doubleValue() ) );

    final IRecord pointBefore = ProfilUtil.getPointBefore( profile, distance.doubleValue() );
    ProfilUtil.insertPoint( profile, newPoint, pointBefore );
    return newPoint;
  }

  @Override
  protected void addMarker( final IProfil profile )
  {
    // Keine trennflächen: they will be set to the bridges edges
    // Keine Bordvollpunkte: verboten bei Brücken
// addMarker( profile, MARKER_TYP_BORDVOLL );
    addMarker( profile, MARKER_TYP_DURCHSTROEMTE );
  }


}
