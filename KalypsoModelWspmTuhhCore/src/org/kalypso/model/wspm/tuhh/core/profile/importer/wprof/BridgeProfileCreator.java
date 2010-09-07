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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
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

  private IWProfPoint[] m_ukPoints;

  private IWProfPoint[] m_okPoints;

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
    setBridgeDefaultValues( bridge );

    final IWProfPoint widthPoint = findBridgetWidth();
    if( widthPoint != null )
      setWidth( profile, bridge, widthPoint );
    setUWheight( profile, bridge, widthPoint );

    profile.addProfileObjects( new IProfileObject[] { bridge } );
  }

  private void setBridgeDefaultValues( final BuildingBruecke bridge )
  {
    bridge.setValueFor( BUILDING_PROPERTY_FORMBEIWERT, new Double( 0.0 ) );
  }

  private void setUWheight( final IProfil profile, final BuildingBruecke bridge, final IWProfPoint widthPoint )
  {
    final double widthPointZ = widthPoint == null ? Double.MAX_VALUE : widthPoint.getValue();
    final IComponent heightComponent = profile.getPointPropertyFor( POINT_PROPERTY_HOEHE );
    final int heightIndex = profile.indexOfProperty( heightComponent );
    final IRecord lowestSoilPoint = ProfilUtil.getMinPoint( profile, heightComponent );
    final double lowestSoilZ = lowestSoilPoint == null ? Double.MAX_VALUE : (Double) lowestSoilPoint.getValue( heightIndex );
    // FIXME: 2cm runter ist zu fix, besser noch mal gegen das uw-Profil prüfen
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

    final GM_Curve profileLine = WspmGeometryUtilities.createProfileSegment( profile, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    if( profileLine != null )
    {
      final double width = location.distance( profileLine );
      final BigDecimal widthScaled = new BigDecimal( width ).setScale( 2, BigDecimal.ROUND_HALF_UP );
      bridge.setValueFor( BUILDING_PROPERTY_BREITE, widthScaled.doubleValue() );
    }
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
// final int okComponent = profile.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE );

    ProfilUtil.interpolateProperty( profile, heightComponent );
    // TODO: maybe optional?
    // Actually, interpolation is not necessary for kalypso-1d.exe
// ProfilUtil.interpolateProperty( profile, ukComponent );
// ProfilUtil.interpolateProperty( profile, okComponent );

    cleanupHeights( profile );

    final IRecord[] trennflaechenPoints = findFirstLast( profile, heightComponent, ukComponent );
    createMarkers( profile, trennflaechenPoints, MARKER_TYP_TRENNFLAECHE );
  }

  private IRecord[] findFirstLast( final IProfil profile, final int heightComponent, final int bridgeComponent )
  {
    final Collection<IRecord> firstLast = new ArrayList<IRecord>( 2 );

    final List<IRecord> points = Arrays.asList( profile.getPoints() );
    final IRecord firstPoint = findFirstBridgePoint( points, heightComponent, bridgeComponent );
    if( firstPoint != null )
      firstLast.add( firstPoint );

    Collections.reverse( points );

    final IRecord lastPoint = findFirstBridgePoint( points, heightComponent, bridgeComponent );
    if( lastPoint != null )
      firstLast.add( lastPoint );

    return firstLast.toArray( new IRecord[firstLast.size()] );
  }

  private IRecord findFirstBridgePoint( final Collection<IRecord> points, final int heightComponent, final int bridgeComponent )
  {
    if( points.size() == 0 )
      return null;

    IRecord lastBridgePoint = points.iterator().next();
    for( final IRecord point : points )
    {
      final Object bridgeValue = point.getValue( bridgeComponent );
      final Object heightValue = point.getValue( heightComponent );

      if( bridgeValue instanceof Number && heightValue instanceof Number )
      {
        final double bridgeHeight = ((Number) bridgeValue).doubleValue();
        final double heightHeight = ((Number) heightValue).doubleValue();
        if( bridgeHeight > heightHeight )
          return lastBridgePoint;
      }

      lastBridgePoint = point;
    }

    return null;
  }

  private void cleanupHeights( final IProfil profile )
  {
    final int ukIndex = profile.indexOfProperty( POINT_PROPERTY_UNTERKANTEBRUECKE );
    final int okIndex = profile.indexOfProperty( POINT_PROPERTY_OBERKANTEBRUECKE );
    final int heightIndex = profile.indexOfProperty( POINT_PROPERTY_HOEHE );
    final double precision = profile.getPointPropertyFor( POINT_PROPERTY_HOEHE ).getPrecision();

    final IRecord[] points = profile.getPoints();
    final List<IRecord> pointsList = Arrays.asList( points );
    /* final IRecord firstUK = */snapFirstToHeight( heightIndex, ukIndex, points, precision );
    snapFirstToHeight( heightIndex, okIndex, points, precision );

    Collections.reverse( pointsList );
    /* final IRecord lastUK = */snapFirstToHeight( heightIndex, ukIndex, points, precision );
    snapFirstToHeight( heightIndex, okIndex, points, precision );

    moveAllAbove( heightIndex, ukIndex, points );
    moveAllAbove( ukIndex, okIndex, points );
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
    initUkOk();

    return swapBackJumps( m_ukPoints );
  }

  /**
   * Initialize uk/ok: swap uk/ok if ok below ok.
   */
  private void initUkOk( )
  {
    /* Init only once */
    if( m_ukPoints != null )
      return;

    final IWProfPoint[] ukPoints = getPoints( m_ukPointsID );
    final IWProfPoint[] okPoints = getPoints( m_okPointsID );

    /* If only one of them is set, we assume it is uk */
    if( ukPoints == null )
    {
      m_ukPoints = okPoints;
      return;
    }

    if( okPoints == null )
    {
      m_ukPoints = ukPoints;
      return;
    }

    final double maxUk = findMax( ukPoints );
    final double maxOk = findMax( ukPoints );

    /* if uk over ok, we swap */
    if( maxUk > maxOk )
    {
      m_okPoints = ukPoints;
      m_ukPoints = okPoints;
    }
    else
    {
      m_ukPoints = ukPoints;
      m_okPoints = okPoints;
    }
  }

  private double findMax( final IWProfPoint[] points )
  {
    double maxValue = -Double.MAX_VALUE;

    for( final IWProfPoint point : points )
    {
      final double value = point.getValue();
      if( !Double.isNaN( value ) )
        maxValue = Math.max( maxValue, value );
    }

    return maxValue;
  }

  private static final BigDecimal MIN_DISTTANCE = new BigDecimal( "0.0001" ); //$NON-NLS-1$

  private static final double dMAX_BACKJUMP_DISTANCE = 0.30;

  private IWProfPoint[] swapBackJumps( final IWProfPoint[] bridgePoints )
  {
    final BridgePoint[] adjustedPoints = new BridgePoint[bridgePoints.length];
    for( int i = 0; i < adjustedPoints.length; i++ )
      adjustedPoints[i] = new BridgePoint( bridgePoints[i] );

    final boolean adjust = true;
    // TODO: optional?
    if( !adjust )
      return adjustedPoints;

    /* Now we do adjust... */
    for( int i = 0; i < adjustedPoints.length; i++ )
    {
      if( i > 1 )
      {
        final BridgePoint before = adjustedPoints[i - 2];
        final BridgePoint here = adjustedPoints[i - 1];
        final BridgePoint after = adjustedPoints[i];

        final BigDecimal beforeDistance = before.getDistance();
        final BigDecimal hereDistance = here.getDistance();
        final BigDecimal afterDistance = after.getDistance();

        final double backDistance = beforeDistance.subtract( hereDistance ).doubleValue();
        final double frontDistance = hereDistance.subtract( afterDistance ).doubleValue();
        if( backDistance > 0 && backDistance < dMAX_BACKJUMP_DISTANCE )
        {
          if( hereDistance.compareTo( beforeDistance ) <= 0 )
          {
            final BigDecimal newHere = beforeDistance.add( MIN_DISTTANCE );
            if( newHere.compareTo( afterDistance ) < 0 )
              here.setDistance( newHere );
          }
        }
        else if( frontDistance > 0 && frontDistance < dMAX_BACKJUMP_DISTANCE )
        {
          final BigDecimal newHere = afterDistance.subtract( MIN_DISTTANCE );
          if( newHere.compareTo( beforeDistance ) > 0 )
            here.setDistance( newHere );
        }
      }
    }

    return adjustedPoints;
  }

  private void addOK( final IProfil profile ) throws CoreException
  {
    final IWProfPoint[] okPoints = getOkPoints();
    if( okPoints == null || okPoints.length < 2 )
      addDefaultProperty( profile, POINT_PROPERTY_OBERKANTEBRUECKE, POINT_PROPERTY_UNTERKANTEBRUECKE );
    else
      addBridgeProperty( profile, okPoints, POINT_PROPERTY_OBERKANTEBRUECKE );
  }

  protected IWProfPoint[] getOkPoints( )
  {
    initUkOk();

    if( m_okPoints == null )
      return null;

    return swapBackJumps( m_okPoints );
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

    try
    {
      final int bridgeIndex = profile.indexOfProperty( bridgeProperty );
      final int commentIndex = profile.indexOfProperty( POINT_PROPERTY_COMMENT );

      for( final IWProfPoint bridgePoint : bridgePoints )
      {
        final BigDecimal distance = bridgePoint.getDistance();
        final double value = bridgePoint.getValue();
        final String comment = bridgePoint.getComment();

        final IRecord point = findOrInsertPointAt( profile, distance.doubleValue(), bridgeIndex );

        point.setValue( bridgeIndex, value );

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

  private IRecord findOrInsertPointAt( final IProfil profile, final double distance, final int buildPropertyIndex )
  {
    final int indexOfDistance = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    final IRecord existingPoint = ProfilUtil.findPoint( profile, distance, 0.00001 );
    if( existingPoint != null )
    {
      final Object buildingValue = existingPoint.getValue( buildPropertyIndex );
      if( buildingValue == null )
        return existingPoint;
    }

    // If no point with this width exist or if it already has the buildingProperty, create a new one:
    final IRecord newPoint = profile.createProfilPoint();
    newPoint.setValue( indexOfDistance, new Double( distance ) );

    final IRecord pointBefore = ProfilUtil.getPointBefore( profile, distance );
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
