/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.core.profil.util;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.i18n.Messages;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  /**
   * @return the values of each point for this pointProperty in the correct order
   */
  public static Object[] getValuesFor( final IProfil profil, final IComponent pointProperty )
  {
    final IRecord[] points = profil.getPoints();
    final Object[] values = new Object[points.length];
    final int iProp = profil.indexOfProperty( pointProperty );
    if( iProp < 0 )
      return values;
    int i = 0;

    for( final IRecord point : points )
    {
      final Object value = point.getValue( iProp );
      if( value == null )
        Debug.print( point, Messages.ProfilUtil_5 + iProp + "   " + pointProperty.getName() ); //$NON-NLS-2$
      values[i] = value;
      i++;
    }
    return values;
  }

  /**
   * @return the DoubleValues of each point for this pointProperty in the correct order
   */
  public static Object[] getValuesFor( final IRecord[] points, final IComponent pointProperty )
  {
    if( points == null || points.length < 1 )
      return new Object[0];
    final TupleResult owner = points[0].getOwner();
    final Object[] values = new Object[points.length];
    final int iProp = owner.indexOfComponent( pointProperty );
    if( iProp < 0 )
      return values;
    int i = 0;

    for( final IRecord point : points )
    {
      final Object value = point.getValue( iProp );
      if( value == null )
        Debug.print( point, Messages.ProfilUtil_5 + iProp + "   " + pointProperty.getName() ); //$NON-NLS-2$
      values[i] = value;
      i++;
    }
    return values;
  }

  public static Double getDoubleValueFor( final String componentID, final IRecord point )
  {
    final TupleResult owner = point == null ? null : point.getOwner();
    if( owner != null )
    {
      final int iComponent = owner.indexOfComponent( componentID );
      if( iComponent > -1 )
      {
        final Object oValue = point.getValue( iComponent );
        if( oValue instanceof Double )
          return (Double) oValue;
      }
    }
    return Double.NaN;
  }

  public static Double getDoubleValueFor( final String componentID, final IProfileObject building )
  {
    final IComponent property = building.getObjectProperty( componentID );

    try
    {
      final Object value = building.getValue( property );
      if( value == null )
        return Double.NaN;
      if( value instanceof Double )
        return (Double) value;
      return Double.NaN;
    }
    catch( IllegalArgumentException e )
    {
      KalypsoModelWspmCorePlugin.getDefault().getLog().log( new Status( IStatus.ERROR, componentID, e.getLocalizedMessage(), null ) );
      return Double.NaN;
    }

  }

  /**
   * @return a subList include both MarkerPoints, maybe null
   */
  public static final List<IRecord> getInnerPoints( final IProfil profil, final IProfilPointMarker leftMarker, final IProfilPointMarker rightMarker )
  {
    final IRecord[] points = profil.getPoints();
    final int leftPos = leftMarker != null ? ArrayUtils.indexOf( points, leftMarker.getPoint() ) : 0;
    final int rightPos = rightMarker != null ? ArrayUtils.indexOf( points, rightMarker.getPoint() ) + 1 : 0;

    return leftPos < rightPos ? profil.getResult().subList( leftPos, rightPos ) : null;

  }

  public static final List<IRecord> getInnerPoints( final IProfil profil, final IComponent markerTyp )
  {
    final IProfilPointMarker[] markers = profil.getPointMarkerFor( markerTyp );
    final IRecord[] points = profil.getPoints();
    final int leftPos = markers.length > 0 ? ArrayUtils.indexOf( points, markers[0].getPoint() ) : 0;
    final int rightPos = markers.length > 1 ? ArrayUtils.indexOf( points, markers[markers.length - 1].getPoint() ) + 1 : 0;
    return leftPos < rightPos ? profil.getResult().subList( leftPos, rightPos ) : null;
  }

  /**
   * return true if all selected properties are equal
   */
  public static final boolean comparePoints( final IComponent[] properties, final IRecord point1, final IRecord point2 )
  {
    for( final IComponent property : properties )
    {

      final Double x1 = getDoubleValueFor( property.getId(), point1 );
      final Double x2 = getDoubleValueFor( property.getId(), point2 );

      if( x1.isNaN() || x2.isNaN() )
      {

        final int index = point1.getOwner().indexOf( property );
        final Object o1 = point1.getValue( index );
        final Object o2 = point2.getValue( index );
        if( o1 == null || o2 == null || o1.equals( o2 ) )
          continue;
      }
      if( Math.abs( x1 - x2 ) > property.getPrecision() )
        return false;
    }
    return true;
  }

  /**
   * return true if getValueFor(property) is equal
   */
  public static final boolean comparePoints( final IComponent property, final IRecord point1, final IRecord point2 )
  {
    return comparePoints( new IComponent[] { property }, point1, point2 );
  }

  /**
   * mirror the profiles points (axis 0.0)
   */
  public static final void flipProfile( final IProfil profile )
  {
    final HashMap<String, IComponent> properties = getComponentsFromProfile( profile );

    TupleResult result = profile.getResult();
    IRecord[] rows = result.toArray( new IRecord[] {} );
    result.clear();

    final int indexBreite = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    /* flip the profile by re-adding with negative width */
    for( IRecord record : rows )
    {
      Double breite = (Double) record.getValue( indexBreite );
      record.setValue( indexBreite, Double.valueOf( breite * -1 ) );

      WspmProfileHelper.addRecordByWidth( profile, record );
    }

    final IRecord[] points = profile.getPoints();
    IRecord previousPoint = null;
    for( final IRecord point : points )
    {
      if( previousPoint == null )
      {
        previousPoint = point;
        continue;
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) )
      {
        final int i = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        final Double value = (Double) point.getValue( i );
        previousPoint.setValue( i, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) )
      {
        final int i = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
        final Double value = (Double) point.getValue( i );
        previousPoint.setValue( i, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      {
        final int i = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
        final Double value = (Double) point.getValue( i );
        previousPoint.setValue( i, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) )
      {
        final int i = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
        final Double value = (Double) point.getValue( i );
        previousPoint.setValue( i, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) )
      {
        final int i = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );
        final Double value = (Double) point.getValue( i );
        previousPoint.setValue( i, value );
      }
      previousPoint = point;
    }

  }

  public final static HashMap<String, IComponent> getComponentsFromProfile( final IProfil profile )
  {
    final HashMap<String, IComponent> propHash = new HashMap<String, IComponent>();
    for( final IComponent component : profile.getPointProperties() )
      propHash.put( component.getId(), component );
    return propHash;
  }

  public final static HashMap<String, IComponent> getComponentsFromRecord( final IRecord record )
  {
    final HashMap<String, IComponent> propHash = new HashMap<String, IComponent>();
    final TupleResult owner = record.getOwner();
    for( final IComponent component : owner.getComponents() )
      propHash.put( component.getId(), component );
    return propHash;
  }

  public static final IRecord splitSegment( final IProfil profile, final IRecord startPoint, final IRecord endPoint )
  {
    if( startPoint == null || endPoint == null )
      return null;
    final IRecord point = profile.createProfilPoint();
    final IComponent[] properties = profile.getPointProperties();

    for( final IComponent property : properties )
    {
      final int index = profile.indexOfProperty( property );
      if( IWspmConstants.POINT_PROPERTY_BREITE.equals( property.getId() ) )
      {
        final Double b1 = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, startPoint );
        final Double l = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, endPoint ) - b1;
        point.setValue( index, b1 + l / 2.0 );
      }
      else if( IWspmConstants.POINT_PROPERTY_HOEHE.equals( property.getId() ) )
      {
        final Double h1 = (Double) startPoint.getValue( index );
        final Double z = (Double) endPoint.getValue( index ) - h1;
        point.setValue( index, h1 + z / 2.0 );
      }
      else if( profile.isPointMarker( property.getId() ) )
        point.setValue( index, startPoint.getValue( index ) );
    }
    return point;
  }

  /**
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite -> 2.suche Punkt bei
   * breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public static final IRecord findPoint( final IProfil profil, final int index, final double breite, final double delta )
  {
    final IRecord[] points = profil.getPoints();
    final IRecord point = (index >= points.length || index < 0) ? null : points[index];

    if( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) == breite )
      return point;

    return findPoint( profil, breite, delta );

  }

  public static IRecord[] getSegment( final IProfil profile, final double breite )
  {
    final IRecord[] points = profile.getPoints();
    final IRecord[] segment = new IRecord[] { null, null };

    for( int i = 0; i < points.length - 1; i++ )
    {
      final double segmentStartWidth = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i] );
      final double segmentEndWidth = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i + 1] );
      if( segmentStartWidth <= breite & segmentEndWidth >= breite )
      {
        segment[0] = points[i];
        segment[1] = points[i + 1];
      }
    }
    return segment;
  }

  public static IRecord findNearestPoint( final IProfil profil, final double breite )
  {
    final IRecord[] points = profil.getPoints();
    final int index = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    IRecord pkt = points[0];
    if( index < 0 )
      return pkt;
    for( int i = 1; i < points.length; i++ )
    {
      final Double b = (Double) points[i].getValue( index );
      if( Math.abs( b - breite ) < Math.abs( (Double) pkt.getValue( index ) - breite ) )
        pkt = points[i];
    }
    return pkt;
  }

  public static IRecord[] findPoints( final IProfil profile, final String propertyID, final Point2D point, final Double radius )
  {

    final IRecord[] points = profile.getPoints();

    final ArrayList<IRecord> found = new ArrayList<IRecord>();
    for( final IRecord p : points )
    {
      final Point2D p2D = new Point2D.Double( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, p ), getDoubleValueFor( propertyID, p ) );
      if( p2D.distance( point ) <= radius )
        found.add( p );
    }
    return found.toArray( new IRecord[] {} );
  }

  public static IRecord findPoint( final IProfil profil, final double breite, final double delta )
  {
    final IRecord pkt = findNearestPoint( profil, breite );
    final double xpos = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, pkt );
    return Math.abs( xpos - breite ) <= delta ? pkt : null;
  }

  public static IRecord getPointBefore( final IProfil profil, final IRecord point )
  {
    final TupleResult points = profil.getResult();
    if( points.isEmpty() || point == points.get( 0 ) )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new IllegalArgumentException( Messages.ProfilUtil_1 + point );

    return points.get( i - 1 );
  }

  public static IRecord getPointBefore( final IProfil profil, final double breite )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;

    IRecord thePointBefore = null;
    for( final IRecord point : points )
    {
      if( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) > breite )
        return thePointBefore;
      thePointBefore = point;
    }
    return thePointBefore;
  }

  public static IRecord getPointAfter( final IProfil profil, final double breite )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;
    for( final IRecord point : points )
      if( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) > breite )
        return point;
    return null;
  }

  public static IRecord getPointAfter( final IProfil profil, final IRecord point )
  {
    final TupleResult points = profil.getResult();
    final int pos = points.indexOf( point );

    if( points.isEmpty() || pos == points.size() - 1 )
      return null;
    if( pos == -1 )
      throw new IllegalArgumentException( Messages.ProfilUtil_2 + point );

    return points.get( pos + 1 );
  }

  public static Double getMaxValueFor( final IProfil profil, final IComponent property )
  {
    if( property == null || profil == null )
      return null;
    final int index = profil.indexOfProperty( property );
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;
    Double maxValue = Double.MIN_VALUE;
    for( final IRecord point : points )
    {
      final Object o = point.getValue( index );
      if( o instanceof Double )
        maxValue = Math.max( maxValue, (Double) o );

    }
    return maxValue > Double.MIN_VALUE ? maxValue : null;
  }

  public static IComponent getComponentForID( final IComponent[] components, final String propertyID )
  {
    if( components == null || components.length < 1 )
      return null;
    for( final IComponent component : components )
    {
      if( component.getId().equals( propertyID ) )
        return component;
    }
    return null;
  }

  public static Point2D getPoint2D( final IRecord p, final IComponent pointProperty )
  {
    final int index = p.getOwner().indexOf( pointProperty );
    final IComponent breite = ProfilUtil.getComponentForID( p.getOwner().getComponents(), IWspmConstants.POINT_PROPERTY_BREITE );
    final int iBreite = p.getOwner().indexOf( breite );
    final Object x = p.getValue( iBreite );
    final Object y = p.getValue( index );
    if( x == null || y == null || !(x instanceof Double) || !(y instanceof Double) )
      throw new IllegalArgumentException( pointProperty.getName() + Messages.ProfilUtil_7 );
    return new Point2D.Double( (Double) x, (Double) y );

  }

  /**
   * @return null if profil has no points or property does not exists
   * @return always one point (first point if no match)
   */
  public static IRecord findNearestPoint( final IProfil profil, final double breite, final double value, final IComponent property )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;
    IRecord bestPoint = points[0];
    for( final IRecord point : points )
      if( Math.abs( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, bestPoint ) - breite ) > Math.abs( getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) - breite )
          || Math.abs( getDoubleValueFor( property.getId(), bestPoint ) - value ) > Math.abs( getDoubleValueFor( property.getId(), point ) - value ) )
        bestPoint = point;
    return bestPoint;
  }

  /**
   * @return null if profil has no points or property does not exists or nomatch
   */
  public static IRecord[] findPoint( final IProfil profil, final Double[] values, final IComponent[] properties )
  {
    final List<IRecord> points = new ArrayList<IRecord>();
    final int maxIndex = Math.min( values.length, properties.length );
    final IRecord[] profilePoints = profil.getPoints();
    for( final IRecord point : profilePoints )
    {
      boolean isEqual = true;
      for( int i = 0; i < maxIndex; i++ )
      {
        final Double delta = properties[i].getPrecision();

        if( Math.abs( getDoubleValueFor( properties[i].getId(), point ) - values[i] ) > delta )
        {
          isEqual = false;
          break;
        }
      }
      if( isEqual )
        points.add( point );
    }
    return points.toArray( new IRecord[0] );
  }

  /**
   * @deprecated use {@code #getPoints2D(IProfil,String)} instead
   */
  @Deprecated
  public static Point2D[] getPoints2D( final IProfil profil, final IComponent pointProperty )
  {
    return getPoints2D( profil, pointProperty.getId() );
  }

  public static Point2D[] getPoints2D( final IProfil profil, final String propertyID )
  {
    if( profil.hasPointProperty( propertyID ) == null )
      return new Point2D[] {};

    final IRecord[] points = profil.getPoints();
    final Point2D[] points2D = new Point2D[points.length];
    int i = 0;
    for( final IRecord p : points )
    {
      final Double x = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, p );
      final Double y = getDoubleValueFor( propertyID, p );
      if( y.isNaN() || x.isNaN() )
        return new Point2D[] {};
      points2D[i++] = new Point2D.Double( x, y );

    }
    return points2D;
  }

  public static Double getMinValueFor( final IProfil profil, final IComponent property )
  {
    final IRecord minPoint = getMinPoint( profil, property );
    if( minPoint == null )
      return null;
    final Object value = minPoint.getValue( profil.indexOfProperty( property ) );
    return value instanceof Double ? (Double) value : null;
  }

  /**
   * Return the profile-point of the given profile with the minimum value at the given property.
   */
  public static IRecord getMinPoint( final IProfil profil, final IComponent property )
  {
    if( profil == null )
      return null;
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;

    Double minValue = Double.MAX_VALUE;
    IRecord minPoint = null;
    final int index = profil.indexOfProperty( property );
    if( index == -1 )
      return null;

    for( final IRecord point : points )
    {
      final Object value = point.getValue( index );
      if( (value instanceof Double) && (Double) value < minValue )
      {
        minValue = (Double) value;
        minPoint = point;
      }
    }
    return minPoint;
  }

  /**
   * return a valid ProfilPoint if operation succeeds, otherwise null
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IRecords#addPoint(double, double)
   */
  public static final IRecord addPoint( final IProfil profil, final double breite, final double hoehe )
  {

    final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int iBreite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    if( iBreite >= 0 && iHoehe >= 0 )
    {
      final IRecord point = profil.createProfilPoint();
      point.setValue( iHoehe, hoehe );
      point.setValue( iBreite, breite );
      if( profil.addPoint( point ) )
        return point;
    }
    return null;
  }

  public static final void insertPoint( final IProfil profil, final IRecord point, final IRecord thePointBefore )
  {
    final TupleResult points = profil.getResult();
    final int index = thePointBefore == null ? 0 : points.indexOf( thePointBefore ) + 1;
    points.add( index, point );
  }

  /**
   * calculates the area of a given profile for the region between two given profile widths.<br>
   * the area is calculatated in dependence of the max heigth value. input: IProfil, start width, end width<br>
   * output: area between the two widths<br>
   */
  public static final double calcArea( final IProfil profil, final double startWidth, final double endWidth )
  {
    double area = 0;
    double width = 0;
    final double maxZ = calcMaxZ( profil );
    final IRecord[] points = profil.getPoints();
    for( int i = 0; i < points.length - 1; i++ )
    {
      final double currentWidth = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i] );
      // just take the profile points between (inclusive) the two given widths.
      if( currentWidth >= startWidth & currentWidth <= endWidth )
      {
        final double z1 = maxZ - getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
        final double z2 = maxZ - getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i + 1] );
        final double width1 = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i] );
        final double width2 = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i + 1] );
        width = width2 - width1;
        area = area + (z1 + z2) / 2 * width;
      }
    }

    return area;
  }

  /**
   * calculates the area of a given profile.<br>
   * the area is calculated in dependence of the max heigth value. input: IProfil<br>
   * output: area <br>
   */
  public static final double calcArea( final IProfil profil )
  {
    double area = 0;
    double width = 0;
    final double maxZ = calcMaxZ( profil );
    final IRecord[] points = profil.getPoints();
    for( int i = 0; i < points.length - 1; i++ )
    {
      final double z1 = maxZ - getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
      final double z2 = maxZ - getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i + 1] );
      final double width1 = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i] );
      final double width2 = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i + 1] );
      width = width2 - width1;
      area = area + (z1 + z2) / 2 * width;
    }

    return area;
  }

  /**
   * gives the maximal z value of a coordinate array
   */
  private static double calcMaxZ( final IProfil profile )
  {
    final IRecord[] points = profile.getPoints();
    if( points.length < 1 )
      return Double.MIN_VALUE;
    Double maxZ = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[0] );
    for( int i = 1; i < points.length; i++ )
    {
      maxZ = Math.max( maxZ, getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] ) );
    }
    return maxZ;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#croppProfile(org.kalypso.model.wspm.core.profil.IRecord,
   *      org.kalypso.model.wspm.core.profil.IRecord)
   */
  public static void croppProfile( final IProfil profile, final double start, final double end )
  {

    final TupleResult points = profile.getResult();
    final IRecord[] segment1 = getSegment( profile, start );
    final IRecord[] segment2 = getSegment( profile, end );
    IRecord startPoint = null;
    IRecord endPoint = null;
    int index1 = 0;
    int index2 = 0;

    startPoint = splitSegment( profile, segment1[0], segment1[1] );
    if( startPoint == null )
    {
      System.out.println( Messages.ProfilUtil_3 );
      startPoint = profile.getPoints()[0];
      index1 = 0;
    }
    else
      index1 = points.indexOf( segment1[1] );

    points.add( index1, startPoint );

    endPoint = splitSegment( profile, segment2[0], segment2[1] );
    if( endPoint == null )
    {
      System.out.println( Messages.ProfilUtil_4 );
      endPoint = points.get( points.size() - 1 );
      index2 = points.size() - 1;
    }
    else
      index2 = points.indexOf( segment2[1] );

    points.add( index2, endPoint );

    final IRecord[] toDelete_1 = points.subList( 0, index1 ).toArray( new IRecord[0] );
    final IRecord[] toDelete_2 = points.subList( index2 + 1, points.size() ).toArray( new IRecord[0] );
    for( final IRecord element : toDelete_1 )
      // for( final IProfilPointMarker marker : provider.getPointMarkerFor( element ) )
// {
// profile.removePointMarker( marker );
// }
      profile.removePoint( element );
    for( final IRecord element : toDelete_2 )
      // for( final IProfilPointMarker marker : profile.getPointMarkerFor( element ) )
// {
// profile.removePointMarker( marker );
// }
      profile.removePoint( element );
  }

  /**
   * returns the georeferenced points of a profile.
   * 
   * @param profile
   *          input profile
   */
  public static IRecord[] getGeoreferencedPoints( final IProfil profile )
  {
    /* List for storing points of the profile, which have a geo reference. */

    final IRecord[] points = profile.getPoints();
    final ArrayList<IRecord> geoReferencedPoints = new ArrayList<IRecord>( points.length );
    for( final IRecord point : points )
    {
      final Double rechtsWert = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, point );
      final Double hochWert = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, point );
      if( !rechtsWert.isNaN() && !hochWert.isNaN() )
        geoReferencedPoints.add( point );
    }
    return geoReferencedPoints.toArray( new IRecord[] {} );
  }

  public static GM_Curve getLine( final IProfil profile, final String crs ) throws GM_Exception
  {
    final IRecord[] georeferencedPoints = getGeoreferencedPoints( profile );
    final GM_Position[] pos = new GM_Position[georeferencedPoints.length];

    for( int i = 0; i < georeferencedPoints.length; i++ )
    {
      final Double x = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, georeferencedPoints[i] );
      final Double y = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, georeferencedPoints[i] );
      final Double z = getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, georeferencedPoints[i] );
      pos[i] = GeometryFactory.createGM_Position( x, y, z );
    }
    final GM_Curve curve = GeometryFactory.createGM_Curve( pos, crs );
    return curve;

  }

  /**
   * creates a profile from {@link LineString} with '0.0' as z-values.
   */
  public static IProfil convertLinestringToEmptyProfile( final LineString jtsCurve, final String type )
  {
    // create a profile only with the digitized points and with 0.0 as z-value
    /* Create the new profile. */
    final IProfil digitizedProfile = ProfilFactory.createProfil( type );

    /* The needed components. */
    final int iRechtswert = digitizedProfile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final int iHochwert = digitizedProfile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final int iBreite = digitizedProfile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iHoehe = digitizedProfile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    if( jtsCurve == null )
      return null;

    double breite = 0.0;

    for( int i = 0; i < jtsCurve.getNumPoints(); i++ )
    {
      final Coordinate coordinate = jtsCurve.getCoordinateN( i );

      final double rechtswert = coordinate.x;
      final double hochwert = coordinate.y;

      /* calculate breite */
      double distance = 0;
      if( i > 0 )
        distance = coordinate.distance( jtsCurve.getCoordinateN( i - 1 ) );

      breite = breite + distance;

      /* elevation is set to 0.0 */
      final double hoehe = 0.0;

      /* Create a new profile point. */
      final IRecord profilePoint = digitizedProfile.createProfilPoint();

      /* Add geo values. */
      profilePoint.setValue( iRechtswert, rechtswert );
      profilePoint.setValue( iHochwert, hochwert );

      /* Add length section values. */
      profilePoint.setValue( iBreite, breite );
      profilePoint.setValue( iHoehe, hoehe );

      /* Add the new point to the profile. */
      digitizedProfile.addPoint( profilePoint );
    }

    return digitizedProfile;
  }

  public static Double[] getDoubleValuesFor( final IProfil profil, final IComponent pointProperty )
  {
    final List<Double> myValues = new ArrayList<Double>();

    final Object[] values = getValuesFor( profil, pointProperty );
    for( final Object object : values )
    {
      myValues.add( Double.valueOf( object.toString() ) );
    }

    return myValues.toArray( new Double[] {} );
  }
}
