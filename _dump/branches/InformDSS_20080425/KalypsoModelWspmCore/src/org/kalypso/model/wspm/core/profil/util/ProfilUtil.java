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
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.Messages;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  /**
   * @return the DoubleValues of each point for this pointProperty in the correct order
   */
  public static Double[] getValuesFor( final IProfil profil, final IComponent pointProperty )
  {
    final IRecord[] points = profil.getPoints();
    final Double[] values = new Double[points.length];
    final int iProp = profil.indexOfProperty( pointProperty );
    if( iProp < 0 )
      return values;
    int i = 0;

    for( final IRecord point : points )
    {
      values[i] = (Double) point.getValue( iProp );
      i++;
    }
    return values;
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
      final Double precision = ProfilObsHelper.getPrecision( property );
      if( !property.getValueTypeName().equals( IWspmConstants.Q_DOUBLE ) )
        continue;
      final Object x1 = point1.getValue( property );
      final Object x2 = point2.getValue( property );
      if( x1 == null || x2 == null )
        continue;
      if( Math.abs( (Double) x1 - (Double) x2 ) > precision )
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
    final TupleResult result = profile.getResult();
    final HashMap<String, IComponent> properties = getComponentsFromProfile( profile );
    final IComponent cmpBreite = profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    if( cmpBreite == null )
      throw new IllegalStateException( Messages.ProfilUtil_0 );
    final IRecord[] points = profile.getPoints();
    for( final IRecord point : points )
    {

      final Double breite = (Double) point.getValue( cmpBreite );
      point.setValue( cmpBreite, -breite );

    }

    ArrayUtils.reverse( points );

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
        final IComponent cmp = properties.get( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        final Double value = (Double) point.getValue( cmp );
        previousPoint.setValue( cmp, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) )
      {
        final IComponent cmp = properties.get( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
        final Double value = (Double) point.getValue( cmp );
        previousPoint.setValue( cmp, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      {
        final IComponent cmp = properties.get( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
        final Double value = (Double) point.getValue( cmp );
        previousPoint.setValue( cmp, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) )
      {
        final IComponent cmp = properties.get( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
        final Double value = (Double) point.getValue( cmp );
        previousPoint.setValue( cmp, value );
      }

      if( properties.containsKey( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) )
      {
        final IComponent cmp = properties.get( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );
        final Double value = (Double) point.getValue( cmp );
        previousPoint.setValue( cmp, value );
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

  public static final IRecord splitSegment( final IProfil profile, final IRecord startPoint, final IRecord endPoint )
  {
    if( startPoint == null || endPoint == null )
      return null;
    final IRecord point = profile.createProfilPoint();
    final IComponent[] properties = profile.getPointProperties();

    for( final IComponent property : properties )
      if( IWspmConstants.POINT_PROPERTY_BREITE.equals( property.getId() ) )
      {
        final Double b1 = (Double) startPoint.getValue( property );
        final Double l = (Double) endPoint.getValue( property ) - b1;
        point.setValue( property, b1 + l / 2.0 );
      }
      else if( IWspmConstants.POINT_PROPERTY_HOEHE.equals( property.getId() ) )
      {
        final Double h1 = (Double) startPoint.getValue( property );
        final Double z = (Double) endPoint.getValue( property ) - h1;
        point.setValue( property, h1 + z / 2.0 );
      }
      else if( !ArrayUtils.contains( profile.getPointMarkerTypes(), property ) )
        point.setValue( property, startPoint.getValue( property ) );
    return point;
  }

  /**
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite -> 2.suche Punkt bei
   * breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public static final IRecord findPoint( final IProfil profil, final int index, final double breite, final double delta )
  {
    final IRecord[] points = profil.getPoints();
    final IRecord point = index >= points.length ? null : points[index];

    if( point != null )
      if( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) == breite )
        return point;

    return findPoint( profil, breite, delta );

  }

  public static IRecord[] getSegment( final IProfil profile, final double breite )
  {
    final IRecord[] points = profile.getPoints();
    final IRecord[] segment = new IRecord[] { null, null };

    for( int i = 0; i < points.length - 1; i++ )
    {
      final double segmentStartWidth = (Double) points[i].getValue( ProfilObsHelper.getPropertyFromId( points[i], IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double segmentEndWidth = (Double) points[i + 1].getValue( ProfilObsHelper.getPropertyFromId( points[i], IWspmConstants.POINT_PROPERTY_BREITE ) );
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

  public static IRecord[] findPoints( final IProfil profile, final IComponent property, final Point2D point, final Double radius )
  {

    final IRecord[] points = profile.getPoints();
    final IComponent cBreite = profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    if( property == null || cBreite == null )
      return new IRecord[] {};
    final ArrayList<IRecord> found = new ArrayList<IRecord>();
    for( final IRecord p : points )
    {

      final Point2D p2D = new Point2D.Double( (Double) p.getValue( cBreite ), (Double) p.getValue( property ) );
      if( p2D.distance( point ) <= radius )
        found.add( p );
    }

    return found.toArray( new IRecord[] {} );
  }

  public static IRecord findPoint( final IProfil profil, final double breite, final double delta )
  {
    final IRecord pkt = findNearestPoint( profil, breite );

    final double xpos = (Double) pkt.getValue( ProfilObsHelper.getPropertyFromId( pkt, IWspmConstants.POINT_PROPERTY_BREITE ) );
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
      if( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) > breite )
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
      if( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) > breite )
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
    if( property == null )
      return null;

    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;
    Double maxValue = Double.MIN_VALUE;
    for( final IRecord point : points )
      maxValue = Math.max( maxValue, (Double) point.getValue( property ) );
    return maxValue;
  }

  public static Point2D getPoint2D( final IRecord p, final IComponent pointProperty )
  {

    final Object x = p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );
    final Object y = p.getValue( pointProperty );

    return new Point2D.Double( x == null ? 0.0 : (Double) x, y == null ? 0.0 : (Double) y );

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
      if( Math.abs( (Double) bestPoint.getValue( ProfilObsHelper.getPropertyFromId( bestPoint, IWspmConstants.POINT_PROPERTY_BREITE ) ) - breite ) > Math.abs( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) )
          - breite )
          || Math.abs( (Double) bestPoint.getValue( property ) - value ) > Math.abs( (Double) point.getValue( property ) - value ) )
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
        final Double delta = ProfilObsHelper.getPrecision( properties[i] );

        if( Math.abs( (Double) point.getValue( properties[i] ) - values[i] ) > delta )
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

  public static Point2D[] getPoints2D( final IProfil profil, final IComponent pointProperty )
  {
    if( pointProperty == null )
      return new Point2D[] {};

    final IRecord[] points = profil.getPoints();
    final Point2D[] points2D = new Point2D[points.length];
    int i = 0;
    for( final IRecord p : points )
    {
      final IComponent cBreite = ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE );
      final Object x = p.getValue( cBreite );
      final Object y = p.getValue( pointProperty );
      if( y == null || x == null )
        return new Point2D[] {};
      points2D[i++] = new Point2D.Double( (Double) x, (Double) y );

    }
    return points2D;
  }

  public static Double getMinValueFor( final IProfil profil, final IComponent property )
  {
    final IRecord minPoint = getMinPoint( profil, property );
    if( minPoint == null )
      return null;

    return (Double) minPoint.getValue( property );
  }

  /**
   * Return the profile-point of the given profile with the minimum value at the given property.
   */
  public static IRecord getMinPoint( final IProfil profil, final IComponent property )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return null;

    Double minValue = Double.MAX_VALUE;
    IRecord minPoint = null;
    for( final IRecord point : points )
    {
      final double value = (Double) point.getValue( property );
      if( value < minValue )
      {
        minValue = value;
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
    final IRecord point = profil.createProfilPoint();

    point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ), hoehe );
    point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ), breite );
    profil.addPoint( point );
    return point;
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

    for( int i = 0; i < profil.getPoints().length - 1; i++ )
    {
      final IRecord p = profil.getPoints()[i];
      final IRecord pPlus = profil.getPoints()[i + 1];

      final double currentWidth = (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );

      // just take the profile points between (inclusive) the two given widths.
      if( currentWidth >= startWidth & currentWidth <= endWidth )
      {
        final double z1 = maxZ - (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_HOEHE ) );
        final double z2 = maxZ - (Double) pPlus.getValue( ProfilObsHelper.getPropertyFromId( pPlus, IWspmConstants.POINT_PROPERTY_HOEHE ) );
        final double width1 = (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );
        final double width2 = (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );
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

    for( int i = 0; i < profil.getPoints().length - 1; i++ )
    {
      final IRecord p = profil.getPoints()[i];
      final IRecord pPlus = profil.getPoints()[i + 1];

      final double z1 = maxZ - (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      final double z2 = maxZ - (Double) pPlus.getValue( ProfilObsHelper.getPropertyFromId( pPlus, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      final double width1 = (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double width2 = (Double) pPlus.getValue( ProfilObsHelper.getPropertyFromId( pPlus, IWspmConstants.POINT_PROPERTY_BREITE ) );
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
    double maxZ = -9999;

    for( int i = 0; i < profile.getPoints().length; i++ )
    {
      final double currentZ = (Double) profile.getPoints()[i].getValue( ProfilObsHelper.getPropertyFromId( profile.getPoints()[i], IWspmConstants.POINT_PROPERTY_HOEHE ) );
      if( maxZ < currentZ )
        maxZ = currentZ;
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
   *            input profile
   */
  public static LinkedList<IRecord> getGeoreferencedPoints( final IProfil profile )
  {
    /* List for storing points of the profile, which have a geo reference. */
    final LinkedList<IRecord> geoReferencedPoints = new LinkedList<IRecord>();

    final IRecord[] points = profile.getPoints();
    for( final IRecord point : points )
    {
      final double rechtsWert = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final double hochWert = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

      if( rechtsWert > 0.0 || hochWert > 0.0 )
        geoReferencedPoints.add( point );
    }
    return geoReferencedPoints;
  }

  public static GM_Curve getLine( final IProfil profile, final String crs ) throws GM_Exception
  {
    final LinkedList<IRecord> georeferencedPoints = getGeoreferencedPoints( profile );
    final GM_Position[] pos = new GM_Position[georeferencedPoints.size()];

    for( int i = 0; i < georeferencedPoints.size(); i++ )
    {
      final double x = (Double) georeferencedPoints.get( i ).getValue( ProfilObsHelper.getPropertyFromId( georeferencedPoints.get( i ), IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final double y = (Double) georeferencedPoints.get( i ).getValue( ProfilObsHelper.getPropertyFromId( georeferencedPoints.get( i ), IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
      final double z = (Double) georeferencedPoints.get( i ).getValue( ProfilObsHelper.getPropertyFromId( georeferencedPoints.get( i ), IWspmConstants.POINT_PROPERTY_HOEHE ) );
      pos[i] = GeometryFactory.createGM_Position( x, y, z );
    }

    final GM_Curve curve = GeometryFactory.createGM_Curve( pos, crs );

    return curve;

  }
}
