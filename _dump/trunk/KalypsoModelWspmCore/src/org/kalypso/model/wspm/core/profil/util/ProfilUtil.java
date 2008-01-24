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
package org.kalypso.model.wspm.core.profil.util;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

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
    final LinkedList<IRecord> points = profil.getPoints();
    final Double[] values = new Double[points.size()];
    int i = 0;

    for( final IRecord point : points )
    {
      values[i] = (Double) point.getValue( pointProperty );
      i++;
    }
    return values;
  }

  /**
   * @return a subList include both MarkerPoints, maybe null
   */
  public static final List<IRecord> getInnerPoints( final IProfil profil, final IProfilPointMarker leftMarker, final IProfilPointMarker rightMarker )
  {

    final LinkedList<IRecord> points = profil.getPoints();
    final int leftPos = (leftMarker != null) ? points.indexOf( leftMarker.getPoint() ) : 0;
    final int rightPos = (rightMarker != null) ? points.indexOf( rightMarker.getPoint() ) + 1 : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;

  }

  public static final List<IRecord> getInnerPoints( final IProfil profil, final IComponent markerTyp )
  {
    final IProfilPointMarker[] markers = profil.getPointMarkerFor( markerTyp );
    final LinkedList<IRecord> points = profil.getPoints();
    final int leftPos = (markers.length > 0) ? points.indexOf( markers[0].getPoint() ) : 0;
    final int rightPos = (markers.length > 1) ? points.indexOf( markers[markers.length - 1].getPoint() ) + 1 : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;
  }

  /**
   * return true if all selected properties are equal
   */
  public static final boolean comparePoints( final IComponent[] properties, final IRecord point1, final IRecord point2 )
  {
    for( final IComponent property : properties )
    {
      final Double precision = ProfilObsHelper.getPrecision( property );
      if( Math.abs( (Double) point1.getValue( property ) - (Double) point2.getValue( property ) ) > precision )
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
    final LinkedList<IRecord> points = profile.getPoints();
    for( final IRecord point : points )
    {
      final TupleResult result = point.getOwner();

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) )
      {
        final Double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) );
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ), -breite );
      }
      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) ) )
      {
        final Double hoehe = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ), -hoehe );
      }
    }
    Collections.reverse( points );
    IRecord previousPoint = null;
    for( final IRecord point : points )
    {
      final TupleResult result = point.getOwner();

      if( previousPoint == null )
      {
        previousPoint = point;
        continue;
      }

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) ) )
      {
        final Double value = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) );
        previousPoint.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ), value );
      }

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) ) )
      {
        final Double value = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) );
        previousPoint.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ), value );
      }

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) ) )
      {
        final Double value = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) );
        previousPoint.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), value );
      }

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) ) )
      {
        final Double value = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) );
        previousPoint.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), value );
      }

      if( result.hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) ) )
      {
        final Double value = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) );
        previousPoint.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), value );
      }
      previousPoint = point;
    }
  }

  public static final IRecord splitSegment( final IProfil profile, final IRecord startPoint, final IRecord endPoint )
  {
    if( (startPoint == null) || (endPoint == null) )
      return null;
    final IRecord point = profile.createProfilPoint();
    final IComponent[] properties = profile.getPointProperties();
    final double b1 = (Double) startPoint.getValue( ProfilObsHelper.getPropertyFromId( startPoint, IWspmConstants.POINT_PROPERTY_BREITE ) );
    final double l = (Double) endPoint.getValue( ProfilObsHelper.getPropertyFromId( endPoint, IWspmConstants.POINT_PROPERTY_BREITE ) ) - b1;
    final double breite = b1 + l / 2.0;
    for( final IComponent property : properties )
    {
      final Double interpolation = ProfilObsHelper.doInterpolation( property, startPoint, endPoint, breite );
      point.setValue( property, interpolation );
    }

    return point;
  }

  /**
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite -> 2.suche Punkt bei
   * breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public static final IRecord findPoint( final IProfil profil, final int index, final double breite, final double delta )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    final IRecord point = index >= points.size() ? null : points.get( index );

    if( point != null )
    {
      if( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) == breite )
        return point;
    }

    return findPoint( profil, breite, delta );

  }

  public static IRecord[] getSegment( final IProfil profile, final double breite )
  {
    final LinkedList<IRecord> points = profile.getPoints();
    final IRecord[] segment = new IRecord[] { null, null };

    for( int i = 0; i < points.size() - 1; i++ )
    {
      final double segmentStartWidth = (Double) points.get( i ).getValue( ProfilObsHelper.getPropertyFromId( points.get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double segmentEndWidth = (Double) points.get( i + 1 ).getValue( ProfilObsHelper.getPropertyFromId( points.get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );
      if( segmentStartWidth <= breite & segmentEndWidth >= breite )
      {
        segment[0] = points.get( i );
        segment[1] = points.get( i + 1 );
      }
    }
    return segment;
  }

  public static IRecord findNearestPoint( final IProfil profil, final double breite )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    IRecord pkt = points.getFirst();
    for( final IRecord point : points )
    {
      if( Math.abs( (Double) pkt.getValue( ProfilObsHelper.getPropertyFromId( pkt, IWspmConstants.POINT_PROPERTY_BREITE ) ) - breite ) > Math.abs( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) )
          - breite ) )
        pkt = point;
    }
    return pkt;
  }

  public static IRecord findPoint( final IProfil profil, final double breite, final double delta )
  {
    final IRecord pkt = findNearestPoint( profil, breite );

    final double xpos = (Double) pkt.getValue( ProfilObsHelper.getPropertyFromId( pkt, IWspmConstants.POINT_PROPERTY_BREITE ) );
    return (Math.abs( xpos - breite ) <= delta) ? pkt : null;
  }

  public static IRecord getPointBefore( final IProfil profil, final IRecord point )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() || point == points.getFirst() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new IllegalArgumentException( "Punkt nicht im Profil: " + point );

    return points.get( i - 1 );
  }

  public static IRecord getPointBefore( final IProfil profil, final double breite )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() )
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
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() )
      return null;
    for( final IRecord point : points )
    {
      if( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) > breite )
        return point;
    }
    return null;
  }

  public static IRecord getPointAfter( final IProfil profil, final IRecord point )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() || point == points.getLast() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new IllegalArgumentException( "Punkt nicht im Profil: " + point );

    return points.get( i + 1 );
  }

  public static Double getMaxValueFor( final IProfil profil, final IComponent property )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() )
      return null;
    Double maxValue = Double.MIN_VALUE;
    for( final IRecord point : points )
    {

      maxValue = Math.max( maxValue, (Double) point.getValue( property ) );

    }
    return maxValue;
  }

  public static Point2D getPoint2D( final IRecord p, final IComponent pointProperty )
  {

    final double x = (Double) p.getValue( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE ) );
    final double y = (Double) p.getValue( pointProperty );
    return new Point2D.Double( x, y );

  }

  /**
   * @return null if profil has no points or property does not exists
   * @return always one point (first point if no match)
   */
  public static IRecord findNearestPoint( final IProfil profil, final double breite, final double value, final IComponent property )
  {
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() )
      return null;
    IRecord bestPoint = points.getFirst();
    for( final IRecord point : points )
    {

      if( (Math.abs( (Double) bestPoint.getValue( ProfilObsHelper.getPropertyFromId( bestPoint, IWspmConstants.POINT_PROPERTY_BREITE ) ) - breite ) > Math.abs( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) )
          - breite ))
          || (Math.abs( (Double) bestPoint.getValue( property ) - value ) > Math.abs( (Double) point.getValue( property ) - value )) )
        bestPoint = point;

    }
    return bestPoint;
  }

  /**
   * @return null if profil has no points or property does not exists or nomatch
   */
  public static IRecord[] findPoint( final IProfil profil, final Double[] values, final IComponent[] properties )
  {
    final List<IRecord> points = new ArrayList<IRecord>();
    final int maxIndex = Math.min( values.length, properties.length );
    final LinkedList<IRecord> profilePoints = profil.getPoints();
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

    final List<IRecord> points = profil.getPoints();
    final Point2D[] points2D = new Point2D[points.size()];
    int i = 0;
    for( final IRecord p : points )
    {

      final IComponent cBreite = ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BREITE );
      final double x = (Double) p.getValue( cBreite );
      final double y = (Double) p.getValue( pointProperty );
      points2D[i++] = new Point2D.Double( x, y );

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
    final LinkedList<IRecord> points = profil.getPoints();
    if( points.isEmpty() )
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
    final LinkedList<IRecord> points = profil.getPoints();
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

    for( int i = 0; i < profil.getPoints().size() - 1; i++ )
    {
      final double currentWidth = (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );

      // just take the profile points between (inclusive) the two given widths.
      if( currentWidth >= startWidth & currentWidth <= endWidth )
      {
        final double z1 = (maxZ - (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_HOEHE ) ));
        final double z2 = (maxZ - (Double) profil.getPoints().get( i + 1 ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i + 1 ), IWspmConstants.POINT_PROPERTY_HOEHE ) ));
        final double width1 = (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );
        final double width2 = (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );
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

    for( int i = 0; i < profil.getPoints().size() - 1; i++ )
    {
      final double z1 = (maxZ - (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_HOEHE ) ));
      final double z2 = (maxZ - (Double) profil.getPoints().get( i + 1 ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i + 1 ), IWspmConstants.POINT_PROPERTY_HOEHE ) ));
      final double width1 = (Double) profil.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double width2 = (Double) profil.getPoints().get( i + 1 ).getValue( ProfilObsHelper.getPropertyFromId( profil.getPoints().get( i + 1 ), IWspmConstants.POINT_PROPERTY_BREITE ) );
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

    for( int i = 0; i < profile.getPoints().size(); i++ )
    {
      final double currentZ = (Double) profile.getPoints().get( i ).getValue( ProfilObsHelper.getPropertyFromId( profile.getPoints().get( i ), IWspmConstants.POINT_PROPERTY_HOEHE ) );
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
    // FIXME
    throw (new NotImplementedException());

// final LinkedList<IRecord> points = profile.getPoints();
// final IRecord[] segment1 = getSegment( profile, start );
// final IRecord[] segment2 = getSegment( profile, end );
// IRecord startPoint = null;
// IRecord endPoint = null;
// int index1 = 0;
// int index2 = 0;
//
// startPoint = splitSegment( profile, segment1[0], segment1[1] );
// if( startPoint == null )
// {
// System.out.println( "Profil konnte nicht abgeschnitten werden. Startpunkt nicht auf Profil. Setze ersten
// Profilpunkt." );
// startPoint = profile.getPoints().getFirst();
// index1 = 0;
// }
// else
// index1 = points.indexOf( segment1[1] );
//
// points.add( index1, startPoint );
//
// endPoint = splitSegment( profile, segment2[0], segment2[1] );
// if( endPoint == null )
// {
// System.out.println( "Profil konnte nicht abgeschnitten werden. Endpunkt nicht auf Profil. Setze letzten Profilpunkt."
// );
// endPoint = profile.getPoints().getLast();
// index2 = points.size() - 1;
// }
// else
// index2 = points.indexOf( segment2[1] );
//
// points.add( index2, endPoint );
//
// final IRecord[] toDelete_1 = points.subList( 0, index1 ).toArray( new IRecord[0] );
// final IRecord[] toDelete_2 = points.subList( index2 + 1, points.size() ).toArray( new IRecord[0] );
// for( final IRecord element : toDelete_1 )
// {
// for( final IProfilPointMarker marker : provider.getPointMarkerFor( element ) )
// {
// profile.removePointMarker( marker );
// }
// profile.removePoint( element );
// }
// for( final IRecord element : toDelete_2 )
// {
// for( final IProfilPointMarker marker : profile.getPointMarkerFor( element ) )
// {
// profile.removePointMarker( marker );
// }
// profile.removePoint( element );
// }
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

    final LinkedList<IRecord> points = profile.getPoints();
    for( final IRecord point : points )
    {
      final double rechtsWert = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final double hochWert = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

      if( rechtsWert > 0.0 || hochWert > 0.0 )
      {
        /* Memorize the point, because it has a geo reference. */

        geoReferencedPoints.add( point );
      }
      // else
      // System.out.print( "The point " + point.toString() + " has no RECHTSWERT or HOCHWERT or is missing both.\n" );
    }
    return geoReferencedPoints;
  }

  public static GM_Curve getLine( final IProfil profile, final CS_CoordinateSystem crs ) throws GM_Exception
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
