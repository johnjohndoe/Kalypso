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
package org.kalypso.model.wspm.core.util;

import java.util.LinkedList;
import java.util.TreeSet;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * @author Holger Albert, Thomas Jung TODO: merge / check this class with {@link ProfilUtil}
 */
public class WspmProfileHelper
{
  /**
   * The constructor is private, should not be instanciated.
   */
  private WspmProfileHelper( )
  {
  }

  /**
   * Returns the witdh position of a geo point projected on a profile.
   * <p>
   * It works with the following steps:<br />
   * <ol>
   * <li>The profile points with a geo reference are stored. All points without a geo reference are ignored.</li>
   * <li>With these points, single line segments are build (using Rechtswert and Hochwert).</li>
   * <li>The geo point is transformed into the coordinate system of the profiles.</li>
   * <li>It is checked for each segment, which distance the geo point has to them.</li>
   * <li>The points of the segment with the lowest distance will be used for projection.</li>
   * </ol>
   * </p>
   * 
   * @param geoPoint
   *            The geo point. It does not have to lie on the profile.
   * @param profile
   *            The profile.
   * @param srsName
   *            The coordinate system, in which the profile lies (or null, but this can behave strange, since it assumes
   *            one).
   * @return The width (X-Direction) of the geo point projected on the profile.
   */
  public static Double getWidthPosition( GM_Point geoPoint, IProfil profile, String srsName ) throws Exception
  {
    /* List for storing points of the profile, which have a geo reference. */
    LinkedList<IProfilPoint> geoReferencedPoints = new LinkedList<IProfilPoint>();

    LinkedList<IProfilPoint> points = profile.getPoints();
    for( IProfilPoint point : points )
    {
      double rechtsWert = point.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double hochWert = point.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      if( rechtsWert > 0.0 || hochWert > 0.0 )
      {
        /* Memorize the point, because it has a geo reference. */
        geoReferencedPoints.add( point );
      }
      // else
      // System.out.print( "The point " + point.toString() + " has no RECHTSWERT or HOCHWERT or is missing both.\n" );
    }

    /* If no or only one geo referenced points are found, return. */
    if( geoReferencedPoints.size() <= 1 )
      return null;

    // END OF FINDING GEOREFERENCED POINTS

    /* It is assumed that all points and values share the same coordinate system. */
    if( srsName == null )
      srsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( geoReferencedPoints.get( 0 ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) ) );

    CS_CoordinateSystem crs = srsName == null ? null : org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory.getInstance().getOGCCSByName( srsName );
    final CS_CoordinateSystem kalypsoCrs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    /* Transform geo point into the coord-system of the line. */
    GeoTransformer transformer = new GeoTransformer( kalypsoCrs );
    final GM_Point transformedGeoPoint = (GM_Point) transformer.transform( geoPoint );
    Geometry comparePoint = JTSAdapter.export( transformedGeoPoint );

    double distance = Double.MAX_VALUE;
    IProfilPoint pointOne = null;
    IProfilPoint pointTwo = null;
    LineSegment segment = null;

    /* No we have a list with fully geo referenced points of a profile. */
    for( int i = 0; i < geoReferencedPoints.size() - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      IProfilPoint tempPointOne = geoReferencedPoints.get( i );
      double rechtsWertOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double hochWertOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      IProfilPoint tempPointTwo = geoReferencedPoints.get( i + 1 );
      double rechtsWertTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double hochWertTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      /* Geo-Projection */
      final GM_Point geoPointOne = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertOne, hochWertOne, crs );
      final GM_Point geoPointTwo = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertTwo, hochWertTwo, crs );

      /* Build the line segment. */
      Coordinate geoCoordOne = JTSAdapter.export( transformer.transform( geoPointOne ).getCentroid().getPosition() );
      Coordinate geoCoordTwo = JTSAdapter.export( transformer.transform( geoPointTwo ).getCentroid().getPosition() );
      LineSegment geoSegment = new LineSegment( geoCoordOne, geoCoordTwo );

      /* Calculate the distance of the geo point to the line. */
      double tempDistance = geoSegment.distance( comparePoint.getCoordinate() );

      /* If it is shorter than the last distance, remember it and the distance. */
      if( tempDistance <= distance )
      {
        distance = tempDistance;
        pointOne = tempPointOne;
        pointTwo = tempPointTwo;
        segment = geoSegment;
      }
    }

    /* Now we have a segment and a distance. The two points of the segment are used to interpolate. */

    /* The point on the geo segment, which is closest to the comparePoint (originally geoPoint). */
    Coordinate geoCoordinate = segment.closestPoint( comparePoint.getCoordinate() );

    double geoSegmentLength = segment.getLength();
    double toGeoPointLength = JTSUtilities.getLengthBetweenPoints( segment.getCoordinate( 0 ), geoCoordinate );

    /* Using Breite to build. */
    final double breiteOne = pointOne.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double breiteTwo = pointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    /* Important: The interpolation is done here :). */
    double toProfilePointLength = (toGeoPointLength / geoSegmentLength) * (breiteTwo - breiteOne);

    return breiteOne + toProfilePointLength;
  }

  /**
   * returns the geographic coordinates (x, y, z) for a given width coordinate as GM_Point.
   * 
   * @param width
   *            width coordinate
   * @param profile
   *            profile
   * @return geo position as GM_Point
   */
  public static GM_Point getGeoPosition( double width, IProfil profile ) throws Exception
  {
    final LinkedList<IProfilPoint> geoReferencedPoints = ProfilUtil.getGeoreferencedPoints( profile );

    final String srsName = (String) profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    CS_CoordinateSystem crs = srsName == null ? null : org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory.getInstance().getOGCCSByName( srsName );
    /* If no or only one geo referenced points are found, return. */
    if( geoReferencedPoints.size() <= 1 )
      return null;

    // END OF FINDING GEOREFERENCED POINTS

    /* No we have a list with fully geo referenced points of a profile. */
    for( int i = 0; i < geoReferencedPoints.size() - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      IProfilPoint tempPointOne = geoReferencedPoints.get( i );
      double widthValueOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigthValueOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      double rechtsWertOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double hochWertOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      IProfilPoint tempPointTwo = geoReferencedPoints.get( i + 1 );
      double widthValueTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigthValueTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      double rechtsWertTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double hochWertTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      /* find the right segment with the neighboring points */
      if( widthValueOne < width & widthValueTwo > width )
      {
        /* calculate the georeference */
        final double deltaOne = width - widthValueOne;
        final double delta = widthValueTwo - widthValueOne;
        final double x = (deltaOne * (rechtsWertTwo - rechtsWertOne) / delta) + rechtsWertOne;
        final double y = (deltaOne * (hochWertTwo - hochWertOne) / delta) + hochWertOne;
        final double z = (deltaOne * (heigthValueTwo - heigthValueOne) / delta) + heigthValueOne;

        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( x, y, z, crs );
      }
      /* if the point is lying on the start point of the segment */
      else if( widthValueOne == width )
      {
        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertOne, hochWertOne, heigthValueOne, crs );
      }
      /* if the point is lying on the end point of the segment */
      else if( widthValueTwo == width )
      {
        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertTwo, hochWertTwo, heigthValueTwo, crs );
      }
    }
    return null;
  }

  /**
   * returns the corresponding height for an given width coordinate. if the width is outside of the profile points, the
   * first / last point height is returned.
   * 
   * @param width
   *            width coordinate
   * @param profile
   *            profile
   * @return The height
   */
  public static Double getHeigthPositionByWidth( double width, IProfil profile ) throws Exception
  {
    LinkedList<IProfilPoint> points = profile.getPoints();
    if( points.size() < 1 )
      return null;

    for( int i = 0; i < points.size() - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      IProfilPoint tempPointOne = points.get( i );
      double widthValueOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigthValueOne = tempPointOne.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );

      IProfilPoint tempPointTwo = points.get( i + 1 );
      double widthValueTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigthValueTwo = tempPointTwo.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );

      /* searching for the right segment */
      if( widthValueOne <= width & widthValueTwo >= width )
      {
        /* calculate the heigth */
        return ((width - widthValueOne) * (heigthValueTwo - heigthValueOne) / (widthValueTwo - widthValueOne)) + heigthValueOne;
      }

    }
    if( width < points.get( 0 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) )
      return points.get( 0 ).getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    else if( width > points.get( points.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) )
      return points.get( points.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    return null;
  }

  /**
   * gets the geo-points of the intersect between profile and water level
   * 
   * @param profil
   *            input profile
   * @param wspHoehe
   *            water level
   */
  public static GM_Point[] calculateWspPoints( final IProfil profil, final double wspHoehe )
  {
    // final IProfilPointProperty[] pointProperties = profil.getPointProperties( );
    // final POINT_PROPERTY ppRW = pointProperties.contains( IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT ) ?
    // IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT : null;
    // final POINT_PROPERTY ppHW = pointProperties.contains( IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ) ?
    // IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT : null;
    if( !profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) || !profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) ) // ignore
      // profile
      // without
      // geo-coordinates
      return new GM_Point[] {};

    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilPoint firstPoint = points.getFirst();
    final IProfilPoint lastPoint = points.getLast();

    final double firstX = firstPoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double firstY = firstPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    final double lastX = lastPoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double lastY = lastPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );

    final Double[] breiteValues = ProfilUtil.getValuesFor( profil, IWspmConstants.POINT_PROPERTY_BREITE );

    final PolyLine wspLine = new PolyLine( new double[] { firstX, lastX }, new double[] { wspHoehe, wspHoehe }, 0.0001 );
    final PolyLine profilLine = new PolyLine( breiteValues, ProfilUtil.getValuesFor( profil, IWspmConstants.POINT_PROPERTY_HOEHE ), 0.0001 );

    /* Same for RW and HW, but filter 0-values */
    final PolyLine rwLine = createPolyline( profil, IWspmConstants.POINT_PROPERTY_BREITE, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final PolyLine hwLine = createPolyline( profil, IWspmConstants.POINT_PROPERTY_BREITE, IWspmConstants.POINT_PROPERTY_HOCHWERT );

    final double[] intersectionXs = profilLine.intersect( wspLine );

    final TreeSet<Double> intersections = new TreeSet<Double>();
    if( firstY < wspHoehe )
      intersections.add( new Double( firstX ) );
    for( final double d : intersectionXs )
      intersections.add( new Double( d ) );
    if( lastY < wspHoehe )
      intersections.add( new Double( lastX ) );

    final GM_Point[] poses = new GM_Point[intersections.size()];
    int count = 0;
    for( final Double x : intersections )
    {
      final double rw = rwLine.getYFor( x, false );
      final double hw = hwLine.getYFor( x, false );

      final String crsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( rw ) );
      final CS_CoordinateSystem crs = crsName == null ? null : org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory.getInstance().getOGCCSByName( crsName );
      final GM_Point point = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rw, hw, wspHoehe, crs );

      poses[count++] = point;
    }

    return poses;
  }

  private static PolyLine createPolyline( final IProfil profil, final String xProperty, final String yProperty )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();

    final double[] xValues = new double[points.size()];
    final double[] yValues = new double[points.size()];
    final IProfilPointProperty pp = profil.getPointProperty( yProperty );
    final double dy = pp == null ? 0.001 : pp.getPrecision();
    int count = 0;
    for( final IProfilPoint point : points )
    {
      final double x = point.getValueFor( xProperty );
      final double y = point.getValueFor( yProperty );

      if( Math.abs( y ) > dy )
      {
        xValues[count] = x;
        yValues[count] = y;
        count++;
      }
    }

    final double[] xFiltered = new double[count];
    final double[] yFiltered = new double[count];

    System.arraycopy( xValues, 0, xFiltered, 0, count );
    System.arraycopy( yValues, 0, yFiltered, 0, count );

    return new PolyLine( xFiltered, yFiltered, 0.0001 );
  }

  /**
   * cuts an IProfil at defined geo-points, that have to lie on the profile-line.
   * 
   * @param profile
   *            the profile
   * @param firstPoint
   *            first geo point
   * @param lastPoint
   *            last geo point
   */
  public static IProfil cutIProfile( final IProfil profile, final GM_Point firstPoint, final GM_Point lastPoint ) throws Exception
  {
    final double width1 = WspmProfileHelper.getWidthPosition( firstPoint, profile, profile.getName() );
    final double width2 = WspmProfileHelper.getWidthPosition( lastPoint, profile, profile.getName() );

    final IProfil orgIProfil = profile;

    final double startWidth;
    final double endWidth;
    final GM_Point geoPoint1;
    final GM_Point geoPoint2;

    if( width1 > width2 )
    {
      startWidth = width2;
      endWidth = width1;
      geoPoint1 = lastPoint;
      geoPoint2 = firstPoint;
    }
    else
    {
      startWidth = width1;
      endWidth = width2;
      geoPoint1 = firstPoint;
      geoPoint2 = lastPoint;
    }

    // calculate elevations
    final double heigth1 = WspmProfileHelper.getHeigthPositionByWidth( startWidth, orgIProfil );
    final double heigth2 = WspmProfileHelper.getHeigthPositionByWidth( endWidth, orgIProfil );

    final LinkedList<IProfilPoint> profilPointList = profile.getPoints();
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    /* set the coordinate system */
    CS_CoordinateSystem crs = (CS_CoordinateSystem) profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    tmpProfil.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, crs );

    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    final IProfilPoint point1 = tmpProfil.createProfilPoint();
    final IProfilPoint point2 = tmpProfil.createProfilPoint();

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    point1.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, startWidth );
    point1.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth1 );
    point1.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, geoPoint1.getX() );
    point1.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, geoPoint1.getY() );

    point2.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, endWidth );
    point2.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth2 );
    point2.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, geoPoint2.getX() );
    point2.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, geoPoint2.getY() );

    tmpProfil.addPoint( point1 );

    for( final IProfilPoint point : profilPointList )
    {
      final double currentWidth = point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      if( currentWidth > startWidth & currentWidth < endWidth )
      {
        final IProfilPoint pt = tmpProfil.createProfilPoint();

        final IProfilPointProperty[] properties = orgIProfil.getPointProperties();
        for( final IProfilPointProperty property : properties )
        {
          final String propertyId = property.toString();
          final double value = point.getValueFor( propertyId );
          pt.setValueFor( propertyId, value );
        }
        tmpProfil.addPoint( pt );
      }
    }

    tmpProfil.addPoint( point2 );

    tmpProfil.setStation( orgIProfil.getStation() );
    return tmpProfil;
  }
}