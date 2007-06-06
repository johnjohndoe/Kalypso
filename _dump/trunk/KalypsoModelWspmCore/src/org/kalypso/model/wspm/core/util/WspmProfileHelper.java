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
package org.kalypso.model.wspm.core.util;

import java.util.LinkedList;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Holger Albert, Thomas Jung
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
    final GM_Point transformedGeoPoint = (GM_Point) transformer.transform( geoPoint  );
    Geometry comparePoint = JTSAdapter.export( transformedGeoPoint  );

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

      /* Geo-Projectino */
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
   * returns the geographic coordinates (x, y, z) for an given width coordinate as GM_Point. Input: width coordinate
   * (double), profile (Iprofil) Output: point (GM_Point)
   */
  public static GM_Point getGeoPosition( double width, IProfil profile ) throws Exception
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

      /* find the right segment with the neighbouring points */
      if( widthValueOne < width & widthValueTwo > width )
      {
        /* calculate the georeference */
        final double deltaOne = width - widthValueOne;
        final double delta = widthValueTwo - widthValueOne;
        final double x = (deltaOne * (rechtsWertTwo - rechtsWertOne) / delta) + rechtsWertOne;
        final double y = (deltaOne * (hochWertTwo - hochWertOne) / delta) + hochWertOne;
        final double z = (deltaOne * (heigthValueTwo - heigthValueOne) / delta) + heigthValueOne;

        Coordinate geoCoord = new Coordinate( x, y, z );
        GeometryFactory factory = new GeometryFactory();

        Point point = factory.createPoint( geoCoord );
        GM_Object object = JTSAdapter.wrap( point );

        return (GM_Point) object;
      }
      /* if the point is lying on the start point of the segemnt */
      else if( widthValueOne == width )
      {
        /* calculate the georeference */
        final double x = rechtsWertOne;
        final double y = hochWertOne;
        final double z = heigthValueOne;

        Coordinate geoCoord = new Coordinate( x, y, z );
        GeometryFactory factory = new GeometryFactory();

        Point point = factory.createPoint( geoCoord );
        GM_Object object = JTSAdapter.wrap( point );

        return (GM_Point) object;
      }
      /* if the point is lying on the end point of the segemnt */
      else if( widthValueTwo == width )
      {
        /* calculate the georeference */
        final double x = rechtsWertTwo;
        final double y = hochWertTwo;
        final double z = heigthValueTwo;

        Coordinate geoCoord = new Coordinate( x, y, z );
        GeometryFactory factory = new GeometryFactory();

        Point point = factory.createPoint( geoCoord );
        GM_Object object = JTSAdapter.wrap( point );

        return (GM_Point) object;
      }
    }
    return null;
  }

  /**
   * returns the corresponding heigth for an given width coordinate. Input: width coordinate (double), profile (Iprofil)
   * Output: heigth (Double)
   * <p>
   * if the width is outside of the profile points, the first / last point heigth is returned.
   * 
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
}