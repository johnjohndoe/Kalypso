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
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * @author Holger Albert, Thomas Jung , kimwerner TODO: merge / check this class with {@link ProfilUtil}
 */
public class WspmProfileHelper
{

  public static final double FUZZINESS = 0.005; // Inaccuracies profile of points

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
  public static Double getWidthPosition( final GM_Point geoPoint, final IProfil profile, String srsName ) throws Exception
  {
    /* List for storing points of the profile, which have a geo reference. */
    final LinkedList<IRecord> geoReferencedPoints = new LinkedList<IRecord>();

    final IRecord[] points = profile.getPoints();
    final int iRechtswert = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final int iHochwert = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final int iBreite = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    for( final IRecord point : points )
    {
      final double rechtsWert = (Double) point.getValue( iRechtswert );
      final double hochWert = (Double) point.getValue( iHochwert );

      if( rechtsWert > 0.0 || hochWert > 0.0 )
        /* Memorize the point, because it has a geo reference. */
        geoReferencedPoints.add( point );
    }

    /* If no or only one geo referenced points are found, return. */
    if( geoReferencedPoints.size() <= 1 )
      return null;

    // END OF FINDING GEOREFERENCED POINTS

    /* It is assumed that all points and values share the same coordinate system. */
    if( srsName == null )
      srsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( (Double) geoReferencedPoints.get( 0 ).getValue( iRechtswert ) ) );

    final String kalypsoCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    /* Transform geo point into the coord-system of the line. */
    final GeoTransformer transformer = new GeoTransformer( kalypsoCrs );
    final GM_Point transformedGeoPoint = (GM_Point) transformer.transform( geoPoint );
    final Geometry comparePoint = JTSAdapter.export( transformedGeoPoint );

    double distance = Double.MAX_VALUE;
    IRecord pointOne = null;
    IRecord pointTwo = null;
    LineSegment segment = null;

    /* No we have a list with fully geo referenced points of a profile. */
    for( int i = 0; i < geoReferencedPoints.size() - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      final IRecord tempPointOne = geoReferencedPoints.get( i );
      final double rechtsWertOne = (Double) tempPointOne.getValue( iRechtswert );
      final double hochWertOne = (Double) tempPointOne.getValue( iHochwert );

      final IRecord tempPointTwo = geoReferencedPoints.get( i + 1 );
      final double rechtsWertTwo = (Double) tempPointTwo.getValue( iRechtswert );
      final double hochWertTwo = (Double) tempPointTwo.getValue( iHochwert );

      /* Geo-Projection */
      final GM_Point geoPointOne = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertOne, hochWertOne, srsName );
      final GM_Point geoPointTwo = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertTwo, hochWertTwo, srsName );

      /* Build the line segment. */
      final Coordinate geoCoordOne = JTSAdapter.export( transformer.transform( geoPointOne ).getCentroid().getPosition() );
      final Coordinate geoCoordTwo = JTSAdapter.export( transformer.transform( geoPointTwo ).getCentroid().getPosition() );
      final LineSegment geoSegment = new LineSegment( geoCoordOne, geoCoordTwo );

      /* Calculate the distance of the geo point to the line. */
      final double tempDistance = geoSegment.distance( comparePoint.getCoordinate() );

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
    final Coordinate geoCoordinate = segment.closestPoint( comparePoint.getCoordinate() );

    final double geoSegmentLength = segment.getLength();
    final double toGeoPointLength = JTSUtilities.getLengthBetweenPoints( segment.getCoordinate( 0 ), geoCoordinate );

    /* Using Breite to build. */
    final double breiteOne = (Double) pointOne.getValue( iBreite );
    final double breiteTwo = (Double) pointTwo.getValue( iBreite );

    /* Important: The interpolation is done here :). */
    final double toProfilePointLength = toGeoPointLength / geoSegmentLength * (breiteTwo - breiteOne);

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
  public static GM_Point getGeoPosition( final double width, final IProfil profile ) throws Exception
  {
    final IRecord[] geoReferencedPoints = ProfilUtil.getGeoreferencedPoints( profile );

    final String srsName = (String) profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );

    /* If no or only one geo referenced points are found, return. */
    if( geoReferencedPoints.length <= 1 )
      return null;

    // END OF FINDING GEOREFERENCED POINTS

    /* No we have a list with fully geo referenced points of a profile. */

    final int iRechtswert = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final int iHochwert = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final int iBreite = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iHoehe = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    for( int i = 0; i < geoReferencedPoints.length - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      final IRecord tempPointOne = geoReferencedPoints[i];
      final Double widthValueOne = (Double) tempPointOne.getValue( iBreite );
      final Double heigthValueOne = (Double) tempPointOne.getValue( iHoehe );
      final Double rechtsWertOne = (Double) tempPointOne.getValue( iRechtswert );
      final Double hochWertOne = (Double) tempPointOne.getValue( iHochwert );

      final IRecord tempPointTwo = geoReferencedPoints[i + 1];
      final Double widthValueTwo = (Double) tempPointTwo.getValue( iBreite );
      final Double heigthValueTwo = (Double) tempPointTwo.getValue( iHoehe );
      final Double rechtsWertTwo = (Double) tempPointTwo.getValue( iRechtswert );
      final Double hochWertTwo = (Double) tempPointTwo.getValue( iHochwert );

      /* find the right segment with the neighboring points */
      if( widthValueOne < width & widthValueTwo > width )
      {
        /* calculate the georeference */
        final double deltaOne = width - widthValueOne;
        final double delta = widthValueTwo - widthValueOne;
        final double x = deltaOne * (rechtsWertTwo - rechtsWertOne) / delta + rechtsWertOne;
        final double y = deltaOne * (hochWertTwo - hochWertOne) / delta + hochWertOne;
        final double z = deltaOne * (heigthValueTwo - heigthValueOne) / delta + heigthValueOne;

        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( x, y, z, srsName );
      }
      /* if the point is lying on the start point of the segment */
      else if( widthValueOne == width )
        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertOne, hochWertOne, heigthValueOne, srsName );
      else if( widthValueTwo == width )
        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rechtsWertTwo, hochWertTwo, heigthValueTwo, srsName );
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
  public static Double getHeigthPositionByWidth( final double width, final IProfil profile ) throws Exception
  {
    final IRecord[] points = profile.getPoints();
    if( points.length < 1 )
      return null;

    final int iBreite = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iHoehe = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    for( int i = 0; i < points.length - 1; i++ )
    {
      /* We need a line string of the to neighbour points. */
      final IRecord tempPointOne = points[i];
      final double widthValueOne = (Double) tempPointOne.getValue( iBreite );
      final double heigthValueOne = (Double) tempPointOne.getValue( iHoehe );

      final IRecord tempPointTwo = points[i + 1];
      final double widthValueTwo = (Double) tempPointTwo.getValue( iBreite );
      final double heigthValueTwo = (Double) tempPointTwo.getValue( iHoehe );

      /* searching for the right segment */
      if( widthValueOne <= width & widthValueTwo >= width )
        /* calculate the heigth */
        return (width - widthValueOne) * (heigthValueTwo - heigthValueOne) / (widthValueTwo - widthValueOne) + heigthValueOne;

    }
    if( width < (Double) points[0].getValue( iBreite ) )
      return (Double) points[0].getValue( iHoehe );
    else if( width > (Double) points[points.length - 1].getValue( iBreite ) )
      return (Double) points[points.length - 1].getValue( iHoehe );
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
  public static GM_Point[] calculateWspPoints( final IProfil profil, final double wspHoehe, String srsName )
  {
    final IComponent cHochwert = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent cRechtswert = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    /* ignore profile without geo-coordinates */
    if( cHochwert == null || cRechtswert == null )
      return new GM_Point[] {};

    final IComponent cHoehe = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int iHoehe = profil.indexOfProperty( cHoehe );
    final IComponent cBreite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iBreite = profil.indexOfProperty( cBreite );

    final IRecord[] points = profil.getPoints();
    final IRecord firstPoint = points[0];
    final IRecord lastPoint = points[points.length - 1];

    final double firstX = (Double) firstPoint.getValue( iBreite );
    final double firstY = (Double) firstPoint.getValue( iHoehe );
    final double lastX = (Double) lastPoint.getValue( iBreite );
    final double lastY = (Double) lastPoint.getValue( iHoehe );

    final Double[] breiteValues = ProfilUtil.getDoubleValuesFor( profil, cBreite );

    final PolyLine wspLine = new PolyLine( new double[] { firstX, lastX }, new double[] { wspHoehe, wspHoehe }, 0.0001 );
    final PolyLine profilLine = new PolyLine( breiteValues, ProfilUtil.getDoubleValuesFor( profil, cHoehe ), 0.0001 );

    /* Same for RW and HW, but filter 0-values */
    final PolyLine rwLine = createPolyline( profil, cBreite, cRechtswert );
    final PolyLine hwLine = createPolyline( profil, cBreite, cHochwert );

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

      if( srsName == null )
        srsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( rw ) );

      final GM_Point point = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( rw, hw, wspHoehe, srsName );

      poses[count++] = point;
    }

    return poses;
  }

  private static PolyLine createPolyline( final IProfil profil, final IComponent xProperty, final IComponent yProperty )
  {
    final IRecord[] points = profil.getPoints();

    final double[] xValues = new double[points.length];
    final double[] yValues = new double[points.length];
    final double dy = yProperty.getPrecision();
    final int xIndex = profil.indexOfProperty( xProperty );
    final int yIndex = profil.indexOfProperty( yProperty );
    int count = 0;
    for( final IRecord point : points )
    {
      final double x = (Double) point.getValue( xIndex );
      final double y = (Double) point.getValue( yIndex );

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

    final IRecord[] profilPointList = profile.getPoints();
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    /* set the coordinate system */
    final String crs = (String) profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    tmpProfil.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, crs );

    final IComponent cBreite = tmpProfil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent cHoehe = tmpProfil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent cHochwert = tmpProfil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent cRechtswert = tmpProfil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    if( !tmpProfil.hasPointProperty( cBreite ) )
      tmpProfil.addPointProperty( cBreite );
    if( !tmpProfil.hasPointProperty( cHoehe ) )
      tmpProfil.addPointProperty( cHoehe );
    if( !tmpProfil.hasPointProperty( cHochwert ) )
      tmpProfil.addPointProperty( cHochwert );
    if( !tmpProfil.hasPointProperty( cRechtswert ) )
      tmpProfil.addPointProperty( cRechtswert );

    final int iBreite = tmpProfil.indexOfProperty( cBreite );
    final int iHoehe = tmpProfil.indexOfProperty( cHoehe );
    final int iRechtswert = tmpProfil.indexOfProperty( cRechtswert );
    final int iHochwert = tmpProfil.indexOfProperty( cHochwert );

    final IRecord point1 = tmpProfil.createProfilPoint();
    final IRecord point2 = tmpProfil.createProfilPoint();

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    point1.setValue( iBreite, startWidth );
    point1.setValue( iHoehe, heigth1 );
    point1.setValue( iHochwert, geoPoint1.getY() );
    point1.setValue( iRechtswert, geoPoint1.getX() );

    point2.setValue( iBreite, endWidth );
    point2.setValue( iHoehe, heigth2 );
    point2.setValue( iHochwert, geoPoint2.getY() );
    point2.setValue( iRechtswert, geoPoint2.getX() );

    tmpProfil.addPoint( point1 );

    for( final IRecord point : profilPointList )
    {
      final double currentWidth = (Double) point.getValue( iBreite );
      if( currentWidth > startWidth & currentWidth < endWidth )
      {
        final IRecord pt = tmpProfil.createProfilPoint();

        final IComponent[] properties = orgIProfil.getPointProperties();
        for( final IComponent property : properties )
        {
          final int iProp = point.getOwner().indexOfComponent(  property );
          final Object value = point.getValue( iProp );
          pt.setValue( iProp, value );
        }
        tmpProfil.addPoint( pt );
      }
    }

    tmpProfil.addPoint( point2 );

    tmpProfil.setStation( orgIProfil.getStation() );
    return tmpProfil;
  }

  public static IRecord addRecordByWidth( final IProfil profile, final IRecord record, boolean overwritePointMarkers )
  {

    final Double width = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, record );

    final IRecord[] records = profile.getPoints();
    final int iBreite = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    for( int i = 0; i < records.length; i++ )
    {
      final IRecord r = records[i];
      final Double rw = (Double) r.getValue( iBreite );

      if( Math.abs( width - rw ) < FUZZINESS )
      {
        /* record already exists - copy values */

        for( final IComponent component : profile.getPointProperties() )
        {
          // don't overwrite existing point markers!
          if( !overwritePointMarkers && profile.isPointMarker( component.getId() ) )
            continue;
          final int index = profile.indexOfProperty( component );
          r.setValue( index, record.getValue( index ) );
        }
        return r;
      }
      else if( width < rw )
      {
        // add new record
        profile.getResult().add( i, record );
        return record;

      }
      else if( width == rw )
        throw new IllegalStateException();
    }

    profile.addPoint( record );

    return record;
  }

  /**
   * Adds a record by its width. If this record(point) already exists in the profile, the existing record will be
   * updated
   */
  public static IRecord addRecordByWidth( final IProfil profile, final IRecord record )
  {
    return addRecordByWidth( profile, record, false );
  }

}