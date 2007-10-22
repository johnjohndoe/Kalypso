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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilComparator;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Thomas Jung
 */
public class SegmentData
{
  private static final class ProfileSegmentData
  {
    public final IProfilPoint[] segmPoints;

    public final int startInd;

    public final int endInd;

    public final double distance;

    public int distInd;

    public ProfileSegmentData( final IProfilPoint[] points, final int start, final int end )
    {
      segmPoints = points;
      startInd = start;
      endInd = end;
      distance = maxSegmentDistance();
    }

    private final double maxSegmentDistance( )
    {
      double maxDistance = Double.NEGATIVE_INFINITY;
      if( (endInd - startInd) >= 2 )
        for( int i = 1; i < (endInd - startInd) - 1; i++ )
        {
          final double currentDistance = calcDistance( segmPoints[startInd], segmPoints[endInd], segmPoints[startInd + i] );
          if( currentDistance > maxDistance )
          {
            maxDistance = currentDistance;
            distInd = (startInd + i);
          }
        }
      else if( (endInd - startInd) == 2 )
      {
        maxDistance = calcDistance( segmPoints[startInd], segmPoints[endInd], segmPoints[startInd + 1] );
        distInd = (startInd + 1);
      }
      else if( (endInd - startInd) == 1 )
      {
        maxDistance = 0;
        distInd = startInd;
      }

      return maxDistance;
    }

    private double calcDistance( final IProfilPoint beginPoint, final IProfilPoint endPoint, final IProfilPoint middlePoint )
    {
      final double bx = beginPoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double by = beginPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      final double ex = endPoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double ey = endPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      final double mx = middlePoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double my = middlePoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );

      final double f = (ey - by) / (ex - bx);

      final double distance1 = (f * mx - 1 * my - f * bx + by) / Math.sqrt( 1 + f * f );
      return Math.abs( distance1 );
    }

  }

  // LineStrings derived from the original data
  private LineString m_profUpOrg;

  private LineString m_profDownOrg;

  private LineString m_bankLeftOrg;

  private LineString m_bankRightOrg;

  /* intersected lines */
  private LineString m_upIntersLinestring;

  private LineString m_downIntersLinestring;

  private LineString m_bankLeftInters;

  private LineString m_bankRightInters;

  private IProfil m_upIntersProfile;

  private IProfil m_downCroppedProfile;

  private IProfil m_downIntersProfile;

  /* intersection data */
  private final List<IntersPointData> m_intersPoints = new ArrayList<IntersPointData>();

  private int m_numBankIntersections;

  // original data
  private final Map<Feature, CreateChannelData.SIDE> m_bankLines;

  private final WspmProfile m_upProfile;

  private final WspmProfile m_DownProfile;

  private LineString m_upProfLineString;

  private LineString m_downProfLineString;

  private final CreateChannelData m_channelData;

  private IProfil m_upCroppedProfile;

  /* areas of the profiles */
  private double m_areaUpCroppedProfile;

  private double m_areaDownCroppedProfile;

  private double m_areaUpIntersProfile;

  private double m_areaDownIntersProfile;

  public SegmentData( final CreateChannelData channelData, final WspmProfile upProfile, final WspmProfile downProfile, final Map<Feature, CreateChannelData.SIDE> bankLines, final int numBankIntersections )
  {
    m_channelData = channelData;
    m_upProfile = upProfile;
    m_DownProfile = downProfile;
    m_bankLines = bankLines;

    m_upProfLineString = convertProfilesToLineStrings( upProfile );
    m_downProfLineString = convertProfilesToLineStrings( downProfile );
    m_numBankIntersections = numBankIntersections;

    intersectOrigBanks();
    m_bankLeftInters = intersectLineString( m_bankLeftOrg, m_numBankIntersections );
    m_bankRightInters = intersectLineString( m_bankRightOrg, m_numBankIntersections );

    if( m_bankLeftOrg != null & m_bankRightOrg != null )
      intersectOrigProfiles();

  }

  /**
   * GETTERS
   */
  public LineString getBankLeftOrg( )
  {
    return m_bankLeftOrg;
  }

  public LineString getBankRightOrg( )
  {
    return m_bankRightOrg;
  }

  public LineString getBankLeftInters( )
  {
    return m_bankLeftInters;
  }

  public LineString getBankRightInters( )
  {
    return m_bankRightInters;
  }

  public LineString getProfDownOrg( )
  {
    return m_profDownOrg;
  }

  public LineString getProfUpOrg( )
  {
    return m_profUpOrg;
  }

  public LineString getProfDownIntersLineString( )
  {
    return m_downIntersLinestring;
  }

  public LineString getProfUpIntersLineString( )
  {
    return m_upIntersLinestring;
  }

  public IProfil getProfilUpOrg( )
  {
    return m_upProfile.getProfil();
  }

  public IProfil getProfilDownOrg( )
  {
    return m_DownProfile.getProfil();
  }

  public IProfil getProfUpIntersProfile( )
  {
    return m_upIntersProfile;
  }

  public IProfil getProfDownIntersProfile( )
  {
    return m_downIntersProfile;
  }

  /**
   * Intersects the two WSPM profiles (upstream/downstream) with the allready intersected bank lines (left/right) of the
   * current segment.<br>
   * The two profiles will be cropped at the intersection points and intersected by a specific number of points. <br>
   * Afterwards an area adjustment for the intersected profiles will be done .
   */
  private void intersectOrigProfiles( )
  {
    /* get the cropped and intersected profiles & linestrings */

    // DOWNSTREAM
    try
    {
      /* crop the profile */
      // IProfil
      m_upCroppedProfile = createCroppedIProfile( m_upProfile, CreateChannelData.PROF.UP );
      // LineString
      m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );

      /* intersect the cropped profile */
      final IProfil tempUpIntersProfile = createIntersectedIProfile( m_upCroppedProfile );

      /*
       * area adjustment
       */

      /* the cropped profile area is the desired value for the intersected profile area */
      m_areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );
      m_areaUpIntersProfile = ProfilUtil.calcArea( tempUpIntersProfile );

      m_upIntersProfile = adjustProfileArea( tempUpIntersProfile, m_areaUpCroppedProfile, m_areaUpIntersProfile );

      final GeometryFactory factory = new GeometryFactory();
      m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // UPSTREAM
    try
    {
      /* crop the profile */
      // LineString
      m_downProfLineString = createCroppedProfileLineString( m_DownProfile, CreateChannelData.PROF.DOWN );
      // IProfil
      m_downCroppedProfile = createCroppedIProfile( m_DownProfile, CreateChannelData.PROF.DOWN );

      /* intersect the cropped profile */
      final IProfil tempDownIntersProfile = createIntersectedIProfile( m_downCroppedProfile );

      /*
       * area adjustment
       */

      /* the cropped profile area is the desired value for the intersected profile area */
      m_areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );
      m_areaDownIntersProfile = ProfilUtil.calcArea( tempDownIntersProfile );

      m_downIntersProfile = adjustProfileArea( tempDownIntersProfile, m_areaDownCroppedProfile, m_areaDownIntersProfile );

      final GeometryFactory factory = new GeometryFactory();
      m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * changes the node elevations of the inner points of an IProfil so that its crosssection area (currentArea) is the
   * same as the area of the cropped original IProfil (targetArea)
   * 
   * @param profile
   *            intersected profile for which the area adjustment shall be done
   * @param targetArea
   *            area of the original cropped profile
   * @param currentArea
   *            current area of the intersected profile
   */
  private IProfil adjustProfileArea( final IProfil profile, final double targetArea, final double currentArea )
  {
    final int numProfPoints = profile.getPoints().size();
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    IProfil tmpProfile2 = null;

    tmpProfil.setStation( profile.getStation() );
    if( m_downCroppedProfile != null & m_upCroppedProfile != null )
    {
      if( profile.getStation() == m_downCroppedProfile.getStation() )
        tmpProfile2 = m_downCroppedProfile;
      else
        tmpProfile2 = m_upCroppedProfile;
    }
    else if( m_downCroppedProfile == null )
      tmpProfile2 = m_upCroppedProfile;
    else
      tmpProfile2 = m_downCroppedProfile;

    final LinkedList<IProfilPoint> profilPointList = profile.getPoints();

    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    // calculate the area difference
    final double dArea = targetArea - currentArea;

    double dZ = 0;
    double wi = 0;

    // calculate the width of the the first and last segment and devide it by two (because of the triangle area of these
    // parts)
    final double startSegmentWidth = profile.getPoints().get( 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - profile.getPoints().get( 0 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double endSegmentWidth = profile.getPoints().get( numProfPoints - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE )
        - profile.getPoints().get( numProfPoints - 2 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    wi = 0.5 * (startSegmentWidth + endSegmentWidth);
    // add the width of the segments inbetween
    for( int i = 1; i < numProfPoints - 2; i++ )
    {
      wi = wi + profile.getPoints().get( i + 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    }
    dZ = dArea / wi;

    // String t = String.format( "Schlauchgenerator: Anpassung der Profilhöhen um: %f ", dZ, " m." );
    // System.out.println( t );

    // start point will not be changed
    tmpProfil.addPoint( profilPointList.get( 0 ).clonePoint() );

    // handle the points inbetween
    for( int i = 1; i < numProfPoints - 1; i++ )
    {
      final IProfilPoint point = tmpProfil.createProfilPoint();

      final double width = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigth = 0;
      try
      {
        heigth = WspmProfileHelper.getHeigthPositionByWidth( width, tmpProfile2 ) - dZ;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      final double x = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      final double y = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

      point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );

      tmpProfil.addPoint( point );
    }

    // end point wil be the same
    tmpProfil.addPoint( profilPointList.get( profilPointList.size() - 1 ).clonePoint() );

    // debugging and testing:
    final double areaNew = ProfilUtil.calcArea( tmpProfil );
    final double diffArea = targetArea - areaNew;
    if( diffArea > 0.10 )
    {
      // String s = String.format( "Schlauchgenerator: Flächenausgleich nicht hinreichend genau: %f - %f", targetArea,
      // areaNew
      // );
      // System.out.println( s );
    }

    return tmpProfil;
  }

  /**
   * initial intersection of an IProfil. There are two ways of intersection:<br> - if there are more profile points
   * than the wished number of intersection points, the intersection is done by Douglas-Peucker<br> - if there are not
   * enough profile points, the intersection is done with an equidistant approach.
   * 
   * @param profile
   *            input profile to be intersected.
   */
  private IProfil createIntersectedIProfile( final IProfil profile ) throws Exception
  {
    final int numProfInters = m_channelData.getNumProfileIntersections();
    final LinkedList<IProfilPoint> profilPointList = profile.getPoints();
    final int numProfPoints = profilPointList.size();

    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    tmpProfil.setStation( profile.getStation() );

    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    if( numProfInters > numProfPoints )
    {
      // start point
      IProfilPoint point = tmpProfil.createProfilPoint();
      double width = profilPointList.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      double heigth = profilPointList.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      double x = profilPointList.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      double y = profilPointList.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );
      tmpProfil.addPoint( point );

      /* do it by equidistant points */
      // keep in mind, that equidistants width doesn't get equidistant georeferenced lengths!
      final double startWidth = profilPointList.get( 0 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double endWidth = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double totalWidth = endWidth - startWidth;
      final double dWidth = totalWidth / (m_channelData.getNumProfileIntersections() - 1); // equidistant widths

      for( int i = 1; i < m_channelData.getNumProfileIntersections() - 1; i++ )
      {
        point = tmpProfil.createProfilPoint();

        width = startWidth + i * dWidth;
        heigth = WspmProfileHelper.getHeigthPositionByWidth( width, profile );
        final GM_Point geoPoint = WspmProfileHelper.getGeoPosition( width, profile );

        point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, geoPoint.getX() );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, geoPoint.getY() );

        tmpProfil.addPoint( point );
      }

      // end point
      point = tmpProfil.createProfilPoint();
      width = profilPointList.getLast().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      heigth = profilPointList.getLast().getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      x = profilPointList.getLast().getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      y = profilPointList.getLast().getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
      point.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );
      tmpProfil.addPoint( point );
    }
    else
    {
      // get the most important points (some kind of Douglas-Peucker)
      final IProfilPoint[] profPoints = profilPointList.toArray( new IProfilPoint[profilPointList.size()] );
      // do it by Douglas-Peucker
      final IProfilPoint[] DPpoints = findIProfileVIPPoints( profPoints, numProfInters );

      Arrays.sort( DPpoints, new ProfilComparator( IWspmConstants.POINT_PROPERTY_BREITE ) );
      for( final IProfilPoint element : DPpoints )
      {
        final IProfilPoint point = tmpProfil.createProfilPoint();
        final double width = element.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
        final double heigth = element.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
        final double x = element.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
        final double y = element.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

        point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
        point.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );

        tmpProfil.addPoint( point );
      }
    }
    return tmpProfil;
  }

  /**
   * intersects a specific linestring by a given number of intersections.<br>
   * the intersection is done by an equidistant approach. <BR>
   * this method is used for intersecting the banklines (Linestrings)
   * 
   * @param linestring
   *            input linestrin to be intersected
   * @param numIntersects
   *            number of intersections of the linestring
   */
  private LineString intersectLineString( final LineString linestring, final int numIntersects )
  {
    if( linestring == null )
      return null;

    // calculate the distance between the intersection points by the distance along the profile linestring.
    final double totaldistance = linestring.getLength();

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite
    /* for now: equidistant points */
    final double dDist = totaldistance / (numIntersects - 1); // equidistant widths
    final double[] dist = new double[numIntersects];
    final Point[] points = new Point[numIntersects];

    dist[0] = 0;
    points[0] = linestring.getStartPoint();

    for( int i = 1; i < dist.length - 1; i++ )
    {
      dist[i] = dist[0] + dDist * i;
      points[i] = JTSUtilities.pointOnLine( linestring, dist[i] );
    }
    dist[numIntersects - 1] = totaldistance;
    points[numIntersects - 1] = linestring.getEndPoint();

    final GM_Point[] gmpoints = new GM_Point[numIntersects]; // points for the LineString
    final GeometryFactory factory = new GeometryFactory();
    final Coordinate[] coordinates = new Coordinate[points.length];
    for( int i = 0; i < gmpoints.length; i++ )
    {
      try
      {
        gmpoints[i] = (GM_Point) JTSAdapter.wrap( points[i] );

        final double z = points[i].getCoordinate().z;

        if( !Double.isNaN( z ) )
        {
          coordinates[i] = new Coordinate( gmpoints[i].getX(), gmpoints[i].getY(), gmpoints[i].getZ() );
        }
        else
        {
          coordinates[i] = new Coordinate( gmpoints[i].getX(), gmpoints[i].getY() );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }
    return factory.createLineString( coordinates );
  }

  /**
   * calculates the intersection of a segment profile. at first, the width coordinates will be calculated and added to
   * the profile. next step is to intersect the profile <br>
   * (a) by Douglas-Peucker <br>
   * (b) equidistant. <br>
   * the geographical data is interpolated from the original profile data.
   * 
   * @param wspmprofile
   *            original profile (WSPMProfile) to be intersected
   * @param prof
   *            additional informations of the corresponding intersection points of that profile (upstream / downstream)
   */
  private IProfil createCroppedIProfile( final WspmProfile wspmprofile, final CreateChannelData.PROF prof ) throws Exception
  {
    final double width1 = calcWidthCoord( wspmprofile, prof, CreateChannelData.WIDTHORDER.FIRST );
    final double width2 = calcWidthCoord( wspmprofile, prof, CreateChannelData.WIDTHORDER.LAST );

    final Point firstPoint = getIntersPoint( prof, CreateChannelData.WIDTHORDER.FIRST );
    final Point lastPoint = getIntersPoint( prof, CreateChannelData.WIDTHORDER.LAST );
    final double startWidth;
    final double endWidth;
    final Point geoPoint1;
    final Point geoPoint2;

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

    // convert WSPM-Profil into IProfil an add the additional intersection width points.
    final IProfil orgIProfil = wspmprofile.getProfil();

    // calculate elevations
    final double heigth1 = WspmProfileHelper.getHeigthPositionByWidth( startWidth, orgIProfil );
    final double heigth2 = WspmProfileHelper.getHeigthPositionByWidth( endWidth, orgIProfil );

    final LinkedList<IProfilPoint> profilPointList = wspmprofile.getProfil().getPoints();

    final GM_Curve line = wspmprofile.getLine();

    final IProfil tmpProfil = ProfilFactory.createProfil( wspmprofile.getProfil().getType() );

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
    final GM_LineString lineString = line.getAsLineString(); // in the linestring the coordinates are already projected

    for( int i = 0; i < profilPointList.size(); i++ )
    {
      final IProfilPoint point = profilPointList.get( i );

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

        // set rw/hw to the projected coordinates
        pt.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, lineString.getPositionAt( i ).getX() );
        pt.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, lineString.getPositionAt( i ).getY() );

        tmpProfil.addPoint( pt );
      }
    }

    tmpProfil.addPoint( point2 );

    tmpProfil.setStation( orgIProfil.getStation() );
    return tmpProfil;
  }

  /**
   * gets the profile width coordinate of a specific intersection point corresponding to that profile
   * 
   * @param profile
   *            input profile
   * @param prof
   *            additional infos concerning the intersection point (profile (up/down))
   * @param widthorder
   *            additional infos concerning the intersection point (First/Last)
   */
  private double calcWidthCoord( final WspmProfile profile, final CreateChannelData.PROF prof, final CreateChannelData.WIDTHORDER widthorder )
  {
    // get the intersection point for the profile
    final Point point = getIntersPoint( prof, widthorder );
    // compute the width coordinates of the intersection points
    try
    {
      final double width = WspmProfileHelper.getWidthPosition( (GM_Point) JTSAdapter.wrap( point ), profile.getProfil(), profile.getSrsName() );
      return width;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * crops the WSPM profile at the intersection points and converts it to a LineString
   * 
   * @param profile
   *            input profile to be cropped
   * @param prof
   *            additional infos about the intersection points (wether it is the upstream / downstream profile of the
   *            current segment)
   * @see JTSUtilities.createLineSegment
   */
  private LineString createCroppedProfileLineString( final WspmProfile profile, final CreateChannelData.PROF prof ) throws GM_Exception
  {
    final LineString lsProfile = (LineString) JTSAdapter.export( profile.getLine() );
    final LineString intersProfile;
    final Point point1 = getIntersPoint( prof, CreateChannelData.SIDE.LEFT );
    final Point point2 = getIntersPoint( prof, CreateChannelData.SIDE.RIGHT );

    /* extract the LineStringSegment of the profile lying between the two corresponding intersection points */
    if( point1 != null & point2 != null )
    {
      intersProfile = JTSUtilities.createLineSegment( lsProfile, point1, point2 );
      return intersProfile;
    }
    else
      return null;
  }

  /**
   * Intersects the selected bank lines with the two profiles of the segment. This method will be called at the very
   * beginning. Here, the four intersection points will be initialised.
   */
  private void intersectOrigBanks( )
  {
    for( final Map.Entry<Feature, CreateChannelData.SIDE> bankEntry : m_bankLines.entrySet() )
    {
      final List<Point> intersPointList = new ArrayList<Point>();

      /* convert current bankLine in Curve */
      final Feature bankFeature = bankEntry.getKey();
      final CreateChannelData.SIDE side = bankEntry.getValue();

      final GM_MultiCurve multiline = (GM_MultiCurve) bankFeature.getDefaultGeometryProperty();
      if( multiline.getSize() > 1 )
        return;
      final GM_Curve bankCurve = multiline.getCurveAt( 0 );

      /* convert bank curve into LineString */
      try
      {
        final LineString bankLine = (LineString) JTSAdapter.export( bankCurve );

        /* intersect it with the first (previous) profile of this segment */
        final Geometry intersectionUpProfile = bankLine.intersection( m_upProfLineString );
        if( intersectionUpProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionUpProfile.getCoordinate().x;
          final double y = intersectionUpProfile.getCoordinate().y;
          double z = 0;
          double width = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionUpProfile );
            width = WspmProfileHelper.getWidthPosition( gmpoint, m_upProfile.getProfil(), m_upProfile.getSrsName() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_upProfile.getProfil() );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }

          final GeometryFactory factory = new GeometryFactory();

          final Coordinate coordinates = new Coordinate( x, y, z );
          final Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.UP, side, width );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionUpProfile );
        }
        else if( intersectionUpProfile instanceof GeometryCollection )
        {
          /*
           * if there are more than one intersection point for one profile, the bank line does not match correctly for
           * this section
           */
        }

        /* intersect it with the second (next) profile of this segment */
        final Geometry intersectionDownProfile = bankLine.intersection( m_downProfLineString );
        if( intersectionDownProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionDownProfile.getCoordinate().x;
          final double y = intersectionDownProfile.getCoordinate().y;
          double z = 0;
          double width = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionDownProfile );
            width = WspmProfileHelper.getWidthPosition( gmpoint, m_DownProfile.getProfil(), m_DownProfile.getSrsName() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_DownProfile.getProfil() );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }

          final GeometryFactory factory = new GeometryFactory();

          final Coordinate coordinates = new Coordinate( x, y, z );
          final Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.DOWN, side, width );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionDownProfile );
        }
        else if( intersectionDownProfile instanceof GeometryCollection )
        {
          /*
           * there should be only one intersection point for each proflie if there are more or less than two
           * intersection point for the two profiles, the bank lines does not match correctly for this section
           */
        }

        if( intersPointList.size() == 2 )
        {
          /* extract the LineStringSegment of the bankline lying between the two corresponding intersection points */
          final Point point1 = intersPointList.get( 0 );
          final Point point2 = intersPointList.get( 1 );

          switch( side )
          {
            case LEFT:
              if( m_bankLeftOrg == null )
                m_bankLeftOrg = JTSUtilities.createLineSegment( bankLine, point1, point2 );
              break;

            case RIGHT:
              if( m_bankRightOrg == null )
                m_bankRightOrg = JTSUtilities.createLineSegment( bankLine, point1, point2 );
              break;

            default:
              break;
          }
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }
    calcWidthOrder();
  }

  /**
   * sets the widthorder depending on the place of the intersection point in the profile context
   */
  private void calcWidthOrder( )
  {
    final IntersPointData DataUp[] = new IntersPointData[2];
    final IntersPointData DataDown[] = new IntersPointData[2];

    double width1 = 0;
    double width2 = 0;
    int j = 0;
    int k = 0;

    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      if( m_intersPoints.get( i ).getProf() == CreateChannelData.PROF.UP )
      {
        DataUp[j] = m_intersPoints.get( i );
        j = j + 1;
      }
      if( m_intersPoints.get( i ).getProf() == CreateChannelData.PROF.DOWN )
      {
        DataDown[k] = m_intersPoints.get( i );
        k = k + 1;
      }
    }
    if( j == 2 )
    {
      width1 = DataUp[0].getWidth();
      width2 = DataUp[1].getWidth();
      if( width2 > width1 )
      {
        DataUp[0].setWidthOrder( WIDTHORDER.FIRST );
        DataUp[1].setWidthOrder( WIDTHORDER.LAST );
      }
      else
      {
        DataUp[0].setWidthOrder( WIDTHORDER.LAST );
        DataUp[1].setWidthOrder( WIDTHORDER.FIRST );
      }
    }
    if( k == 2 )
    {
      width1 = DataDown[0].getWidth();
      width2 = DataDown[1].getWidth();
      if( width2 > width1 )
      {
        DataDown[0].setWidthOrder( WIDTHORDER.FIRST );
        DataDown[1].setWidthOrder( WIDTHORDER.LAST );
      }
      else
      {
        DataDown[0].setWidthOrder( WIDTHORDER.LAST );
        DataDown[1].setWidthOrder( WIDTHORDER.FIRST );
      }
    }

  }

  /**
   * converts a WSPM profile into an linestring
   * 
   * @param profile
   *            Input profile to be converted.
   */
  private LineString convertProfilesToLineStrings( final WspmProfile profile )
  {
    // get the profile line
    final GM_Curve profCurve = profile.getLine();
    try
    {
      final LineString profLine = (LineString) JTSAdapter.export( profCurve );
      return profLine;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public void paintSegment( final Graphics g, final MapPanel mapPanel ) throws Exception
  {
    // g.dispose();

    // if( m_bankRightOrg != null )
    // paintLineString( m_bankRightOrg, g, mapPanel, new Color( 150, 0, 0 ) );
    // if( m_bankLeftOrg != null )
    // paintLineString( m_bankLeftOrg, g, mapPanel, new Color( 0, 150, 0 ) );
    // if( m_profDownInters != null )
    // paintLinePoints( m_profDownInters, g, mapPanel, new Color( 100, 255, 50 ) );
    // if( m_profUpInters != null )
    // paintLinePoints( m_profUpInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankLeftInters != null )
      paintLineString( m_bankLeftInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankRightInters != null )
      paintLineString( m_bankRightInters, g, mapPanel, new Color( 100, 255, 50 ) );
  }

  private void paintLineStringPoints( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color )
  {
    if( line == null )
      return;

    final Color oldColor = g.getColor();
    g.setColor( color );

    for( int i = 0; i < line.getNumPoints(); i++ )
    {
      final int pointRectWidth = 8;
      final int halfRectWidth = pointRectWidth / 2;

      final double x = line.getPointN( i ).getCoordinate().x;
      final double y = line.getPointN( i ).getCoordinate().y;
      final int xs = (int) mapPanel.getProjection().getDestX( x );
      final int ys = (int) mapPanel.getProjection().getDestY( y );
      g.fill3DRect( xs - halfRectWidth, ys - halfRectWidth, pointRectWidth, pointRectWidth, true );
    }
    g.setColor( oldColor );
  }

  private void paintPoint( final Point point, final Graphics g, final MapPanel mapPanel, final Color color )
  {
    final Color oldColor = g.getColor();
    g.setColor( color );

    final int pointRectWidth = 8;
    final int halfRectWidth = pointRectWidth / 2;

    final double x = point.getCoordinate().x;
    final double y = point.getCoordinate().y;
    final int xs = (int) mapPanel.getProjection().getDestX( x );
    final int ys = (int) mapPanel.getProjection().getDestY( y );
    g.fill3DRect( xs - halfRectWidth, ys - halfRectWidth, pointRectWidth, pointRectWidth, true );

    g.setColor( oldColor );
  }

  public boolean complete( )
  {
    boolean check;
    check = false;

    if( m_downIntersLinestring != null & m_upIntersLinestring != null & m_bankLeftInters != null & m_bankRightInters != null )
    {
      check = true;
    }
    return check;
  }

  public boolean checkBankDataComplete( )
  {
    boolean check;
    check = false;

    if( m_bankLeftInters != null & m_bankRightInters != null )
    {
      check = true;
    }
    return check;
  }

  /**
   * gets the intersection point (Point) of the profile (prof) for the specified bank side (side).
   */
  private Point getIntersPoint( final CreateChannelData.PROF prof, final CreateChannelData.SIDE side )
  {
    Point intersPoint = null;
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( side == data.getSide() & prof == data.getProf() )
      {
        intersPoint = data.getPoint();
        return intersPoint;
      }
    }
    return null;
  }

  /**
   * gets the intersection point (Point) of the profile (prof) for the specified side (widthorder).
   */
  private Point getIntersPoint( final CreateChannelData.PROF prof, final CreateChannelData.WIDTHORDER widthorder )
  {
    Point intersPoint = null;
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( widthorder == data.getWidthOrder() & prof == data.getProf() )
      {
        intersPoint = data.getPoint();
        return intersPoint;
      }
    }
    return null;
  }

  /**
   * sets the intersection point (Point) of the profile (prof) for the specified bank side (side).
   */
  public void setIntersPoint( final GM_Point gmpoint, final PROF prof, final WIDTHORDER widthorder, final double width )
  {
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( prof == data.getProf() & data.getWidthOrder() == widthorder )
      {
        // store the old point to get the right banklne points
        final Point oldPoint = data.getPoint();

        final GeometryFactory factory = new GeometryFactory();
        final Point point = factory.createPoint( new Coordinate( gmpoint.getX(), gmpoint.getY(), gmpoint.getZ() ) );
        data.setPoint( point );
        data.setWidth( width );
        if( point.distance( oldPoint ) > 0.01 )
          updateBanklines( oldPoint, point );
      }
    }

  }

  /**
   * gets the most important profile points by sequently adding the points with the maximum distance to the segment
   * initially defined by the start and end point of the profile
   */
  protected IProfilPoint[] findIProfileVIPPoints( final IProfilPoint[] points, final int allowedNumPoints )
  {
    final List<IProfilPoint> pointsToKeep = new ArrayList<IProfilPoint>( allowedNumPoints - 1 );

    // store the first point of the input profile in the profile point list.
    pointsToKeep.add( points[0] );

    final LinkedList<ProfileSegmentData> profSegmentList = new LinkedList<ProfileSegmentData>();

    /* begin with the start and end point of the profile */
    final ProfileSegmentData startSegment = new ProfileSegmentData( points, 0, points.length - 1 );
    profSegmentList.add( startSegment );

    for( int i = 1; i < allowedNumPoints - 1; i++ )
    {
      double maxDist = Double.NEGATIVE_INFINITY;
      int indexMax = 0;

      for( int j = 0; j < profSegmentList.size(); j++ )
      {
        // find the maxDistanceSegment

        final ProfileSegmentData currentProfSegment = profSegmentList.get( j );
        final double currentDist = currentProfSegment.distance;
        if( currentDist > maxDist )
        {
          maxDist = currentDist;
          indexMax = j;
        }
      }
      // store the found maximum in the profile point list
      pointsToKeep.add( points[profSegmentList.get( indexMax ).distInd] );

      // split the maxDistanceSegment
      final ProfileSegmentData firstSplittedSegment = new ProfileSegmentData( points, profSegmentList.get( indexMax ).startInd, profSegmentList.get( indexMax ).distInd );
      final ProfileSegmentData secondSplittedSegment = new ProfileSegmentData( points, profSegmentList.get( indexMax ).distInd, profSegmentList.get( indexMax ).endInd );

      // store the new segments in the list
      profSegmentList.set( indexMax, firstSplittedSegment );
      profSegmentList.add( indexMax + 1, secondSplittedSegment );
    }
    pointsToKeep.add( points[points.length - 1] );
    return pointsToKeep.toArray( new IProfilPoint[pointsToKeep.size()] );
  }

  public int getNumBankIntersections( )
  {
    return m_numBankIntersections;
  }

  public void setNumBankIntersections( final int numIntersections )
  {
    m_numBankIntersections = numIntersections;
  }

  public void updateBankIntersection( )
  {
    if( m_bankLeftInters != null )
      m_bankLeftInters = intersectLineString( m_bankLeftOrg, m_numBankIntersections );
    if( m_bankRightInters != null )
      m_bankRightInters = intersectLineString( m_bankRightOrg, m_numBankIntersections );
  }

  public void updateBankIntersection2( )
  {
    if( m_bankLeftInters != null )
      m_bankLeftInters = intersectLineString( m_bankLeftInters, m_numBankIntersections );
    if( m_bankRightInters != null )
      m_bankRightInters = intersectLineString( m_bankRightInters, m_numBankIntersections );
  }

  /**
   * manages the update of the profile data, after the intersected profiles were chaged by the chart view layer in the
   * gui. things to do: -update the intersection points -> will be done by the layer -update the intersected banklines
   * -update the profiles (-> croping, intersecting, elevation adjusting)
   */
  public void updateProfileIntersection( )
  {
    if( complete() == true )
    {
      /* get the cropped and intersected profiles & linestrings */

      // muss jedes mal nach profile edit aufgerufen werden!
      // DOWNSTREAM
      try
      {

        /* crop the profile */
        // IProfil
        m_upCroppedProfile = createCroppedIProfile( m_upProfile, CreateChannelData.PROF.UP );

        /* the cropped profile area is the desired value for the intersected profile area */
        final double areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );

        /* intersect the cropped profile */
        // here not necessary, because the initial intersection was allready done. The intersection here will be
        // handeled by the user.
        // final IProfil tempPreviousIntersProfile = createIntersectedIProfile( m_previousCroppedProfile );
        // LineString
        // m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );
        final IProfil tmpupIntersProfile = adaptProfileElevations( m_upIntersProfile, m_upCroppedProfile );
        final double areaUpIntersProfile = ProfilUtil.calcArea( tmpupIntersProfile );

        // Flächenausgleich!!
        m_upIntersProfile = adjustProfileArea( m_upIntersProfile, areaUpCroppedProfile, areaUpIntersProfile );

        // m_upIntersProfile = createIntersectedIProfile( m_upCroppedProfile );

        final GeometryFactory factory = new GeometryFactory();
        m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      // UPSTREAM
      try
      {
        /* crop the profile */
        // IProfil
        m_downCroppedProfile = createCroppedIProfile( m_DownProfile, CreateChannelData.PROF.DOWN );

        /* the cropped profile area is the desired value for the intersected profile area */
        final double areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );

        /* intersect the cropped profile */
        // here not necessary, because the initial intersection was allready done. The intersection here will be
        // handeled by the user.
        // LineString
        // final IProfil tempNextIntersProfile = createIntersectedIProfile( m_nextCroppedProfile );
        final IProfil tmpdownIntersProfile = adaptProfileElevations( m_downIntersProfile, m_downCroppedProfile );
        final double areaDownIntersProfile = ProfilUtil.calcArea( tmpdownIntersProfile );

        // Flächenausgleich!!
        m_downIntersProfile = adjustProfileArea( m_downIntersProfile, areaDownCroppedProfile, areaDownIntersProfile );

        final GeometryFactory factory = new GeometryFactory();
        m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * For the area adjustment it is necessary first to get the original heigths of the cropped profile to the intersected
   * profile
   */
  private IProfil adaptProfileElevations( final IProfil intersProfile, final IProfil croppedProfile )
  {
    final LinkedList<IProfilPoint> profilPointList = intersProfile.getPoints();
    final IProfil tmpProfil = ProfilFactory.createProfil( intersProfile.getType() );

    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    for( final IProfilPoint point : profilPointList )
    {
      final double currentWidth = point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

      final IProfilPoint pt = tmpProfil.createProfilPoint();

      final IProfilPointProperty[] properties = intersProfile.getPointProperties();
      for( final IProfilPointProperty property : properties )
      {
        final String propertyId = property.toString();
        final double value = point.getValueFor( propertyId );
        pt.setValueFor( propertyId, value );
      }
      try
      {
        pt.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, WspmProfileHelper.getHeigthPositionByWidth( currentWidth, croppedProfile ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      tmpProfil.addPoint( pt );
    }
    return tmpProfil;
  }

  /**
   * gets the map extend (GM_Envelope) of the current segment
   */
  public GM_Envelope getSegmentMapExtend( )
  {
    final Geometry[] boundary = new Geometry[4];
    int i;

    i = 0;
    if( m_upIntersLinestring != null )
    {
      boundary[0] = m_upIntersLinestring.getBoundary();
      i = i + 1;
    }
    if( m_downIntersLinestring != null )
    {
      boundary[1] = m_downIntersLinestring.getBoundary();
      i = i + 1;
    }
    if( m_bankLeftInters != null )
    {
      boundary[2] = m_bankLeftInters.getBoundary();
      i = i + 1;
    }
    if( m_bankRightInters != null )
    {
      boundary[3] = m_bankRightInters.getBoundary();
      i = i + 1;
    }

    double maxX = 0;
    double maxY = 0;
    double minX = 0;
    double minY = 0;

    for( int j = 0; j < i; j++ )
    {
      Coordinate[] coords = new Coordinate[2];
      coords = boundary[j].getCoordinates();

      for( int k = 0; k < 2; k++ )
      {

        if( j == 0 & k == 0 )
        {
          maxX = coords[k].x;
          maxY = coords[k].y;
          minX = coords[k].x;
          minY = coords[k].y;
        }

        if( coords[k].x > maxX )
          maxX = coords[k].x;
        if( coords[k].y > maxY )
          maxY = coords[k].y;
        if( coords[k].x < minX )
          minX = coords[k].x;
        if( coords[k].x < minY )
          minY = coords[k].y;
      }
    }

    minX = minX - (maxX - minX) / 10;
    minY = minY - (maxY - minY) / 10;
    maxX = maxX + (maxX - minX) / 10;
    maxY = maxY + (maxY - minY) / 10;

    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );

  }

  /**
   * the editable bank linestring is painted
   */
  public void paintBankLineLineString( final MapPanel panel, final Graphics g, final int side, final Color color )
  {
    // paint the line
    if( side == 1 )
    {
      try
      {
        paintLineString( getBankLeftInters(), g, panel, color );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    else if( side == 2 )
    {
      try
      {
        paintLineString( getBankRightInters(), g, panel, color );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    // paint the nodes
    if( side == 1 )
    {
      paintLineStringPoints( getBankLeftInters(), g, panel, color );
    }
    else if( side == 2 )
    {
      paintLineStringPoints( getBankRightInters(), g, panel, color );
    }

  }

  @SuppressWarnings("unchecked")
  private void paintLineString( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color ) throws GM_Exception, CoreException
  {
    if( line == null )
      return;

    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final GM_Curve Curve = (GM_Curve) JTSAdapter.wrap( line );
    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
    defaultstroke = symb.getStroke();
    // float[] dArray = new float[3];
    // dArray[0] = 6;
    // dArray[1] = 3;
    stroke.setWidth( 3 );
    // stroke.setDashArray( dArray );
    stroke.setStroke( color );
    symb.setStroke( stroke );

    final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, Curve, symb );
    de.paint( g, mapPanel.getProjection(), new NullProgressMonitor() );

    // Set the Stroke back to default
    symb.setStroke( defaultstroke );

  }

  public void setBankLeftInters( final LineString bankLeftInters )
  {
    m_bankLeftInters = bankLeftInters;
  }

  public void setBankRightInters( final LineString bankRightInters )
  {
    m_bankRightInters = bankRightInters;
  }

  public void setBankLeftOrg( final LineString bankLeftInters )
  {
    m_bankLeftOrg = bankLeftInters;
  }

  public void setBankRightOrg( final LineString bankRightInters )
  {
    m_bankRightOrg = bankRightInters;
  }

  public void paintProfile( final CreateChannelData.PROF currentProfile, final MapPanel panel, final Graphics g, final Color color )
  {
    LineString line = null;
    final GeometryFactory factory = new GeometryFactory();

    if( currentProfile == PROF.UP )
      line = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );
    else if( currentProfile == PROF.DOWN )
      line = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

    try
    {
      paintLineString( line, g, panel, color );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    paintLineStringPoints( line, g, panel, color );

  }

  public void paintIntersectionPoints( final MapPanel panel, final Graphics g, final Color color, final PROF prof )
  {
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      if( m_intersPoints.get( i ).getProf() == prof )
      {
        final Point point = m_intersPoints.get( i ).getPoint();
        paintPoint( point, g, panel, color );
      }
    }

  }

  private Coordinate[] convertProfileToCoordinates( final IProfil profile )
  {
    final Coordinate[] coords = new Coordinate[profile.getPoints().size()];

    for( int i = 0; i < coords.length; i++ )
    {
      final double x = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      final double y = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
      final double z = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      coords[i] = new Coordinate( x, y, z );
    }
    return coords;
  }

  /**
   * sets the user edited profile as new intersected profile
   */
  public void setNewIntersectedProfile( final IProfil profile, final PROF prof )
  {
    if( prof == PROF.UP )
    {
      m_upIntersProfile = profile;
    }
    else if( prof == PROF.DOWN )
    {
      m_downIntersProfile = profile;
    }

    updateProfileIntersection();
  }

  /**
   * updates the intersected bankline linestring with the new edge point (moved by profile chart) by moving the
   * first/last line point (oldPoint) to the new location (newPoint).
   */
  private void updateBanklines( final Point oldPoint, final Point newPoint )
  {
    // find the correct bankline
    Point point = m_bankLeftInters.getPointN( 0 );

    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankLeftOrg = factory.createLineString( coords );
    }

    point = m_bankLeftInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[m_numBankIntersections - 1] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankLeftOrg = factory.createLineString( coords );
    }

    point = m_bankRightInters.getPointN( 0 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankRightOrg = factory.createLineString( coords );

    }

    point = m_bankRightInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[m_numBankIntersections - 1].setCoordinate( newPoint.getCoordinate() );

      final GeometryFactory factory = new GeometryFactory();
      m_bankRightOrg = factory.createLineString( coords );
    }
  }

}