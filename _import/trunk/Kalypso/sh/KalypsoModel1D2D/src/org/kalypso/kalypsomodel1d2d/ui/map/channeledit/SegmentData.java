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
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfileUtilities;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
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
  private Map<Feature, CreateChannelData.SIDE> m_bankLines;

  private WspmProfile m_upProfile;

  private WspmProfile m_DownProfile;

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

  public LineString getBankLeftInters( )
  {
    return m_bankLeftInters;
  }

  public LineString getBankLeftOrg( )
  {
    return m_bankLeftOrg;
  }

  public LineString getBankRightInters( )
  {
    return m_bankRightInters;
  }

  public LineString getBankRightOrg( )
  {
    return m_bankRightOrg;
  }

  public LineString getProfDownIntersLineString( )
  {
    return m_downIntersLinestring;
  }

  public LineString getProfDownOrg( )
  {
    return m_profDownOrg;
  }

  public LineString getProfUpIntersLineString( )
  {
    return m_upIntersLinestring;
  }

  public LineString getProfUpOrg( )
  {
    return m_profUpOrg;
  }

  public IProfil getProfilUpOrg( )
  {
    return m_upProfile.getProfil();
  }

  public IProfil getProfilDownOrg( )
  {
    return m_DownProfile.getProfil();
  }

  public IProfil getProfDownIntersProfile( )
  {
    return m_downIntersProfile;
  }

  public IProfil getProfUpCroppedProfile( )
  {
    return m_upCroppedProfile;
  }

  public IProfil getProfUpIntersProfile( )
  {
    return m_upIntersProfile;
  }

  /**
   * intersects the two selected WSPM profiles (upstream/downstream) with the allready intersected bank lines
   * (left/right) of the current segment
   */
  private void intersectOrigProfiles( )
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
      m_areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );

      /* intersect the cropped profile */
      final IProfil tempUpIntersProfile = createIntersectedIProfile( m_upCroppedProfile );
      // LineString
      m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );
      m_areaUpIntersProfile = ProfilUtil.calcArea( tempUpIntersProfile );

      // Flächenausgleich!!
      m_upIntersProfile = adjustProfileArea( tempUpIntersProfile, m_areaUpCroppedProfile, m_areaUpIntersProfile );

      GeometryFactory factory = new GeometryFactory();
      m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

    }
    catch( Exception e )
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

      /* the cropped profile area is the desired value for the intersected profile area */
      m_areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );

      /* intersect the cropped profile */
      final IProfil tempDownIntersProfile = createIntersectedIProfile( m_downCroppedProfile );
      m_areaDownIntersProfile = ProfilUtil.calcArea( tempDownIntersProfile );

      // Flächenausgleich!!
      m_downIntersProfile = adjustProfileArea( tempDownIntersProfile, m_areaDownCroppedProfile, m_areaDownIntersProfile );

      GeometryFactory factory = new GeometryFactory();
      m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  private IProfil adjustProfileArea( final IProfil profile, final double targetArea, final double currentArea )
  {
    final int numProfPoints = profile.getPoints().size();
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );
    
    tmpProfil.setStation( profile.getStation() );
    
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
    wi = wi + profile.getPoints().get( numProfPoints - 2 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - profile.getPoints().get( 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    dZ = dArea / wi;

    String t = String.format( "Schlauchgenerator: Anpassung der Profilhöhen um: %f ", dZ, " m." );
    System.out.println( t );

    // start point will not be changed
    tmpProfil.addPoint( profilPointList.get( 0 ).clonePoint() );

    // handle the points inbetween
    for( int i = 1; i < numProfPoints - 1; i++ )
    {
      IProfilPoint point = tmpProfil.createProfilPoint();

      final double width = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double heigth = profile.getPoints().get( i ).getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE ) - dZ;
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
      String s = String.format( "Schlauchgenerator: Flächenausgleich nicht hinreichend genau: %f - %f", targetArea, areaNew );
      System.out.println( s );
    }
    // /********************************

    return tmpProfil;
  }

  private IProfil createIntersectedIProfile( IProfil profile ) throws Exception
  {
    final int numProfInters = m_channelData.getNumProfileIntersections();
    final int numProfPoints = profile.getPoints().size();

    final LinkedList<IProfilPoint> profilPointList = profile.getPoints();
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );
    
    tmpProfil.setStation( profile.getStation() );
    
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

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

    if( numProfInters > numProfPoints )
    {
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

    }
    else
    {
      // do it by Douglas-Peucker

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

    return tmpProfil;
  }

  /**
   * intersects a specific linestring
   */
  private LineString intersectLineString( LineString linestring, int numIntersects )
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
    GeometryFactory factory = new GeometryFactory();
    Coordinate[] coordinates = new Coordinate[points.length];
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
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
    }
    return factory.createLineString( coordinates );
  }

  /**
   * calculates the intersection of a segment profile. at first the width coordinates will be calculated and added to
   * the profile. next step is to intersect the profile (a) by Douglas-Peucker (b) equidistant the geographical data is
   * interpolated from the original profile data. Input: original profile (WSPMProfile) to be intersected, corresponding
   * intersection points (CreateChannelData.PROF). Output: LineString of the intersected / interpolated profile points.
   * OBSOLETE!!! -> then delete it!!
   */

  private IProfil createCroppedIProfile( WspmProfile wspmprofile, CreateChannelData.PROF prof ) throws Exception
  {
    final double width1 = calcWidthCoord( wspmprofile, prof, CreateChannelData.SIDE.LEFT );
    final double width2 = calcWidthCoord( wspmprofile, prof, CreateChannelData.SIDE.RIGHT );

    // convert WSPM-Profil into IProfil an add the additional intersection width points.
    final IProfil orgIProfil = wspmprofile.getProfil();

    final IProfil tmpProfil = TuhhProfileUtilities.copyProfile( orgIProfil );

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    final double startWidth;
    final double endWidth;
    final Point geoPoint1;
    final Point geoPoint2;

    if( width1 > width2 )
    {
      // save this information to the intersectionPointData
      for( int i = 0; i < m_intersPoints.size(); i++ )
      {
        final IntersPointData data = m_intersPoints.get( i );
        if( data.getProf() == prof & data.getSide() == CreateChannelData.SIDE.LEFT )
          data.setWidthOrder( CreateChannelData.WIDTHORDER.LAST );
        if( data.getProf() == prof & data.getSide() == CreateChannelData.SIDE.RIGHT )
          data.setWidthOrder( CreateChannelData.WIDTHORDER.FIRST );
      }

      startWidth = width2;
      endWidth = width1;
      geoPoint1 = getIntersPoint( prof, CreateChannelData.SIDE.RIGHT );
      geoPoint2 = getIntersPoint( prof, CreateChannelData.SIDE.LEFT );
    }
    else
    {
      // save this information to the intersectionPointData
      for( int i = 0; i < m_intersPoints.size(); i++ )
      {
        final IntersPointData data = m_intersPoints.get( i );
        if( data.getProf() == prof & data.getSide() == CreateChannelData.SIDE.RIGHT )
          data.setWidthOrder( CreateChannelData.WIDTHORDER.FIRST );
        if( data.getProf() == prof & data.getSide() == CreateChannelData.SIDE.LEFT )
          data.setWidthOrder( CreateChannelData.WIDTHORDER.LAST );
      }

      startWidth = width1;
      endWidth = width2;
      geoPoint1 = getIntersPoint( prof, CreateChannelData.SIDE.LEFT );
      geoPoint2 = getIntersPoint( prof, CreateChannelData.SIDE.RIGHT );
    }

    ProfilUtil.croppProfile( tmpProfil, startWidth, endWidth );

    tmpProfil.getPoints().getFirst().setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, geoPoint1.getX() );
    tmpProfil.getPoints().getFirst().setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, geoPoint1.getY() );

    tmpProfil.getPoints().getLast().setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, geoPoint2.getX() );
    tmpProfil.getPoints().getLast().setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, geoPoint2.getY() );

    return tmpProfil;
  }

  /**
   * gets the profile width coordinate of a specific intersection point corresponding to that profile
   */
  private double calcWidthCoord( final WspmProfile profile, CreateChannelData.PROF prof, CreateChannelData.SIDE side )
  {
    // get the intersection point for the profile
    final Point point = getIntersPoint( prof, side );
    // compute the width coordinates of the intersection points
    try
    {
      final double width = WspmProfileHelper.getWidthPosition( (GM_Point) JTSAdapter.wrap( point ), profile.getProfil() );
      return width;
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * crops the WSPM profile at the intersection points and converts it to a LineString
   */
  private LineString createCroppedProfileLineString( WspmProfile profile, CreateChannelData.PROF prof ) throws GM_Exception
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
   * intersects the selected bank lines with the two profiles of the segment
   */
  private void intersectOrigBanks( )
  {
    // final Map<Point, Boolean> intersectionpoints = new HashMap<Point, Boolean>();
    for( final Map.Entry<Feature, CreateChannelData.SIDE> bankEntry : m_bankLines.entrySet() )
    {
      final List<Point> intersPointList = new ArrayList<Point>();

      /* convert current bankLine in Curve */
      final Feature bankFeature = bankEntry.getKey();
      final CreateChannelData.SIDE side = bankEntry.getValue();
      final GM_Curve bankCurve = (GM_Curve) bankFeature.getDefaultGeometryProperty();

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
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionUpProfile );
            final double width = WspmProfileHelper.getWidthPosition( gmpoint, m_upProfile.getProfil() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_upProfile.getProfil() );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }

          GeometryFactory factory = new GeometryFactory();

          Coordinate coordinates = new Coordinate( x, y, z );
          Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.UP, side );
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
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionDownProfile );
            final double width = WspmProfileHelper.getWidthPosition( gmpoint, m_DownProfile.getProfil() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_DownProfile.getProfil() );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }

          GeometryFactory factory = new GeometryFactory();

          Coordinate coordinates = new Coordinate( x, y, z );
          Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.DOWN, side );
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
          Point point = intersPointList.get( 0 );
          Point point2 = intersPointList.get( 1 );

          switch( side )
          {
            case LEFT:
              if( m_bankLeftOrg == null )
                m_bankLeftOrg = JTSUtilities.createLineSegment( bankLine, point, point2 );
              break;

            case RIGHT:
              if( m_bankRightOrg == null )
                m_bankRightOrg = JTSUtilities.createLineSegment( bankLine, point, point2 );
              break;

            default:
              break;
          }
        }
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  private LineString convertProfilesToLineStrings( WspmProfile profile )
  {
    // get the profile line
    final GM_Curve profCurve = profile.getLine();
    try
    {
      final LineString profLine = (LineString) JTSAdapter.export( profCurve );
      return profLine;
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public void paintSegment( final Graphics g, final MapPanel mapPanel ) throws GM_Exception
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
  private Point getIntersPoint( CreateChannelData.PROF prof, CreateChannelData.SIDE side )
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
   * sets the intersection point (Point) of the profile (prof) for the specified bank side (side).
   */
  public void setIntersPoint( GM_Point gmpoint, PROF prof, WIDTHORDER widthorder )
  {
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( prof == data.getProf() & data.getWidthOrder() == widthorder )
      {
        // store the old point to get the right banklne points
        Point oldPoint = data.getPoint();

        GeometryFactory factory = new GeometryFactory();
        Point point = factory.createPoint( new Coordinate( gmpoint.getX(), gmpoint.getY(), gmpoint.getZ() ) );
        data.setPoint( point );

        if( point.distance( oldPoint ) > 0.01 )
          updateBanklines( oldPoint, point );
      }
    }

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

    final double distance = (f * mx - 1 * my - f * bx + by) / Math.sqrt( 1 + f * f );
    return Math.abs( distance );
  }

  /*
   * // /** @return the points witch are redundant
   */
  // private Point[] reduceIt( final Point[] points, final int begin, final int end, final double allowedDistance )
  // throws ProfilDataException
  // {
  // if( end - begin < 2 )
  // return new Point[0];
  //
  // // für alle punkte abstand zu segment[begin-end] ausrechnen
  // final double[] distances = new double[end - (begin + 1)];
  // double maxdistance = 0.0;
  // int maxdistIndex = -1;
  //
  // for( int i = 0; i < distances.length; i++ )
  // {
  // final double distance = calcDistance( points[begin], points[end], points[i + begin + 1] );
  // distances[i] = distance;
  //
  // if( distance > maxdistance )
  // {
  // maxdistance = distance;
  // maxdistIndex = i + begin + 1;
  // }
  // }
  // }
  /** @return the points witch are redundant */
  private IProfilPoint[] reduceIt( final IProfilPoint[] points, final int begin, final int end, final double allowedDistance ) throws ProfilDataException
  {
    if( end - begin < 2 )
      return new IProfilPoint[0];

    // für alle punkte abstand zu segment[begin-end] ausrechnen
    final double[] distances = new double[end - (begin + 1)];
    double maxdistance = 0.0;
    int maxdistIndex = -1;
    for( int i = 0; i < distances.length; i++ )
    {
      final double distance = calcDistance( points[begin], points[end], points[i + begin + 1] );
      distances[i] = distance;

      if( distance > maxdistance )
      {
        maxdistance = distance;
        maxdistIndex = i + begin + 1;
      }
    }

    // falls ein punkt dabei, dessen diff > maxdiff, splitten
    if( maxdistance > allowedDistance && maxdistIndex != -1 )
    {
      final IProfilPoint[] beginReduced = reduceIt( points, begin, maxdistIndex, allowedDistance );
      final IProfilPoint[] endReduced = reduceIt( points, maxdistIndex, end, allowedDistance );
      final List<IProfilPoint> reduced = new ArrayList<IProfilPoint>( beginReduced.length + endReduced.length );
      reduced.addAll( Arrays.asList( beginReduced ) );
      reduced.addAll( Arrays.asList( endReduced ) );
      return reduced.toArray( new IProfilPoint[reduced.size()] );
    }

    // kein Punkt mehr wichtig: alle zwischenpunkte zurückgeben
    final IProfilPoint[] reduced = new IProfilPoint[end - (begin + 1)];
    for( int i = 0; i < reduced.length; i++ )
      reduced[i] = points[i + begin + 1];

    return reduced;
  }

  protected Point[] reducePoints( final Point[] points, final Point[] pointsToKeep, final double allowedDistance ) 
  {
    // reduce segment wise
    final Set<Point> pointsToKeepList = new HashSet<Point>( Arrays.asList( pointsToKeep ) );
    final List<Point> pointsToRemove = new ArrayList<Point>( points.length - 2 );

    int segmentBegin = 0;
    for( int i = 0; i < points.length; i++ )
    {
      if( i == segmentBegin )
        continue;

      final Point point = points[i];
      if( pointsToKeepList.contains( point ) || i == points.length - 1 )
      {
        // final Point[] toRemove = reduceIt( points, segmentBegin, i, allowedDistance );
        // pointsToRemove.addAll( Arrays.asList( toRemove ) );
        segmentBegin = i;
      }
    }

    return pointsToRemove.toArray( new Point[pointsToRemove.size()] );
  }

  protected IProfilPoint[] reducePoints( final IProfilPoint[] points, final IProfilPoint[] pointsToKeep, final double allowedDistance ) throws ProfilDataException
  {
    // reduce segment wise
    final Set<IProfilPoint> pointsToKeepList = new HashSet<IProfilPoint>( Arrays.asList( pointsToKeep ) );
    final List<IProfilPoint> pointsToRemove = new ArrayList<IProfilPoint>( points.length - 2 );

    int segmentBegin = 0;
    for( int i = 0; i < points.length; i++ )
    {
      if( i == segmentBegin )
        continue;

      final IProfilPoint point = points[i];
      if( pointsToKeepList.contains( point ) || i == points.length - 1 )
      {
        final IProfilPoint[] toRemove = reduceIt( points, segmentBegin, i, allowedDistance );
        pointsToRemove.addAll( Arrays.asList( toRemove ) );
        segmentBegin = i;
      }
    }

    return pointsToRemove.toArray( new IProfilPoint[pointsToRemove.size()] );
  }

  public int getNumBankIntersections( )
  {
    return m_numBankIntersections;
  }

  public void setNumBankIntersections( int numIntersections )
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

//  /**
//   * sets the new banklines because of user-changes in the intersected profiles and intersection points. takes the old
//   * intersected bankline and moves their start and end points to the new intersection points.
//   */
//  public void setNewBankIntersection( PROF prof )
//  {
//    /*
//     * check all the start and end point of the allready intersected bank line 
//     * and compare them with the intersection points?!
//     */
//    
//    // left side
//    
//    Point startPoint = m_bankLeftInters.getStartPoint();
//    Point endPoint = m_bankLeftInters.getEndPoint();
//
//    GeometryFactory factory = new GeometryFactory();
//    Coordinate[] coords = m_bankLeftInters.getCoordinates();
//
//    for( int i = 0; i < m_intersPoints.size(); i++ )
//    {
//      if( m_intersPoints.get( i ).getSide().LEFT == CreateChannelData.SIDE.LEFT & m_intersPoints.get( i ).getProf() == prof )
//      {
//        if( m_intersPoints.get( i ).getPoint().intersects( endPoint ) )
//        {
//
//        }
//      }
//
//    }
//
//    // m_bankLeftInters =
//    // m_bankRightInters =
//
//  }

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
        //m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );
        final double areaUpIntersProfile = ProfilUtil.calcArea( m_upIntersProfile );

        // Flächenausgleich!!
        // m_upIntersProfile = adjustProfileArea( m_upIntersProfile, areaUpCroppedProfile, areaUpIntersProfile );
        
        m_upIntersProfile = createIntersectedIProfile( m_upCroppedProfile );
        
        GeometryFactory factory = new GeometryFactory();
        m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

      }
      catch( Exception e )
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
        final double areaDownIntersProfile = ProfilUtil.calcArea( m_downIntersProfile );

        // Flächenausgleich!!
        // m_downIntersProfile = adjustProfileArea( m_downIntersProfile, areaDownCroppedProfile, areaDownIntersProfile
        // );

        GeometryFactory factory = new GeometryFactory();
        m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * intersects a profile linestring <br>
   * (a) after initialisation of the widget <br>
   * (b) after changing the number of profile intersection points, the intersection will be done by the
   * Douglas-Peucker-Algorithm <br>
   * if there are less profile points than the specified number of intersections, an equidistant approach will be done.<br>
   * if the linestring has allready the right number of points, nothing will be done.
   */
  private LineString intersectProfileLineString( LineString profLinestring, int numProfIntersects )
  {
    if( profLinestring == null )
      return null;

    // calculate the distance between the intersection points by the distance along the profile linestring.
    final double totaldistance = profLinestring.getLength();

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite

    if( numProfIntersects > profLinestring.getNumPoints() )
    {
      /*
       * equidistant widths, this attempt will be used, if there are not enough profile points to reach the number of
       * intersection points
       */
      final double dDist = totaldistance / (numProfIntersects - 1); // equidistant widths
      final double[] dist = new double[numProfIntersects];
      final Point[] points = new Point[numProfIntersects];

      dist[0] = 0;
      points[0] = profLinestring.getStartPoint();

      for( int i = 1; i < dist.length - 1; i++ )
      {
        dist[i] = dist[0] + dDist * i;
        points[i] = JTSUtilities.pointOnLine( profLinestring, dist[i] );
      }
      dist[numProfIntersects - 1] = totaldistance;
      points[numProfIntersects - 1] = profLinestring.getEndPoint();

      final GM_Point[] gmpoints = new GM_Point[numProfIntersects]; // points for the LineString
      GeometryFactory factory = new GeometryFactory();
      Coordinate[] coordinates = new Coordinate[points.length];
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
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      }
      return factory.createLineString( coordinates );
    }
    else if( numProfIntersects < profLinestring.getNumPoints() )
    {
      /* TODO: implement Douglas-Peucker for width calculation */
      final double dDist = totaldistance / (numProfIntersects - 1); // equidistant widths
      final double[] dist = new double[numProfIntersects];
      final Point[] points = new Point[numProfIntersects];

      dist[0] = 0;
      points[0] = profLinestring.getStartPoint();

      for( int i = 1; i < dist.length - 1; i++ )
      {
        dist[i] = dist[0] + dDist * i;
        points[i] = JTSUtilities.pointOnLine( profLinestring, dist[i] );
      }
      dist[numProfIntersects - 1] = totaldistance;
      points[numProfIntersects - 1] = profLinestring.getEndPoint();
      final GM_Point[] gmpoints = new GM_Point[numProfIntersects]; // points for the LineString
      GeometryFactory factory = new GeometryFactory();
      Coordinate[] coordinates = new Coordinate[points.length];

      return factory.createLineString( coordinates );
    }
    /* the input profile linestring is allready correct intersected */
    return profLinestring;
  }

  /**
   * gets the map extend (GM_Envelope) of the current segment
   */
  public GM_Envelope getSegmentMapExtend( )
  {
    GM_Envelope segmentMapExtend = null;
    Geometry[] boundary = new Geometry[4];
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
  public void paintBankLineLineString( MapPanel panel, Graphics g, int side, final Color color )
  {
    // paint the line
    if( side == 1 )
    {
      try
      {
        paintLineString( getBankLeftInters(), g, panel, color );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    else if( side == 2 )
    {
      try
      {
        paintLineString( getBankRightInters(), g, panel, color );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
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

  private void paintLineString( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color ) throws GM_Exception
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

    DisplayElement de;
    try
    {
      de = DisplayElementFactory.buildLineStringDisplayElement( null, Curve, symb );
      de.paint( g, mapPanel.getProjection() );
    }
    catch( IncompatibleGeometryTypeException e )
    {
      e.printStackTrace();
    }

    // Set the Stroke back to default
    symb.setStroke( defaultstroke );

  }

  public void setBankLeftInters( LineString bankLeftInters )
  {
    m_bankLeftInters = bankLeftInters;
  }

  public void setBankRightInters( LineString bankRightInters )
  {
    m_bankRightInters = bankRightInters;
  }

  public void paintProfile( CreateChannelData.PROF currentProfile, MapPanel panel, Graphics g, Color color )
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
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
    paintLineStringPoints( line, g, panel, color );

  }

  public void paintIntersectionPoints( MapPanel panel, Graphics g, Color color, PROF prof )
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

  private Coordinate[] convertProfileToCoordinates( IProfil profile )
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

  public void setNewIntersectedProfile( IProfil profile, PROF prof )
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
  private void updateBanklines( Point oldPoint, Point newPoint )
  {
    // find the correct bankline
    Point point = m_bankLeftInters.getPointN( 0 );

    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      GeometryFactory factory = new GeometryFactory();
      m_bankLeftInters = factory.createLineString( coords );
    }

    point = m_bankLeftInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[m_numBankIntersections - 1] = newPoint.getCoordinate();

      GeometryFactory factory = new GeometryFactory();
      m_bankLeftInters = factory.createLineString( coords );
    }

    point = m_bankRightInters.getPointN( 0 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      GeometryFactory factory = new GeometryFactory();
      m_bankRightInters = factory.createLineString( coords );

    }

    point = m_bankRightInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[m_numBankIntersections - 1] = newPoint.getCoordinate();

      GeometryFactory factory = new GeometryFactory();
      m_bankRightInters = factory.createLineString( coords );
    }
  }
}