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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;

import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.Fill_Impl;
import org.kalypsodeegree_impl.graphics.sld.Graphic_Impl;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Mark_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
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
  private LineString m_profUpInters;

  private LineString m_profDownInters;

  private LineString m_bankLeftInters;

  private LineString m_bankRightInters;

  /* intersection data */
  private final List<IntersPointData> m_intersPoints = new ArrayList<IntersPointData>();

  private final int m_numBankIntersections;

  // private IProfil m_intersUpIProfil;

  // private IProfil m_intersDownIProfil;

  // original data
  private Map<Feature, CreateChannelData.SIDE> m_bankLines;

  private WspmProfile m_previousProfile;

  private WspmProfile m_nextProfile;

  private LineString m_previousProfLineString;

  private LineString m_nextProfLineString;

  private final CreateChannelData m_channelData;

  public SegmentData( final CreateChannelData channelData, final WspmProfile previousProfile, final WspmProfile nextProfile, final Map<Feature, CreateChannelData.SIDE> bankLines, final int numBankIntersections )
  {

    m_channelData = channelData;
    m_previousProfile = previousProfile;
    m_nextProfile = nextProfile;
    m_bankLines = bankLines;

    m_previousProfLineString = convertProfilesToLineStrings( previousProfile );
    m_nextProfLineString = convertProfilesToLineStrings( nextProfile );
    m_numBankIntersections = numBankIntersections;

    intersectOrigBanks();
    m_bankLeftInters = intersectLineString( m_bankLeftOrg, m_numBankIntersections );
    m_bankRightInters = intersectLineString( m_bankRightOrg, m_numBankIntersections );

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

  public LineString getProfDownInters( )
  {
    return m_profDownInters;
  }

  public LineString getProfDownOrg( )
  {
    return m_profDownOrg;
  }

  public LineString getProfUpInters( )
  {
    return m_profUpInters;
  }

  public LineString getProfUpOrg( )
  {
    return m_profUpOrg;
  }

  /**
   * intersects the two selected WSPM profiles (upstream/downstream) with the allready intersected bank lines
   * (left/right) of the current segment
   */
  private void intersectOrigProfiles( )
  {
    /* get the profile linestrings */

    // TODO: Flächenausgleich!!
    // DOWNSTREAM
    try
    {
      m_previousProfLineString = createCroppedProfileLiineString( m_previousProfile, CreateChannelData.PROF.DOWN );
      m_profDownInters = intersectLineString( m_previousProfLineString, m_channelData.getNumProfileIntersections() );

      // m_profDownInters = intersectProfile( m_previousProfile, CreateChannelData.PROF.DOWN );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    // UPSTREAM
    try
    {
      m_nextProfLineString = createCroppedProfileLiineString( m_nextProfile, CreateChannelData.PROF.UP );
      m_profUpInters = intersectLineString( m_nextProfLineString, m_channelData.getNumProfileIntersections() );
      // m_profUpInters = intersectProfile( m_nextProfile, CreateChannelData.PROF.UP );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * intersects a specific linestring
   */
  private LineString intersectLineString( LineString linestring, int numIntersects )
  {

    // calculate the distance between the intersection points by the distance along the profile linestring.
    final double totaldistance = linestring.getLength();

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite

    /* TODO: implement Douglas-Peucker for width calculation */

    /* for now: equidistant widths */
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

  // /**
  // * intersects a specific linestring
  // */
  // //private LineString intersectProfileLineString( LineString ProfLineString ) throws GM_Exception
  // {
  //
  // // calculate the distance between the intersection points by the distance along the profile linestring.
  // final double totaldistance = ProfLineString.getLength();
  //
  // // then compute the additional coodinates of the intersected profile linestring
  // // by the given spinner data of the composite
  // final int numProfInters = m_channelData.getNumProfileIntersections();
  //
  // /* TODO: implement Douglas-Peucker for width calculation */
  //
  // /* for now: equidistant widths */
  // final double dDist = totaldistance / (numProfInters - 1); // equidistant widths
  // final double[] dist = new double[numProfInters];
  // final Point[] points = new Point[numProfInters];
  //
  // dist[0] = 0;
  // points[0] = ProfLineString.getStartPoint();
  //
  // for( int i = 1; i < dist.length - 1; i++ )
  // {
  // dist[i] = dist[0] + dDist * i;
  // points[i] = JTSUtilities.pointOnLine( ProfLineString, dist[i] );
  // }
  // dist[numProfInters - 1] = totaldistance;
  // points[numProfInters - 1] = ProfLineString.getEndPoint();
  //
  // final GM_Point[] gmpoints = new GM_Point[numProfInters]; // points for the LineString
  // GeometryFactory factory = new GeometryFactory();
  // Coordinate[] coordinates = new Coordinate[points.length];
  // for( int i = 0; i < gmpoints.length; i++ )
  // {
  // gmpoints[i] = (GM_Point) JTSAdapter.wrap( points[i] );
  // coordinates[i] = new Coordinate( gmpoints[i].getX(), gmpoints[i].getY(), gmpoints[i].getZ() );
  // }
  // return factory.createLineString( coordinates );
  // }

  /**
   * calculates the intersection of a segment profile. at first the width coordinates will be calculated and added to
   * the profile. next step is to intersect the profile (a) by Douglas-Peucker (b) equidistant the geographical data is
   * interpolated from the original profile data. Input: original profile (WSPMProfile) to be intersected, corresponding
   * intersection points (CreateChannelData.PROF). Output: LineString of the intersected / interpolated profile points.
   * OBSOLETE!
   */
  private LineString intersectProfile( WspmProfile wspmprofile, CreateChannelData.PROF prof ) throws Exception
  {
    final double width1 = calcWidthCoord( wspmprofile, prof, CreateChannelData.SIDE.LEFT );
    final double width2 = calcWidthCoord( wspmprofile, prof, CreateChannelData.SIDE.RIGHT );

    // convert WSPM-Profil into IProfil an add the additional intersection width points.
    IProfil intersIProfil = wspmprofile.getProfil();

    // calculate elevations
    final double heigth1 = WspmProfileHelper.getHeigthPositionByWidth( width1, intersIProfil );
    final double heigth2 = WspmProfileHelper.getHeigthPositionByWidth( width2, intersIProfil );

    final IProfilPoint point1 = intersIProfil.createProfilPoint();
    final IProfilPoint point2 = intersIProfil.createProfilPoint();

    point1.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width1 );
    point2.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width2 );
    point1.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth1 );
    point2.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth2 );

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    final double startWidth;
    final double endWidth;

    if( width1 > width2 )
    {
      startWidth = width2;
      endWidth = width1;
    }
    else
    {
      startWidth = width1;
      endWidth = width2;
    }
    final double totalwidth = endWidth - startWidth;

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite
    final int numProfInters = m_channelData.getNumProfileIntersections();

    /* TODO: implement Douglas-Peucker for width calculation */

    /* for now: equidistant widths */
    final double dWith = totalwidth / (numProfInters - 1); // equidistant widths
    final double[] widths = new double[numProfInters];
    final GM_Point[] points = new GM_Point[numProfInters]; // points for the LineString

    widths[0] = startWidth;
    points[0] = WspmProfileHelper.getGeoPosition( widths[0], intersIProfil );

    for( int i = 1; i < widths.length - 1; i++ )
    {
      widths[i] = startWidth + dWith * i;
      points[i] = WspmProfileHelper.getGeoPosition( widths[i], intersIProfil );
    }
    widths[numProfInters - 1] = endWidth;
    points[numProfInters - 1] = WspmProfileHelper.getGeoPosition( widths[numProfInters - 1], intersIProfil );

    // at last create a linestring for all the points
    GeometryFactory factory = new GeometryFactory();
    Coordinate[] coordinates = new Coordinate[points.length];
    for( int i = 0; i < points.length; i++ )
    {
      coordinates[i] = new Coordinate( points[i].getX(), points[i].getY(), points[i].getZ() );
    }
    return factory.createLineString( coordinates );
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
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * crops the WSPM profile at the intersection points and converts it to a LineString
   */
  private LineString createCroppedProfileLiineString( WspmProfile profile, CreateChannelData.PROF prof ) throws GM_Exception
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
        final Geometry intersectionPreviousProfile = bankLine.intersection( m_previousProfLineString );
        if( intersectionPreviousProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionPreviousProfile.getCoordinate().x;
          final double y = intersectionPreviousProfile.getCoordinate().y;
          double z = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionPreviousProfile );
            final double width = WspmProfileHelper.getWidthPosition( gmpoint, m_previousProfile.getProfil() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_previousProfile.getProfil() );
          }
          catch( Exception e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }

          GeometryFactory factory = new GeometryFactory();

          Coordinate coordinates = new Coordinate( x, y, z );
          Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.DOWN, side );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionPreviousProfile );
        }
        else if( intersectionPreviousProfile instanceof GeometryCollection )
        {
          /*
           * if there are more than one intersection point for one profile, the bank line does not match correctly for
           * this section
           */
        }

        /* intersect it with the second (next) profile of this segment */
        final Geometry intersectionNextProfile = bankLine.intersection( m_nextProfLineString );
        if( intersectionNextProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionNextProfile.getCoordinate().x;
          final double y = intersectionNextProfile.getCoordinate().y;
          double z = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionNextProfile );
            final double width = WspmProfileHelper.getWidthPosition( gmpoint, m_nextProfile.getProfil() );
            z = WspmProfileHelper.getHeigthPositionByWidth( width, m_nextProfile.getProfil() );
          }
          catch( Exception e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }

          GeometryFactory factory = new GeometryFactory();

          Coordinate coordinates = new Coordinate( x, y, z );
          Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.UP, side );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionNextProfile );
        }
        else if( intersectionNextProfile instanceof GeometryCollection )
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
        // TODO Auto-generated catch block
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
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  private LineString convertBanksToLineStrings( Feature bank )
  {
    // get the profile line
    final GM_Curve bankCurve = (GM_Curve) bank.getDefaultGeometryProperty();
    try
    {
      final LineString bankLine = (LineString) JTSAdapter.export( bankCurve );
      return bankLine;
    }
    catch( GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  public void paintSegment( final Graphics g, final MapPanel mapPanel ) throws GM_Exception
  {
    if( m_bankRightOrg != null )
      paintBankLine( m_bankRightOrg, g, mapPanel, new Color( 150, 0, 0 ) );
    if( m_bankLeftOrg != null )
      paintBankLine( m_bankLeftOrg, g, mapPanel, new Color( 0, 150, 0 ) );
//    if( m_profDownInters != null )
//      paintLinePoints( m_profDownInters, g, mapPanel, new Color( 100, 255, 50 ) );
//    if( m_profUpInters != null )
//      paintLinePoints( m_profUpInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankLeftInters != null )
      paintLinePoints( m_bankLeftInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankRightInters != null )
      paintLinePoints( m_bankRightInters, g, mapPanel, new Color( 100, 255, 50 ) );
    // if( m_intersPoints != null )
    // {
    // final Point[] points = new Point[m_intersPoints.size()];
    // for( int i = 0; i < m_intersPoints.size(); i++ )
    // {
    // points[i] = m_intersPoints.get( i ).getPoint() ;
    // }
    // paintPoints( points, g, mapPanel, new Color( 100, 255, 50 ) );
    // }
  }

  private void paintLinePoints( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color )
  {
    final PointSymbolizer symb = new PointSymbolizer_Impl();
    DisplayElement de;

    for( int i = 0; i < line.getNumPoints(); i++ )
    {
      GM_Point point2;
      try
      {
        point2 = (GM_Point) JTSAdapter.wrap( line.getPointN( i ) );
        de = DisplayElementFactory.buildPointDisplayElement( null, point2, symb );
        de.paint( g, mapPanel.getProjection() );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private void paintPoints( final Point[] points, final Graphics g, final MapPanel mapPanel, final Color color )
  {
    final PointSymbolizer symb = new PointSymbolizer_Impl();
    DisplayElement de;

    for( int i = 0; i < points.length; i++ )
    {
      try
      {
        GM_Point point = (GM_Point) JTSAdapter.wrap( points[i] );
        de = DisplayElementFactory.buildPointDisplayElement( null, point, symb );
        de.paint( g, mapPanel.getProjection() );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private void paintBankLine( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color ) throws GM_Exception
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final GM_Curve Curve = (GM_Curve) JTSAdapter.wrap( line );

    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );

    defaultstroke = symb.getStroke();

    float[] dArray = new float[3];

    dArray[0] = 6;
    dArray[1] = 3;

    stroke.setWidth( 2 );
    stroke.setDashArray( dArray );
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
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    // TODO: Set the Stroke back to default
    symb.setStroke( defaultstroke );

  }

  public boolean complete( )
  {
    boolean check;
    check = false;

    if( m_profDownInters != null & m_profUpInters != null & m_bankLeftInters != null & m_bankRightInters != null )
    {
      check = true;
    }
    return check;
  }

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

  private double calcDistance( final IProfilPoint beginPoint, final IProfilPoint endPoint, final IProfilPoint middlePoint ) throws ProfilDataException
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

  /** @return the points with are redundant */
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

}