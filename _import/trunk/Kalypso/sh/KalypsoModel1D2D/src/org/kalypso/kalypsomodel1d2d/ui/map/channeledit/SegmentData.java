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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author jung
 */
public class SegmentData
{
  // LineStrings derived friom the original data
  private LineString m_profUpOrg;

  private LineString m_profDownOrg;

  private LineString m_bankLeftOrg;

  private LineString m_bankRightOrg;

  // intersected lines
  private LineString m_profUpInters;

  private LineString m_profDownInters;

  private LineString m_bankLeftInters;

  private LineString m_bankRightInters;

  // original data
  private Map<Feature, CreateChannelData.SIDE> m_bankLines;

  private WspmProfile m_previousProfile;

  private WspmProfile m_nextProfile;

  private LineString m_previousProfLineString;

  private LineString m_nextProfLineString;

  public SegmentData( final WspmProfile previousProfile, final WspmProfile nextProfile, final Map<Feature, CreateChannelData.SIDE> bankLines )
  {
    m_previousProfile = previousProfile;
    m_nextProfile = nextProfile;
    m_bankLines = bankLines;

    m_previousProfLineString = convertProfilesToLineStrings( previousProfile );
    m_nextProfLineString = convertProfilesToLineStrings( nextProfile );

    intersectBanks();

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

  private void intersectBanks( )
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
          // final GeometryCollection gc = (GeometryCollection) intersectionPreviousProfile;
          // intersectionpoints.put( (Point) gc.getGeometryN( 0 ), side );
          intersPointList.add( (Point) intersectionPreviousProfile );
        }
        else if( intersectionPreviousProfile instanceof GeometryCollection )
        {
          /*
           * if there are more than one intersection point for one profile, the bank line does not match correctly for
           * this section
           */
          // TODO: maybe its good to get just the first intersection point, so that there will be always a point.
          // final GeometryCollection gc = (GeometryCollection) intersectionPreviousProfile;
          // if( gc.getGeometryN( 0 ) instanceof Point )
          // intersectionpoints.put( (Point)gc.getGeometryN( 0 ), side );
        }

        /* intersect it with the second (next) profile of this segment */
        final Geometry intersectionNextProfile = bankLine.intersection( m_nextProfLineString );
        if( intersectionNextProfile instanceof Point )
        {
          // final GeometryCollection gc = (GeometryCollection) intersectionPreviousProfile;
          intersPointList.add( (Point) intersectionNextProfile );
        }
        else if( intersectionNextProfile instanceof GeometryCollection )
        {
          /* there should be only one intersection point for each proflie */
          // TODO: maybe its good to get just the first two intersection points, so that there will be always two
          // points.
          // final GeometryCollection gc = (GeometryCollection) intersectionPreviousProfile;
          // if( gc.getGeometryN( 0 ) instanceof Point )
          // intersectionpoints.put( (Point)gc.getGeometryN( 0 ), side );
        }
        /*
         * if there are more or less than two intersection point for the two profiles, the bank lines does not match
         * correctly for this section
         */
        if( intersPointList.size() == 2 )
        {
          /* extract the LineStringSegment of the bankline lying between the two corresponding intersection points */
          //final CreateChannelData.SIDE bankSide = bankEntry.getValue();

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
      paintSegmentLine( m_bankRightOrg, g, mapPanel, new Color( 255, 0, 0 ) );
    if( m_bankLeftOrg != null )
      paintSegmentLine( m_bankLeftOrg, g, mapPanel, new Color( 0, 255, 0 ) );

  }

  private void paintSegmentLine( final LineString line, final Graphics g, final MapPanel mapPanel, final Color color ) throws GM_Exception
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final GM_Curve Curve = (GM_Curve) JTSAdapter.wrap( line );

    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );

    defaultstroke = symb.getStroke();

    stroke.setWidth( 8 );
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

    // LineString bankLeftOrg = getBankLeftOrg();
    //
    // LinkedList<java.awt.Point> list = new LinkedList<java.awt.Point>();
    // for( int i = 0; i < bankLeftOrg.getNumPoints(); i++ )
    // {
    // Point pointN = bankLeftOrg.getPointN( i );
    // GM_Point point = (GM_Point) JTSAdapter.wrap( pointN );
    // java.awt.Point drawPoint = MapUtilities.retransform( mapPanel, point );
    // list.add( drawPoint );
    // }
    //
    // for( int i = 0; i < list.size() - 1; i++ )
    // {
    // java.awt.Point point1 = list.get( i );
    // java.awt.Point point2 = list.get( i + 1 );
    //
    // g.drawLine( point1.x, point1.y, point2.x, point2.y );
  }

  private void paintRightBankLine( final Graphics g, final MapPanel mapPanel, final Color color ) throws GM_Exception
  {
    // LineString bankLeftOrg = getBankLeftOrg();
    //
    // LinkedList<java.awt.Point> list = new LinkedList<java.awt.Point>();
    // for( int i = 0; i < bankLeftOrg.getNumPoints(); i++ )
    // {
    // Point pointN = bankLeftOrg.getPointN( i );
    // GM_Point point = (GM_Point) JTSAdapter.wrap( pointN );
    // java.awt.Point drawPoint = MapUtilities.retransform( mapPanel, point );
    // list.add( drawPoint );
    // }
    //
    // for( int i = 0; i < list.size() - 1; i++ )
    // {
    // java.awt.Point point1 = list.get( i );
    // java.awt.Point point2 = list.get( i + 1 );
    //
    // g.drawLine( point1.x, point1.y, point2.x, point2.y );
    // }
  }

  public boolean complete( )
  {
    final boolean check;
    check = false;

    return check;
  }

}