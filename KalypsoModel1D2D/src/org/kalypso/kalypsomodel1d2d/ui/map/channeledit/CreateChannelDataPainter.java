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

import java.awt.Graphics;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Point;

/**
 * Paints the current state of the create main channel on the map.
 *
 * @author Gernot Belger
 */
class CreateChannelDataPainter
{
  private final SLDPainter2 m_selectedProfilePainter = new SLDPainter2( getClass().getResource( "resources/selectedProfile.sld" ) );

  private final SLDPainter2 m_leftBankPainter = new SLDPainter2( getClass().getResource( "resources/bankLeft.sld" ) );

  private final SLDPainter2 m_rightBankPainter = new SLDPainter2( getClass().getResource( "resources/bankRight.sld" ) );

  private final SLDPainter2 m_editProfileLinePainter = new SLDPainter2( getClass().getResource( "resources/editProfileLine.sld" ) );

  private final SLDPainter2 m_editProfilePointPainter = new SLDPainter2( getClass().getResource( "resources/editProfilePoint.sld" ) );

  private final SLDPainter2 m_intersectionPointPainter = new SLDPainter2( getClass().getResource( "resources/intersectionPoint.sld" ) );

  private final SLDPainter2 m_edgePainter = new SLDPainter2( getClass().getResource( "resources/edge.sld" ) );

  private final SLDPainter2 m_bankLinePainter = new SLDPainter2( getClass().getResource( "resources/bankLine.sld" ) );

  private final SLDPainter2 m_bankPointPainter = new SLDPainter2( getClass().getResource( "resources/bankPoint.sld" ) );

  private final CreateChannelData m_data;

  private final String m_srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();



  public CreateChannelDataPainter( final CreateChannelData data )
  {
    m_data = data;
  }

  public void paint( final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    paintSelectedProfile( g, projection );

    paintBanks( g, projection, CreateChannelData.SIDE.RIGHT, m_leftBankPainter );
    paintBanks( g, projection, CreateChannelData.SIDE.LEFT, m_rightBankPainter );

    /* draw intersected profile */
    drawIntersProfiles( g, projection );

    /* draw intersection points */
    drawIntersPoints( g, projection );

    /* draw mesh */
    final Coordinate[][] meshCoords = m_data.getMeshCoords();
    if( meshCoords != null )
      paintEdges( meshCoords, g, projection );

    /* draw editable bankline */
    if( m_data.isBankEdit() && m_data.getMeshStatus() == true )
      drawBankLines( g, projection );
  }

  private void paintSelectedProfile( final Graphics g, final GeoTransform projection )
  {
    final IProfileFeature[] selectedProfiles = m_data.getSelectedProfiles();

    for( final IProfileFeature profile : selectedProfiles )
    {
      final GM_Curve line = profile.getLine();
      m_selectedProfilePainter.paint( g, projection, line );
    }
  }

  private void drawIntersProfiles( final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    if( m_data.getSelectedSegment() != null )
    {
      final SegmentData currentSegment = m_data.getSelectedSegment();
      final PROF prof = m_data.getCurrentProfile();

      if( currentSegment != null && currentSegment.complete() == true && prof != null )
        paintProfile( g, projection, currentSegment, prof );
    }
  }

  private void drawIntersPoints( final Graphics g, final GeoTransform projection )
  {
    if( m_data.getSelectedSegment() != null )
    {
      final SegmentData currentSegment = m_data.getSelectedSegment();
      if( currentSegment != null && currentSegment.complete() == true )
        paintIntersectionPoints( g, projection, currentSegment, m_data.getCurrentProfile() );
    }
  }

  private void paintBanks( final Graphics g, final GeoTransform projection, final CreateChannelData.SIDE side, final SLDPainter2 painter )
  {
    final GM_Curve curve = m_data.getBanklineForSide( side );
    painter.paint( g, projection, curve );
  }

  private void paintEdges( final Coordinate[][] coords, final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    for( final Coordinate[] element : coords )
    {
      for( int j = 0; j < element.length - 1; j++ )
      {
        final GM_Position p0 = GeometryFactory.createGM_Position( element[j].x, element[j].y );
        final GM_Position p1 = GeometryFactory.createGM_Position( element[j + 1].x, element[j + 1].y );

        final GM_Curve edge = GeometryFactory.createGM_Curve( new GM_Position[] { p0, p1 }, m_srsName );

        m_edgePainter.paint( g, projection, edge );
      }
    }

    for( int j = 0; j < coords[0].length; j++ )
    {
      for( int i = 0; i < coords.length - 1; i++ )
      {
        final Coordinate c0 = coords[i][j];
        final Coordinate c1 = coords[i + 1][j];

        final GM_Position p0 = GeometryFactory.createGM_Position( c0.x, c0.y );
        final GM_Position p1 = GeometryFactory.createGM_Position( c1.x, c1.y );

        final GM_Curve edge = GeometryFactory.createGM_Curve( new GM_Position[] { p0, p1 }, m_srsName );

        m_edgePainter.paint( g, projection, edge );
      }
    }
  }

  private void drawBankLines( final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    final SegmentData[] segments = m_data.getSegments();
    for( final SegmentData segment : segments )
    {
      if( segment != null )
        paintBank( g, projection, segment );
    }
  }

  private void paintProfile( final Graphics g, final GeoTransform projection, final SegmentData currentSegment, final PROF prof ) throws GM_Exception
  {
    final IProfil currentProfile = currentSegment.getCurrentProfile( prof );
    if( currentProfile == null )
      return;

    final GM_Curve line = ProfilUtil.getLine( currentProfile, m_srsName );

    m_editProfileLinePainter.paint( g, projection, line );

    paintPoints( g, projection, line, m_editProfilePointPainter );
  }

  /**
   * Paints the vertices of a line as single points.
   */
  private void paintPoints( final Graphics g, final GeoTransform projection, final GM_Curve line, final SLDPainter2 pointPainter ) throws GM_Exception
  {
    final GM_Position[] positions = line.getAsLineString().getPositions();
    for( final GM_Position position : positions )
    {
      final GM_Point point = GeometryFactory.createGM_Point( position, line.getCoordinateSystem() );
      pointPainter.paint( g, projection, point );
    }
  }

  private void paintIntersectionPoints( final Graphics g, final GeoTransform projection, final SegmentData segment, final PROF prof )
  {
    final IntersPointData[] points = segment.getIntersectionPoints();
    for( final IntersPointData intersectionPoint : points )
    {
      if( intersectionPoint.getProf() == prof )
      {
        final Point point = intersectionPoint.getPoint();
        final GM_Point gmPoint = GeometryFactory.createGM_Point( point.getX(), point.getY(), m_srsName );
        m_intersectionPointPainter.paint( g, projection, gmPoint );
      }
    }
  }

  /**
   * Paints the editable bank
   */
  private void paintBank( final Graphics g, final GeoTransform projection, final SegmentData segment ) throws GM_Exception
  {
    // paint the line
    final GM_Curve bankLeft = (GM_Curve) JTSAdapter.wrap( segment.getBankLeftInters(), m_srsName );
    m_bankLinePainter.paint( g, projection, bankLeft );

    final GM_Curve bankRight = (GM_Curve) JTSAdapter.wrap( segment.getBankRightInters(), m_srsName );
    m_bankLinePainter.paint( g, projection, bankRight );

    // paint the nodes
    paintPoints( g, projection, bankLeft, m_bankPointPainter );
    paintPoints( g, projection, bankRight, m_bankPointPainter );
  }
}