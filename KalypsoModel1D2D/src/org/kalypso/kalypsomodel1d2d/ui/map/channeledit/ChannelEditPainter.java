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
import java.util.HashSet;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ChannelMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IBankData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMeshPainter;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * Paints the current state of the create main channel on the map.
 *
 * @author Gernot Belger
 */
class ChannelEditPainter
{
  private final SLDPainter2 m_selectedProfilePainter = new SLDPainter2( getClass().getResource( "resources/selectedProfile.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_leftBankPainter = new SLDPainter2( getClass().getResource( "resources/bankLeft.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_rightBankPainter = new SLDPainter2( getClass().getResource( "resources/bankRight.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_editProfileLinePainter = new SLDPainter2( getClass().getResource( "resources/editProfileLine.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_editProfilePointPainter = new SLDPainter2( getClass().getResource( "resources/editProfilePoint.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_meshEdgePainter = new SLDPainter2( getClass().getResource( "resources/meshEdge.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_meshVertexPainter = new SLDPainter2( getClass().getResource( "resources/meshVertex.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_bankLinePainter = new SLDPainter2( getClass().getResource( "resources/bankLine.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_bankLineDownPainter = new SLDPainter2( getClass().getResource( "resources/bankLineDown.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_bankLineUpPainter = new SLDPainter2( getClass().getResource( "resources/bankLineUp.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_bankPointPainter = new SLDPainter2( getClass().getResource( "resources/bankPoint.sld" ) ); //$NON-NLS-1$

  private final ChannelEditData m_data;

  private final String m_srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  public ChannelEditPainter( final ChannelEditData data )
  {
    m_data = data;
  }

  public void paint( final Graphics g, final GeoTransform projection ) throws GM_Exception, GeoTransformerException
  {
    paintSelectedProfiles( g, projection );

    final ISegmentData[] segments = m_data.getSegments();

    paintBanks( g, projection, ChannelEditData.SIDE.RIGHT, m_leftBankPainter );
    paintBanks( g, projection, ChannelEditData.SIDE.LEFT, m_rightBankPainter );

    /* draw intersected profile */
    paintActiveIntersectionProfile( g, projection );

    /* draw intersection points */
    // drawIntersectionPoints( g, projection );

    /* draw mesh */
    paintMeshes( g, projection, segments );

    /* draw editable bankline */
    drawBankLines( g, projection, segments );
  }

  private void paintMeshes( final Graphics g, final GeoTransform projection, final ISegmentData[] segments )
  {
    for( final ISegmentData segment : segments )
    {
      final QuadMesh mesh = segment.getMesh();
      final QuadMeshPainter painter = new QuadMeshPainter( mesh, m_meshEdgePainter, m_meshVertexPainter );
      painter.paint( g, projection );
    }
  }

  private void paintSelectedProfiles( final Graphics g, final GeoTransform projection )
  {
    final IProfileData[] selectedProfiles = m_data.getSelectedProfiles();

    for( final IProfileData profile : selectedProfiles )
    {
      try
      {
        final GM_Curve line = profile.getOriginalProfileGeometry();
        m_selectedProfilePainter.paint( g, projection, line );
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  private void paintActiveIntersectionProfile( final Graphics g, final GeoTransform projection ) throws GM_Exception, GeoTransformerException
  {
    final IProfileData activeProfile = m_data.getActiveProfile();
    if( activeProfile == null )
      return;

    final IProfile intersectionProfile = activeProfile.getWorkingProfile();
    if( intersectionProfile == null )
      return;

    final GM_Curve line = (GM_Curve)ProfileUtil.getLine( intersectionProfile ).transform( m_srsName );

    m_editProfileLinePainter.paint( g, projection, line );

    paintPoints( g, projection, line, m_editProfilePointPainter );
  }

  private void paintBanks( final Graphics g, final GeoTransform projection, final ChannelEditData.SIDE side, final SLDPainter2 painter )
  {
    final GM_Curve curve = m_data.getBanklineForSide( side );
    painter.paint( g, projection, curve );
  }

  private void drawBankLines( final Graphics g, final GeoTransform projection, final ISegmentData[] segments ) throws GM_Exception
  {
    final ChannelMesh editData = m_data.getEditData();
    if( editData == null )
      return;

    final Set<IBankData> banks = new HashSet<>();

    for( final ISegmentData segment : segments )
    {
      banks.add( segment.getBankLeft() );
      banks.add( segment.getBankRight() );
    }

    for( final IBankData bank : banks )
      paintBank( g, projection, bank, m_bankLinePainter );

    /* paint banks of current segment */
    final IProfileData activeProfile = m_data.getActiveProfile();
    if( activeProfile == null )
      return;

    final ISegmentData downSegment = activeProfile.getDownSegment();
    if( downSegment != null )
    {
      paintBank( g, projection, downSegment.getBankLeft(), m_bankLineDownPainter );
      paintBank( g, projection, downSegment.getBankRight(), m_bankLineDownPainter );
    }

    final ISegmentData upSegment = activeProfile.getUpSegment();
    if( upSegment != null )
    {
      paintBank( g, projection, upSegment.getBankLeft(), m_bankLineUpPainter );
      paintBank( g, projection, upSegment.getBankRight(), m_bankLineUpPainter );
    }
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

  private void paintBank( final Graphics g, final GeoTransform projection, final IBankData bank, final SLDPainter2 linePainter ) throws GM_Exception
  {
    if( bank == null )
      return;

    // paint the line
    final GM_Curve bankCurve = (GM_Curve)JTSAdapter.wrap( bank.getWorkingGeometry(), m_srsName );

    linePainter.paint( g, projection, bankCurve );

    // paint the nodes
    paintPoints( g, projection, bankCurve, m_bankPointPainter );
  }
}