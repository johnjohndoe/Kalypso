/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.jts.QuadMesher.JTSCoordsElevInterpol;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesher;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Represants a segment of the river between two profiles.<br/>
 * The segment is directed going from 'down' to 'up'.<br/>
 * 
 * @author Thomas Jung
 */
class SegmentData implements ISegmentData
{
  private BankData m_leftBank;

  private BankData m_rightBank;

  private int m_numBankSegments;

  private final ProfileData m_upProfileData;

  private final ProfileData m_downProfileData;

  private QuadMesh m_mesh;

  public SegmentData( final ProfileData upProfile, final ProfileData downProfile, final Map<GM_Curve, SIDE> banks, final int numberOfBankIntersections )
  {
    m_upProfileData = upProfile;
    m_downProfileData = downProfile;

    upProfile.setDownSegment( this );
    downProfile.setUpSegment( this );

    m_numBankSegments = numberOfBankIntersections;

    /* Initialize bankline intersection data */
    final BanklineIntersector leftIntersector = new BanklineIntersector( this, banks, SIDE.LEFT, numberOfBankIntersections );
    m_leftBank = leftIntersector.getBankline();

    final BanklineIntersector rightIntersector = new BanklineIntersector( this, banks, SIDE.RIGHT, numberOfBankIntersections );
    m_rightBank = rightIntersector.getBankline();
  }

  public int getNumBankIntersections( )
  {
    return m_numBankSegments;
  }

  @Override
  public QuadMesh getMesh( )
  {
    return m_mesh;
  }

  /**
   * gets the map extend (GM_Envelope) of the current segment
   */
  public GM_Envelope getSegmentMapExtent( final String srsName )
  {
    final Envelope[] boxes = new Envelope[4];

    if( m_upProfileData != null )
    {
      final LineString segmentedProfileUp = m_upProfileData.getWorkingGeometry();
      if( segmentedProfileUp != null )
        boxes[0] = segmentedProfileUp.getEnvelopeInternal();
    }

    if( m_downProfileData != null )
    {
      final LineString segmentedProfileDown = m_downProfileData.getWorkingGeometry();
      if( segmentedProfileDown != null )
        boxes[1] = segmentedProfileDown.getEnvelopeInternal();
    }

    if( m_leftBank != null )
      boxes[2] = m_leftBank.getWorkingGeometry().getEnvelopeInternal();

    if( m_rightBank != null )
      boxes[3] = m_rightBank.getWorkingGeometry().getEnvelopeInternal();

    final Envelope fullExtend = new Envelope();
    for( final Envelope box : boxes )
    {
      if( box != null )
        fullExtend.expandToInclude( box );
    }

    return JTSAdapter.wrap( fullExtend, srsName );
  }

  @Override
  public ProfileData getProfileDown( )
  {
    return m_downProfileData;
  }

  @Override
  public ProfileData getProfileUp( )
  {
    return m_upProfileData;
  }

  @Override
  public BankData getBankLeft( )
  {
    return m_leftBank;
  }

  @Override
  public BankData getBankRight( )
  {
    return m_rightBank;
  }

  @Override
  public boolean hasBanks( )
  {
    return m_leftBank != null && m_rightBank != null;
  }

  public BankData getBank( final SIDE side )
  {
    switch( side )
    {
      case LEFT:
        return m_leftBank;
      case RIGHT:
        return m_rightBank;

      default:
        throw new IllegalArgumentException();
    }
  }

  private QuadMesh buildMesh( )
  {
    final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final ProfileData downProfile = getProfileDown();
    final ProfileData upProfile = getProfileUp();

    final BankData bankLeft = getBankLeft();
    final BankData bankRight = getBankRight();

    final LineString topLine = upProfile.getWorkingGeometry();
    final LineString leftLine = bankLeft == null ? null : bankLeft.getWorkingGeometry();
    final LineString bottomLine = downProfile.getWorkingGeometry();
    final LineString rightLine = bankRight == null ? null : bankRight.getWorkingGeometry();

    if( Objects.isNull( leftLine, topLine, rightLine, bottomLine ) )
      return null;

    if( topLine.getNumPoints() != bottomLine.getNumPoints() )
      return null;

    if( leftLine.getNumPoints() != rightLine.getNumPoints() )
      return null;

    final QuadMesher mesher = new QuadMesher();
    // TODO: validate mesh and also keep mesh status -> paint mesh accordingly

    // REMARK: order of lines is important, mesher will interpolate from left to right (here the bank lines)
    // The result is what we want, i.e. the interpolated profile lines are straight, the interpolated bank lines
    // have the same curvature as the input bank lines.
    mesher.createMesh( srsName, leftLine, topLine, rightLine, bottomLine );

    // REMARK2: however, the interpolation order yields the wrong z-values, the z values should be interpolated
    // between the top/bottom lines (i.e profiles) instead of left right.
    // So we need to do it again ourselfs.

    final QuadMesh mesh = mesher.getMesh();

    final Coordinate[][] grid = mesh.getGrid();

    final JTSCoordsElevInterpol adjuster = new JTSCoordsElevInterpol( grid );

    for( int j = 1; j < grid.length - 1; j++ )
      adjuster.calculateElevationsForColumn( j );

    // REMARK: actually the z value are changed inline, so we could use the old mesh...
    final Coordinate[][] reinterpolatedGrid = adjuster.calculateElevations();

    return new QuadMesh( reinterpolatedGrid, srsName );
  }

  public void updateMesh( )
  {
    m_mesh = buildMesh();
  }

  @Override
  public int getNumberBankPoints( )
  {
    return m_numBankSegments;
  }

  @Override
  public boolean isBanksUserChanged( )
  {
    if( m_leftBank != null && m_leftBank.isUserChanged() )
      return true;

    if( m_rightBank != null && m_rightBank.isUserChanged() )
      return true;

    return false;
  }

  @Override
  public void updateNumberOfBankPoints( final int numberPoints )
  {
    m_numBankSegments = numberPoints;

    m_leftBank = updateNumberOfPoints( m_leftBank, numberPoints );
    m_rightBank = updateNumberOfPoints( m_rightBank, numberPoints );

    updateMesh();
  }

  private BankData updateNumberOfPoints( final BankData bank, final int numberOfBankPoints )
  {
    final LineString originalGeometry = bank.getOriginalGeometry();
    final LineString croppedGeometry = bank.getCroppedOriginalGeometry();

    final LineString oldWorkingGeometry = bank.getWorkingGeometry();

    final LineString workingGeometry = ChannelEditUtil.intersectLineString( oldWorkingGeometry, numberOfBankPoints );

    return new BankData( this, originalGeometry, croppedGeometry, workingGeometry, bank.isUserChanged() );
  }

  @Override
  public void updateWorkingGeometry( final IBankData bank, final LineString newSegmentedLine )
  {
    Assert.isTrue( bank == m_leftBank || bank == m_rightBank );

    if( bank == m_leftBank )
      m_leftBank = updateBankSegmentedGeometry( m_leftBank, newSegmentedLine );
    else if( bank == m_rightBank )
      m_rightBank = updateBankSegmentedGeometry( m_rightBank, newSegmentedLine );

    updateMesh();
  }

  private BankData updateBankSegmentedGeometry( final BankData bank, final LineString newSegmentedLine )
  {
    final LineString originalGeometry = bank.getOriginalGeometry();
    final LineString croppedGeometry = bank.getCroppedOriginalGeometry();

    return new BankData( this, originalGeometry, croppedGeometry, newSegmentedLine, true );
  }

  void updateBankEndpoints( final Coordinate oldEndpointLocation, final Coordinate newEndpointLocation )
  {
    if( oldEndpointLocation != null )
    {
      m_leftBank = updateBankEndpoint( m_leftBank, oldEndpointLocation, newEndpointLocation );
      m_rightBank = updateBankEndpoint( m_rightBank, oldEndpointLocation, newEndpointLocation );
    }
  }

  private BankData updateBankEndpoint( final BankData bank, final Coordinate oldEndpointLocation, final Coordinate newEndpointLocation )
  {
    final LineString originalGeometry = bank.getOriginalGeometry();
    final LineString croppedGeometry = bank.getCroppedOriginalGeometry();

    final LineString segmentedLine = bank.getWorkingGeometry();
    final LineString newSegmentedLine = updateBankEndpoint( segmentedLine, oldEndpointLocation, newEndpointLocation );

    return new BankData( this, originalGeometry, croppedGeometry, newSegmentedLine, bank.isUserChanged() );
  }

  private LineString updateBankEndpoint( final LineString segmentedLine, final Coordinate oldEndpointLocation, final Coordinate newEndpointLocation )
  {
    /* either it is the start of the line... */
    final Point startPoint = segmentedLine.getStartPoint();
    if( startPoint.getCoordinate().distance( oldEndpointLocation ) < 0.01 )
    {
      final Coordinate[] coordinates = segmentedLine.getCoordinates();
      coordinates[0] = newEndpointLocation;
      return segmentedLine.getFactory().createLineString( coordinates );
    }

    /* ... or the end ... */
    final Point endPoint = segmentedLine.getEndPoint();
    if( endPoint.getCoordinate().distance( oldEndpointLocation ) < 0.01 )
    {
      final Coordinate[] coordinates = segmentedLine.getCoordinates();
      coordinates[coordinates.length - 1] = newEndpointLocation;
      return segmentedLine.getFactory().createLineString( coordinates );
    }

    /* ... or neither start nor end -> other bank was changed, so return old value ... */
    return segmentedLine;
  }
}