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
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesher;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;

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

  // /**
  // * manages the update of the profile data, after the intersected profiles were changed by the chart view layer in
  // the
  // * gui. things to do: -update the intersection points -> will be done by the layer -update the intersected banklines
  // * -update the profiles (-> croping, intersecting, elevation adjusting)
  // */
  // public void updateProfileIntersection( )
  // {
  // if( complete() == true )
  // {
  // /* get the cropped and intersected profiles & linestrings */
  //
  // // muss jedes mal nach profile edit aufgerufen werden!
  // // UPSTREAM
  // try
  // {
  // /* crop the profile */
  // m_upCroppedProfile = createCroppedIProfile( m_upProfile, CreateChannelData.PROF.UP );
  //
  // /* the cropped profile area is the desired value for the intersected profile area */
  // final double areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );
  //
  // /* intersect the cropped profile */
  // // here not necessary, because the initial intersection was already done. The intersection here will be
  // // handled by the user.
  // // final IProfil tempPreviousIntersProfile = createIntersectedIProfile( m_previousCroppedProfile );
  // // LineString
  // // m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );
  // final IProfil tmpupIntersProfile = adaptProfileElevations( m_upIntersProfile, m_upCroppedProfile );
  // final double areaUpIntersProfile = ProfilUtil.calcArea( tmpupIntersProfile );
  //
  // // Fl�chenausgleich!!
  // m_upIntersProfile = adjustProfileArea( m_upIntersProfile, areaUpCroppedProfile, areaUpIntersProfile );
  //
  // // m_upIntersProfile = createIntersectedIProfile( m_upCroppedProfile );
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_upIntersLinestring = factory.createLineString( ProfilUtil.getLineCoordinates( m_upIntersProfile ) );
  // }
  // catch( final Exception e )
  // {
  // e.printStackTrace();
  // }
  //
  // // DOWNSTREAM
  // try
  // {
  // /* crop the profile */
  // // IProfil
  // m_downCroppedProfile = createCroppedIProfile( m_DownProfile, CreateChannelData.PROF.DOWN );
  //
  // /* the cropped profile area is the desired value for the intersected profile area */
  // final double areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );
  //
  // /* intersect the cropped profile */
  // // here not necessary, because the initial intersection was already done. The intersection here will be
  // // handeled by the user.
  // // LineString
  // // final IProfil tempNextIntersProfile = createIntersectedIProfile( m_nextCroppedProfile );
  // final IProfil tmpdownIntersProfile = adaptProfileElevations( m_downIntersProfile, m_downCroppedProfile );
  // final double areaDownIntersProfile = ProfilUtil.calcArea( tmpdownIntersProfile );
  //
  // // Fl�chenausgleich!!
  // m_downIntersProfile = adjustProfileArea( m_downIntersProfile, areaDownCroppedProfile, areaDownIntersProfile );
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_downIntersLinestring = factory.createLineString( ProfilUtil.getLineCoordinates( m_downIntersProfile ) );
  //
  // }
  // catch( final Exception e )
  // {
  // e.printStackTrace();
  // }
  // }
  // }

  /**
   * gets the map extend (GM_Envelope) of the current segment
   */
  public GM_Envelope getSegmentMapExtend( final String srsName )
  {
    final Envelope[] boxes = new Envelope[4];

    if( m_upProfileData != null )
    {
      final LineString segmentedProfileUp = m_upProfileData.getSegmentedGeometry();
      if( segmentedProfileUp != null )
        boxes[0] = segmentedProfileUp.getEnvelopeInternal();
    }

    if( m_downProfileData != null )
    {
      final LineString segmentedProfileDown = m_downProfileData.getSegmentedGeometry();
      if( segmentedProfileDown != null )
        boxes[1] = segmentedProfileDown.getEnvelopeInternal();
    }

    if( m_leftBank != null )
      boxes[2] = m_leftBank.getSegmented().getEnvelopeInternal();

    if( m_rightBank != null )
      boxes[3] = m_rightBank.getSegmented().getEnvelopeInternal();

    final Envelope fullExtend = new Envelope();
    for( final Envelope box : boxes )
      fullExtend.expandToInclude( box );

    return JTSAdapter.wrap( fullExtend, srsName );
  }

  // /**
  // * updates the intersected bankline linestring with the new edge point (moved by profile chart) by moving the
  // * first/last line point (oldPoint) to the new location (newPoint).
  // */
  // private void updateBanklines( final Point oldPoint, final Point newPoint )
  // {
  // // TODO: we have a problem with projected coords. check what has to be transformed (the new coord?). and do it!!
  //
  // // find the correct bankline
  // Point point = m_bankLeftSegmented.getPointN( 0 );
  //
  // if( point.distance( oldPoint ) < 0.01 )
  // {
  // Coordinate[] coords = new Coordinate[m_numBankSegments];
  // coords = m_bankLeftSegmented.getCoordinates();
  // coords[0] = newPoint.getCoordinate();
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_bankLeftOrg = factory.createLineString( coords );
  // }
  //
  // point = m_bankLeftSegmented.getPointN( m_numBankSegments - 1 );
  // if( point.distance( oldPoint ) < 0.01 )
  // {
  // Coordinate[] coords = new Coordinate[m_numBankSegments];
  // coords = m_bankLeftSegmented.getCoordinates();
  // coords[m_numBankSegments - 1] = newPoint.getCoordinate();
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_bankLeftOrg = factory.createLineString( coords );
  // }
  //
  // point = m_bankRightSegmented.getPointN( 0 );
  // if( point.distance( oldPoint ) < 0.01 )
  // {
  // Coordinate[] coords = new Coordinate[m_numBankSegments];
  // coords = m_bankRightSegmented.getCoordinates();
  // coords[0] = newPoint.getCoordinate();
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_bankRightOrg = factory.createLineString( coords );
  //
  // }
  //
  // point = m_bankRightSegmented.getPointN( m_numBankSegments - 1 );
  // if( point.distance( oldPoint ) < 0.01 )
  // {
  // Coordinate[] coords = new Coordinate[m_numBankSegments];
  // coords = m_bankRightSegmented.getCoordinates();
  // coords[m_numBankSegments - 1].setCoordinate( newPoint.getCoordinate() );
  //
  // final GeometryFactory factory = new GeometryFactory();
  // m_bankRightOrg = factory.createLineString( coords );
  // }
  // }

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

    final LineString topLine = upProfile.getSegmentedGeometry();
    final LineString leftLine = bankLeft == null ? null : bankLeft.getSegmented();
    final LineString bottomLine = downProfile.getSegmentedGeometry();
    final LineString rightLine = bankRight == null ? null : bankRight.getSegmented();

    if( Objects.isNull( leftLine, topLine, rightLine, bottomLine ) )
      return null;

    if( topLine.getNumPoints() != bottomLine.getNumPoints() )
      return null;

    if( leftLine.getNumPoints() != rightLine.getNumPoints() )
      return null;

    final QuadMesher mesher = new QuadMesher();
    // TODO: validate mesh and also keep mesh status -> paint mesh accordingly
    mesher.createMesh( srsName, leftLine, topLine, rightLine, bottomLine );

    return mesher.getMesh();
  }

  public void updateMesh( )
  {
    m_mesh = buildMesh();
  }

  @Override
  public int getNumberBankSegments( )
  {
    return m_numBankSegments;
  }

  @Override
  public boolean isBanksUserChanged( )
  {
    return m_leftBank.isUserChanged() || m_rightBank.isUserChanged();
  }

  @Override
  public void updateNumberOfSegments( final int segments )
  {
    m_numBankSegments = segments;

    m_leftBank = updateNumberOfSegments( m_leftBank, segments );
    m_rightBank = updateNumberOfSegments( m_rightBank, segments );

    updateMesh();
  }

  // FIXME: instead use current segmented line and change number of segments, so user edits do not get lost
  private BankData updateNumberOfSegments( final BankData bank, final int segments )
  {
    final LineString originalGeometry = bank.getOriginalGeometry();
    final LineString croppedGeometry = bank.getCroppedOriginalGeometry();

    final LineString oldSegmentedGeometry = bank.getSegmented();

    final LineString segmentedGeometry = ChannelEditUtil.intersectLineString( oldSegmentedGeometry, segments );

    return new BankData( this, originalGeometry, croppedGeometry, segmentedGeometry, bank.isUserChanged() );
  }

  @Override
  public void updateSegmentedGeometry( final IBankData bank, final LineString newSegmentedLine )
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
}