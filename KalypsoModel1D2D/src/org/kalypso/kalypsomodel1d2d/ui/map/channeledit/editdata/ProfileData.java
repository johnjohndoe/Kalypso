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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import org.deegree.model.spatialschema.GeometryException;
import org.eclipse.core.runtime.Assert;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Wraps a profile and keeps track of the segmented profile.
 *
 * @author Gernot Belger
 * @author Thomas Jung
 */
class ProfileData implements IProfileData
{
  private final IProfileFeature m_feature;

  private IProfil m_segmentedProfile;

  private SegmentData m_upSegment;

  private SegmentData m_downSegment;

  private final int m_numIntersections;

  private boolean m_isUserChanged = false;

  public ProfileData( final IProfileFeature feature, final int numIntersections )
  {
    m_feature = feature;
    m_numIntersections = numIntersections;
  }

  @Override
  public boolean isUserChaned( )
  {
    return m_isUserChanged;
  }

  @Override
  public IProfileFeature getFeature( )
  {
    return m_feature;
  }

  @Override
  public IProfil getProfilOrg( )
  {
    return m_feature.getProfil();
  }

  @Override
  public IProfil getProfIntersProfile( )
  {
    return m_segmentedProfile;
  }

  /* Wraps profile features into this data class. */
  public static ProfileData[] from( final IProfileFeature[] features, final int numIntersections )
  {
    final ProfileData[] profiles = new ProfileData[features.length];

    for( int i = 0; i < profiles.length; i++ )
      profiles[i] = new ProfileData( features[i], numIntersections );

    return profiles;
  }

  public void setDownSegment( final SegmentData downSegment )
  {
    // REMARK: make sure, segments is only ever set once
    Assert.isNotNull( downSegment );
    if( downSegment == m_downSegment )
      return;
    Assert.isTrue( m_downSegment == null );

    m_downSegment = downSegment;
  }

  public void setUpSegment( final SegmentData upSegment )
  {
    // REMARK: make sure, segments is only ever set once
    Assert.isNotNull( upSegment );
    if( upSegment == m_upSegment )
      return;
    Assert.isTrue( m_upSegment == null );

    m_upSegment = upSegment;
  }

  @Override
  public SegmentData getUpSegment( )
  {
    return m_upSegment;
  }

  @Override
  public SegmentData getDownSegment( )
  {
    return m_downSegment;
  }

  public LineString getSegmentedGeometry( )
  {
    try
    {
      if( m_segmentedProfile == null )
        return null;

      final GM_Curve line = (GM_Curve)ProfilUtil.getLine( m_segmentedProfile ).transform( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      return (LineString)JTSAdapter.export( line );
    }
    catch( final GM_Exception | GeoTransformerException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public void recalculateSegmentedProfile( )
  {
    try
    {
      m_segmentedProfile = null;

      /* crop the profile */
      final SegmentData segment = findSegmentWithBanks();
      if( segment == null )
        return;

      final IProfil croppedProfile = createCroppedIProfile( segment );

      /* intersect the cropped profile */
      final ProfileSegmenter segmenter = new ProfileSegmenter( m_numIntersections );
      final IProfil segmentedProfile = segmenter.execute( croppedProfile );

      /* Area adjustment */
      final ProfileAreaAdjuster adjuster = new ProfileAreaAdjuster( croppedProfile );
      m_segmentedProfile = adjuster.execute( segmentedProfile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Calculates the intersection of a segment profile. at first, the width coordinates will be calculated and added to
   * the profile. next step is to intersect the profile <br>
   * (a) by Douglas-Peucker <br>
   * (b) equidistant. <br>
   * the geographical data is interpolated from the original profile data.
   *
   * @param wspmprofile
   *          original profile (WSPMProfile) to be intersected
   * @param prof
   *          additional informations of the corresponding intersection points of that profile (upstream / downstream)
   */
  private IProfil createCroppedIProfile( final SegmentData segment ) throws Exception
  {
    final IProfil originalProfile = m_feature.getProfil();
    final GM_Curve profileLine = m_feature.getLine();

    final Point point1 = getIntersPoint( profileLine, segment, SIDE.LEFT );
    final Point point2 = getIntersPoint( profileLine, segment, SIDE.RIGHT );

    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    // REMARK: the new profile is in kalypso coordinates
    final GM_Point intersectionPoint1 = (GM_Point)JTSAdapter.wrap( point1, kalypsoSRS );
    final GM_Point intersectionPoint2 = (GM_Point)JTSAdapter.wrap( point2, kalypsoSRS );

    final Double width1 = WspmProfileHelper.getWidthPosition( intersectionPoint1, originalProfile );
    final Double width2 = WspmProfileHelper.getWidthPosition( intersectionPoint2, originalProfile );

    /* exchange left and right accoring to orientation of profile */
    final GM_Point startPoint;
    final GM_Point endPoint;
    final double startWidth;
    final double endWidth;

    if( width1 > width2 )
    {
      startWidth = width2;
      endWidth = width1;

      startPoint = intersectionPoint2;
      endPoint = intersectionPoint1;
    }
    else
    {
      startWidth = width1;
      endWidth = width2;
      startPoint = intersectionPoint1;
      endPoint = intersectionPoint2;
    }

    // convert WSPM-Profil into IProfil and add start, end and all points between those

    // calculate elevations
    final double startHeight = WspmProfileHelper.getHeightByWidth( startWidth, originalProfile );
    final double endHeight = WspmProfileHelper.getHeightByWidth( endWidth, originalProfile );

    /* Create new profile and fill with record */
    final IProfil newProfil = ChannelEditUtil.createEmptyProfile( originalProfile );

    final String profilSRS = originalProfile.getSrsName();

    final IProfileRecord startRecord = newProfil.createProfilPoint();
    final IProfileRecord endRecord = newProfil.createProfilPoint();

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    startRecord.setBreite( startWidth );
    startRecord.setHoehe( startHeight );
    startRecord.setRechtswert( startPoint.getX() );
    startRecord.setHochwert( startPoint.getY() );

    endRecord.setBreite( endWidth );
    endRecord.setHoehe( endHeight );
    endRecord.setRechtswert( endPoint.getX() );
    endRecord.setHochwert( endPoint.getY() );

    newProfil.addPoint( startRecord );

    /* Copy points between start/end into new profile */
    final IProfileRecord[] profilPointList = originalProfile.getPoints();

    for( final IProfileRecord point : profilPointList )
    {
      final double currentWidth = point.getBreite();

      if( currentWidth > startWidth & currentWidth < endWidth )
      {
        /* transform location */
        final GM_Position location = GeometryFactory.createGM_Position( point.getRechtswert(), point.getHochwert() );
        final GM_Position transformedLocation = location.transform( profilSRS, kalypsoSRS );

        final IProfileRecord pt = newProfil.createProfilPoint();
        pt.setBreite( point.getBreite() );
        pt.setHoehe( point.getHoehe() );
        pt.setRechtswert( transformedLocation.getX() );
        pt.setHochwert( transformedLocation.getY() );

        newProfil.addPoint( pt );
      }
    }

    newProfil.addPoint( endRecord );

    return newProfil;
  }

  private Point getIntersPoint( final GM_Curve profileLine, final SegmentData segment, final SIDE side ) throws GM_Exception
  {
    final BankData bank = segment.getBank( side );
    final LineString segmentedBank = bank.getSegmented();

    // get the intersection point for the profile
    return findIntersectionPoint( profileLine, segmentedBank );
  }

  /* Intersection point is the nearest end-point of the bankline to the profile line */
  private static Point findIntersectionPoint( final GM_Curve profileLine, final LineString bankLine ) throws GM_Exception
  {
    final LineString jtsLine = (LineString)JTSAdapter.export( profileLine );

    final Point startPoint = bankLine.getStartPoint();
    final Point endPoint = bankLine.getEndPoint();

    final double distStart = jtsLine.distance( startPoint );
    final double distEnd = jtsLine.distance( endPoint );

    if( distStart < distEnd )
      return startPoint;
    else
      return endPoint;
  }

  private SegmentData findSegmentWithBanks( )
  {
    if( m_upSegment != null && m_upSegment.hasBanks() )
      return m_upSegment;

    if( m_downSegment != null && m_downSegment.hasBanks() )
      return m_downSegment;

    return null;
  }

  /**
   * The boundig box of the profile is the union of the boxes of the two adjacent segments and the real profile.
   */
  @Override
  public GM_Envelope getSegmentMapExtent( final String srsName ) throws GeometryException
  {
    final GM_Envelope profileEnvelope = m_feature.getBoundedBy();

    final GM_Envelope downEnvelpe = m_downSegment == null ? null : m_downSegment.getSegmentMapExtend( srsName );
    final GM_Envelope upEnvelpe = m_upSegment == null ? null : m_upSegment.getSegmentMapExtend( srsName );

    return GeometryUtilities.mergeEnvelopes( profileEnvelope, downEnvelpe, upEnvelpe );
  }

  @Override
  public String toString( )
  {
    return getFeature().getBigStation().toString();
  }

  @Override
  public void updateSegmentedProfile( final IProfil newSegmentedProfile )
  {
    Assert.isNotNull( newSegmentedProfile );
    Assert.isTrue( newSegmentedProfile.getPoints().length == m_numIntersections );

    m_isUserChanged = true;

    m_segmentedProfile = newSegmentedProfile;

    // TODO: update banks if necessary (end point changed)
    // TODO: move endpoints of adjacent banks to endpoints of profile

    /* update meshes of adjacent segments */
    if( m_downSegment != null )
      m_downSegment.updateMesh();

    if( m_upSegment != null )
      m_upSegment.updateMesh();
  }
}