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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
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
  /** Wether or not the area should be auto adjusted to the original profile */
  private final boolean m_doAreaAdjustment = true;

  private final IProfile m_originalProfile;

  private IProfile m_workingProfile;

  private SegmentData m_upSegment;

  private SegmentData m_downSegment;

  private final int m_numPoints;

  private boolean m_isUserChanged = false;

  private final String m_id;

  private final IProfileFeature m_feature;

  public ProfileData( final String id, final IProfileFeature feature, final IProfile originalProfile, final int numPoints )
  {
    m_id = id;
    m_feature = feature;
    m_originalProfile = originalProfile;
    m_numPoints = numPoints;
  }

  @Override
  public String getId( )
  {
    return m_id;
  }

  @Override
  public boolean isUserChanged( )
  {
    return m_isUserChanged;
  }

  @Override
  public IProfile getOriginalProfile( )
  {
    return m_originalProfile;
  }

  @Override
  public IProfile getWorkingProfile( )
  {
    return m_workingProfile;
  }

  /* Wraps profile features into this data class. */
  public static ProfileData[] from( final IProfileFeature[] features, final int numIntersections )
  {
    final ProfileData[] profiles = new ProfileData[features.length];

    for( int i = 0; i < profiles.length; i++ )
    {
      final IProfileFeature feature = features[i];

      final String id = feature.getId();

      final IProfile transformedProfil = transformOriginalProfile( feature );

      profiles[i] = new ProfileData( id, feature, transformedProfil, numIntersections );
    }

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

  public LineString getWorkingGeometry( )
  {
    try
    {
      if( m_workingProfile == null )
        return null;

      final GM_Curve line = (GM_Curve)ProfileUtil.getLine( m_workingProfile ).transform( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      return (LineString)JTSAdapter.export( line );
    }
    catch( final GM_Exception | GeoTransformerException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  void recalculateWorkingProfile( )
  {
    try
    {
      m_workingProfile = null;

      /* crop the profile */
      final SegmentData segment = findSegmentWithBanks();
      if( segment == null )
        return;

      final IProfile croppedProfile = createCroppedIProfile( segment );

      /* intersect the cropped profile */
      final ProfileIntersector segmenter = new ProfileIntersector( m_numPoints );
      final IProfile segmentedProfile = segmenter.execute( croppedProfile );

      /* Area adjustment */
      m_workingProfile = autoAdjustArea( croppedProfile, segmentedProfile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private IProfile autoAdjustArea( final IProfile croppedProfile, final IProfile segmentedProfile )
  {
    if( !m_doAreaAdjustment )
      return segmentedProfile;

    final ProfileAreaAdjuster adjuster = new ProfileAreaAdjuster( croppedProfile );
    return adjuster.execute( segmentedProfile );
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
  private IProfile createCroppedIProfile( final SegmentData segment ) throws GM_Exception, GeoTransformerException
  {
    final GM_Curve profileLine = getOriginalProfileGeometry();

    final Point point1 = getIntersPoint( profileLine, segment, SIDE.LEFT );
    final Point point2 = getIntersPoint( profileLine, segment, SIDE.RIGHT );

    return cropProfile( point1, point2 );
  }

  private IProfile cropProfile( final Point point1, final Point point2 ) throws GeoTransformerException, GM_Exception
  {
    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    // REMARK: the new profile is in kalypso coordinates
    final GM_Point intersectionPoint1 = (GM_Point)JTSAdapter.wrap( point1, kalypsoSRS );
    final GM_Point intersectionPoint2 = (GM_Point)JTSAdapter.wrap( point2, kalypsoSRS );

    final Double width1 = WspmProfileHelper.getWidthPosition( intersectionPoint1, m_originalProfile );
    final Double width2 = WspmProfileHelper.getWidthPosition( intersectionPoint2, m_originalProfile );

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
    final double startHeight = WspmProfileHelper.getHeightByWidth( startWidth, m_originalProfile );
    final double endHeight = WspmProfileHelper.getHeightByWidth( endWidth, m_originalProfile );

    /* Create new profile and fill with record */
    final IProfile newProfil = ChannelEditUtil.createEmptyProfile( m_originalProfile );

    final String profilSRS = m_originalProfile.getSrsName();

    final IProfileRecord startRecord = newProfil.createProfilPoint();
    final IProfileRecord endRecord = newProfil.createProfilPoint();

    /* calculate the width of the intersected profile */

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
    final IProfileRecord[] profilPointList = m_originalProfile.getPoints();

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
    final LineString segmentedBank = bank.getWorkingGeometry();

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
  public GM_Envelope getMapExtent( final String srsName ) throws GM_Exception
  {
    final GM_Envelope profileEnvelope = getOriginalProfileGeometry().getEnvelope();

    final GM_Envelope downEnvelpe = m_downSegment == null ? null : m_downSegment.getSegmentMapExtent( srsName );
    final GM_Envelope upEnvelpe = m_upSegment == null ? null : m_upSegment.getSegmentMapExtent( srsName );

    return GeometryUtilities.mergeEnvelopes( profileEnvelope, downEnvelpe, upEnvelpe );
  }

  @Override
  public GM_Curve getOriginalProfileGeometry( ) throws GM_Exception
  {
    return ProfileUtil.getLine( m_originalProfile );
  }

  @Override
  public String toString( )
  {
    return String.format( "%.4f", m_originalProfile.getStation() ); //$NON-NLS-1$
  }

  @Override
  public void updateWorkingProfile( final IProfile newWorkingProfile )
  {
    Assert.isNotNull( newWorkingProfile );
    Assert.isTrue( newWorkingProfile.getPoints().length == m_numPoints );

    m_isUserChanged = true;

    try
    {
      /* adjust area */
      final LineString segmentedLine = ProfileUtil.getLineString( newWorkingProfile );
      // REMARK: we know the segmented profile is in Kalypso srs, so no projection is needed
      final Point startPoint = segmentedLine.getStartPoint();
      final Point endPoint = segmentedLine.getEndPoint();

      final IProfile croppedProfile = cropProfile( startPoint, endPoint );

      final IProfile adjustedSegmentedProfile = autoAdjustArea( croppedProfile, newWorkingProfile );

      /* force endpoint of bank lines onto endpoints of profile */
      adjustBanklineEndpoints( m_workingProfile, adjustedSegmentedProfile );

      m_workingProfile = adjustedSegmentedProfile;
    }
    catch( GeoTransformerException | GM_Exception e )
    {
      e.printStackTrace();
    }

    /* update meshes */
    if( m_downSegment != null )
      m_downSegment.updateMesh();
    if( m_upSegment != null )
      m_upSegment.updateMesh();
  }

  private void adjustBanklineEndpoints( final IProfile oldSegmentedProfile, final IProfile newSegmentedProfile )
  {
    if( oldSegmentedProfile == null )
      return;

    final IProfileRecord[] oldPoints = oldSegmentedProfile.getPoints();
    final IProfileRecord[] newPoints = newSegmentedProfile.getPoints();

    /* handle start point */
    final Coordinate oldStartLocation = oldPoints[0].getCoordinate();
    final Coordinate newStartLocation = newPoints[0].getCoordinate();
    if( oldStartLocation.distance( newStartLocation ) > 0.0001 )
      adjustBanklineEndpoints( oldStartLocation, newStartLocation );

    /* handle end point */
    final Coordinate oldEndLocation = oldPoints[oldPoints.length - 1].getCoordinate();
    final Coordinate newEndLocation = newPoints[oldPoints.length - 1].getCoordinate();
    if( oldEndLocation.distance( newEndLocation ) > 0.0001 )
      adjustBanklineEndpoints( oldEndLocation, newEndLocation );
  }

  private void adjustBanklineEndpoints( final Coordinate oldEndpointLocation, final Coordinate newEndpointLocation )
  {
    /* update banks and meshes of adjacent segments */
    if( m_downSegment != null )
      m_downSegment.updateBankEndpoints( oldEndpointLocation, newEndpointLocation );

    if( m_upSegment != null )
      m_upSegment.updateBankEndpoints( oldEndpointLocation, newEndpointLocation );
  }

  IProfileFeature getFeature( )
  {
    return m_feature;
  }

  private static IProfile transformOriginalProfile( final IProfileFeature feature )
  {
    try
    {
      /* we do not want to change the orignial profile, so we first have to clone it */
      /* Get the feature type of the root feature of the workspace. */
      final IFeatureType rootFeatureType = feature.getFeatureType();

      /* Create temporary workspace. */
      final IFeatureProviderFactory factory = feature.getWorkspace().getFeatureProviderFactory();
      final GMLWorkspace tmpWorkspace = FeatureFactory.createGMLWorkspace( rootFeatureType, null, factory );
      final IProfileFeature clonedProfile = (IProfileFeature)tmpWorkspace.getRootFeature();
      FeatureHelper.copyProperties( feature, clonedProfile, (QName[])null );

      /* change crs of the cloned profile */
      final IProfile transformedProfile = clonedProfile.getProfile();

      final String sourceSRS = transformedProfile.getSrsName();
      final String targetSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      final IProfileRecord[] records = transformedProfile.getPoints();
      for( final IProfileRecord record : records )
      {
        final Double rw = record.getRechtswert();
        final Double hw = record.getHochwert();

        if( Objects.isNotNull( rw, hw ) )
        {
          final GM_Position pos = GeometryFactory.createGM_Position( rw, hw );
          final GM_Position transformedPos = pos.transform( sourceSRS, targetSRS );

          record.setRechtswert( transformedPos.getX() );
          record.setHochwert( transformedPos.getY() );
        }
      }

      transformedProfile.setSrsName( targetSRS );

      return transformedProfile;
    }
    catch( final Exception e )
    {
      // sould never happen...
      e.printStackTrace();
      return null;
    }
  }
}