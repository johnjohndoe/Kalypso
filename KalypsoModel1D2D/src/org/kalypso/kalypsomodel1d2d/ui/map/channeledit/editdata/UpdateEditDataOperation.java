/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypsodeegree.model.geometry.GM_Curve;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 */
public class UpdateEditDataOperation implements ICoreRunnableWithProgress
{
  private final ChannelMesh m_oldData;

  private final ChannelMesh m_newData;

  private Map<String, IProfileData> m_oldProfileHash;

  private Map<String, ISegmentData> m_oldSegmentHash;

  private Map<String, IProfileData> m_newProfileHash;

  private Map<String, ISegmentData> m_newSegmentHash;

  public UpdateEditDataOperation( final ChannelMesh oldData, final ChannelMesh newData )
  {
    m_oldData = oldData;
    m_newData = newData;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final String taskName = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.0" ); //$NON-NLS-1$

    monitor.beginTask( taskName, 0 );

    /* fetch data */
    final Map<GM_Curve, SIDE> banks = m_newData.getBanklines();
    final int numberBanklinePoints = m_newData.getNumberBanklinePoints();
    final IProfileData[] profiles = m_newData.getProfiles();

    /* hash profiles */
    m_oldProfileHash = hashProfiles( m_oldData );
    m_oldSegmentHash = hashSegments( m_oldData.getSegments() );
    m_newProfileHash = hashProfiles( m_newData );

    /* fix: prepare for exception */
    m_newSegmentHash = new HashMap<>();

    final SegmentData[] segments = createSegments( profiles, banks, numberBanklinePoints );

    /* hash it for later */
    m_newSegmentHash = hashSegments( segments );

    m_newData.setSegments( segments );

    /* build intersections and meshes */
    updateSegments( profiles, segments );

    return Status.OK_STATUS;
  }

  private Map<String, ISegmentData> hashSegments( final ISegmentData[] segments )
  {
    final Map<String, ISegmentData> segmentHash = new HashMap<>();

    if( segments == null )
      return segmentHash;

    for( final ISegmentData segment : segments )
    {
      final String key = getSegmentKey( segment );

      segmentHash.put( key, segment );
    }

    return segmentHash;
  }

  private String getSegmentKey( final ISegmentData segment )
  {
    final IProfileData profileDown = segment.getProfileDown();
    final IProfileData profileUp = segment.getProfileUp();

    return String.format( "%s#%s", profileDown.getId(), profileUp.getId() ); //$NON-NLS-1$
  }

  private static Map<String, IProfileData> hashProfiles( final ChannelMesh data )
  {
    final IProfileData[] profiles = data.getProfiles();
    final Map<String, IProfileData> profileHash = new HashMap<>();
    for( final IProfileData profile : profiles )
      profileHash.put( profile.getId(), profile );

    return profileHash;
  }

  private void updateSegments( final IProfileData[] profiles, final ISegmentData[] segments )
  {
    // if( banks.size() < 2 )
//      return  ;

    /* update segments with old existing data */
    for( final ISegmentData segment : segments )
      updateSegmentFromOld( segment );

    /**
     * Intersects the two WSPM profiles (upstream/downstream) with the allready intersected bank lines (left/right) of
     * the current segment.<br>
     * The two profiles will be cropped at the intersection points and intersected by a specific number of points. <br>
     * Afterwards an area adjustment for the intersected profiles will be done .
     */
    for( final IProfileData profile : profiles )
      ((ProfileData)profile).recalculateWorkingProfile();

    /* save old edited profiles by simply copying them to the new data objects */
    for( final IProfileData profile : profiles )
    {
      final IProfileData oldProfile = m_oldProfileHash.get( profile.getId() );
      if( oldProfile != null && oldProfile.isUserChanged() )
      {
        /* simply set old working profile to new one */
        profile.updateWorkingProfile( oldProfile.getWorkingProfile() );
      }
    }

    for( final ISegmentData segment : segments )
      ((SegmentData)segment).updateMesh();
  }

  private SegmentData[] createSegments( final IProfileData[] profiles, final Map<GM_Curve, SIDE> banks, final int numberBanklinePoints )
  {
    final Collection<SegmentData> segments = new ArrayList<>();

    // loop over all profiles
    // take two neighbouring profiles create a segment for them

    IProfileData lastProfile = null;
    for( final IProfileData profile : profiles )
    {
//      if( !checkInteIntersectionWithBanks( profile ) )
//        continue;

      if( lastProfile != null )
      {
        final ProfileData downProfile = (ProfileData)lastProfile;
        final ProfileData upProfile = (ProfileData)profile;

        final SegmentData segment = new SegmentData( upProfile, downProfile, banks, numberBanklinePoints );

        segments.add( segment );
      }

      lastProfile = profile;
    }

    return segments.toArray( new SegmentData[segments.size()] );
  }

  private void updateSegmentFromOld( final ISegmentData newSegment )
  {
    final String key = getSegmentKey( newSegment );

    final ISegmentData oldSegment = m_oldSegmentHash.get( key );
    if( oldSegment == null )
      return;

    /* left */
    final IBankData oldLeft = oldSegment.getBankLeft();
    final IBankData newLeft = newSegment.getBankLeft();

    final IBankData oldRight = oldSegment.getBankRight();
    final IBankData newRight = newSegment.getBankRight();

    updateBankFromOld( newSegment, oldLeft, newLeft );
    updateBankFromOld( newSegment, oldRight, newRight );
  }

  private void updateBankFromOld( final ISegmentData newSegment, final IBankData oldBank, final IBankData newBank )
  {
    /* if no old, changed, data is available, do nothing */
    if( oldBank == null || !oldBank.isUserChanged() )
      return;

    /* if new bank is empty, do nothing */
    if( newBank == null )
      return;

    /* only update, if we are using the same original bank data */
    final LineString croppedOld = oldBank.getCroppedOriginalGeometry();
    final LineString croppedNew = newBank.getCroppedOriginalGeometry();

    if( !ObjectUtils.equals( croppedOld, croppedNew ) )
      return;

    final LineString oldWorkingGeometry = oldBank.getWorkingGeometry();
    newSegment.updateWorkingGeometry( newBank, oldWorkingGeometry );
  }

  public boolean hasDataLoss( )
  {
    /* check for data loss in profiles */
    for( final String oldID : m_oldProfileHash.keySet() )
    {
      if( !m_newProfileHash.containsKey( oldID ) )
      {
        final IProfileData oldProfile = m_oldProfileHash.get( oldID );
        if( oldProfile.isUserChanged() )
          return true;
      }
    }

    /* check for data loss in banks */
    for( final String oldKey : m_oldSegmentHash.keySet() )
    {
      final ISegmentData oldSegment = m_oldSegmentHash.get( oldKey );

      if( m_newSegmentHash.containsKey( oldKey ) )
      {
        /* old segment is still there, but maybe banks have changed? */
        final ISegmentData newSegment = m_newSegmentHash.get( oldKey );
        if( isBankDataLost( oldSegment, newSegment ) )
          return true;
      }
      else
      {
        /* oldSegment is no longer available, but was user edited */
        if( oldSegment != null && oldSegment.isBanksUserChanged() )
          return true;
      }
    }

    return false;
  }

  private boolean isBankDataLost( final ISegmentData oldSegment, final ISegmentData newSegment )
  {
    final IBankData oldLeft = oldSegment.getBankLeft();
    final IBankData newLeft = newSegment.getBankLeft();

    if( oldLeft != null && oldLeft.isUserChanged() && (newLeft == null || !newLeft.isUserChanged()) )
      return true;

    final IBankData oldRight = oldSegment.getBankRight();
    final IBankData newRight = newSegment.getBankRight();

    if( oldRight != null && oldRight.isUserChanged() && (newRight == null || !newRight.isUserChanged()) )
      return true;

    return false;
  }
}