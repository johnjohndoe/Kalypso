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
package org.kalypso.model.km.internal.binding;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.IWizardContainer;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.core.ProfileData;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileObservationReader;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.model.km.internal.ui.kmupdate.ProfileSetReadOperation;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Gernot Belger
 */
public class KMChannelElement
{
  private final KMChannel m_channel;

  private final KalininMiljukovType m_km;

  private ProfileDataSet m_profileSet;

  public KMChannelElement( final KMChannel channel, final KalininMiljukovType km )
  {
    m_channel = channel;
    m_km = km;
  }

  public KalininMiljukovType getKMType( )
  {
    return m_km;
  }

  public KMChannel getKMChannel( )
  {
    return m_channel;
  }

  public void loadData( final IWizardContainer context )
  {
    final String path = m_km.getFile();
    /* Prevents dead lock during construction of page */
    if( StringUtils.isBlank( path ) )
    {
      m_profileSet = null;
      return;
    }

    final ProfileSetReadOperation operation = new ProfileSetReadOperation( path );
    final IStatus status = RunnableContextHelper.execute( context, true, false, operation );
    m_profileSet = operation.getProfileSet();
    if( !status.isOK() )
      new StatusDialog( context.getShell(), status, Messages.getString( "KMChannelElement.0" ) ).open(); //$NON-NLS-1$

    updateProfileList();
  }

  public void loadData( ) throws CoreException
  {
    final String path = m_km.getFile();
    /* Prevents dead lock during construction of page */
    if( StringUtils.isBlank( path ) )
    {
      m_profileSet = null;
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString("KMChannelElement.1") ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    try
    {
      final ProfileObservationReader reader = new ProfileObservationReader( new Path( m_km.getFile() ) );
      m_profileSet = reader.getDataSet();
      updateProfileList();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), "Failed to read profile data", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private void updateProfileList( )
  {
    if( m_km == null )
      return;

    /* Keep enabled stations */
    final Set<BigDecimal> enabledStations = new HashSet<>( Arrays.asList( getEnabledStations() ) );

    final List<Profile> profileList = m_km.getProfile();
    profileList.clear();

    if( m_profileSet == null )
      return;

    final ProfileData[] allProfiles = m_profileSet.getAllProfiles();
    for( final ProfileData pd : allProfiles )
    {
      final Profile profile = KMBindingUtils.OF.createKalininMiljukovTypeProfile();

      final BigDecimal station = pd.getStation();
      profile.setStation( station );
      final boolean wasChecked = enabledStations.contains( station );
      profile.setEnabled( wasChecked );
      profileList.add( profile );
    }
  }

  private BigDecimal[] getEnabledStations( )
  {
    final Collection<BigDecimal> result = new ArrayList<>();

    final List<Profile> profiles = m_km.getProfile();
    for( final Profile profile : profiles )
    {
      if( profile.isEnabled() )
        result.add( profile.getStation() );
    }

    return result.toArray( new BigDecimal[result.size()] );
  }

  public ProfileDataSet getEnabledSet( )
  {
    if( m_profileSet == null )
      return null;

    final ProfileData[] enabledProfiles = getEnabledProfileData();
    return new ProfileDataSet( enabledProfiles );
  }

  private ProfileData[] getEnabledProfileData( )
  {
    final Collection<ProfileData> result = new ArrayList<>();

    final List<Profile> profiles = m_km.getProfile();
    for( final Profile profile : profiles )
    {
      final ProfileData profileData = findData( profile );

      final BigDecimal station = profile.getStation();

      final boolean isBetween = KMBindingUtils.isBetween( m_km, station );
      final boolean isValid = profileData.isValidForKalypso() == null;
      final boolean isEnabled = profile.isEnabled();
      if( isBetween && isValid && isEnabled )
        result.add( profileData );
    }

    return result.toArray( new ProfileData[result.size()] );
  }

  public ProfileDataSet getProfileSet( )
  {
    return m_profileSet;
  }

  public boolean isValid( final Profile profile )
  {
    final ProfileData data = findData( profile );
    if( data == null )
      return false;

    return data.isValidForKalypso() == null;
  }

  private ProfileData findData( final Profile profile )
  {
    if( m_profileSet == null )
      return null;

    final BigDecimal profileStation = profile.getStation();

    final ProfileData[] data = m_profileSet.getAllProfiles();
    for( final ProfileData profileData : data )
    {
      final BigDecimal station = profileData.getStation();
      if( ObjectUtils.equals( profileStation, station ) )
        return profileData;
    }

    return null;
  }

  public String getValidMessage( final Profile profile )
  {
    final ProfileData data = findData( profile );
    if( data == null )
      return StringUtils.EMPTY;

    final String valid = data.isValidForKalypso();
    if( valid == null )
      return Messages.getString( "org.kalypso.model.km.ProfileData.10" ); //$NON-NLS-1$

    return valid;
  }

  public void enableAllProfiles( )
  {
    final List<Profile> profiles = m_km.getProfile();
    for( final Profile profile : profiles )
    {
      final ProfileData pd = findData( profile );
      final boolean valid = pd == null ? false : pd.isValidForKalypso() == null;
      profile.setEnabled( valid );
    }
  }
}