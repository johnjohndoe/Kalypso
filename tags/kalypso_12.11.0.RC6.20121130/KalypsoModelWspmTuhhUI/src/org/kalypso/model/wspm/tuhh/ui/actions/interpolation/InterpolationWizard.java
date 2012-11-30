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
package org.kalypso.model.wspm.tuhh.ui.actions.interpolation;

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.util.ProfileInterpolation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfilesSelection;
import org.kalypso.model.wspm.ui.profil.wizard.ProfileHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class InterpolationWizard extends Wizard implements IWorkbenchWizard
{
  private InterpolationStationPage m_stationPage;

  private WspmWaterBody m_waterBody;

  private TuhhReach m_reach;

  private InterpolationStationData m_interpolationData;

  private ProfilesSelection m_profileSelection;

  public InterpolationWizard( )
  {
    setNeedsProgressMonitor( false );
    setHelpAvailable( false );
    setWindowTitle( Messages.getString( "InterpolationWizard_0" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_profileSelection = ProfileHandlerUtils.getSelectionChecked( selection );

    final Feature container = m_profileSelection.getContainer();

    m_waterBody = ProfileUiUtils.findWaterbody( container );
    if( m_waterBody == null )
      throw new IllegalStateException( Messages.getString( "InterpolateProfileHandler_0" ) ); //$NON-NLS-1$

    m_reach = ProfileUiUtils.findReach( container );

    final IProfileFeature[] profiles = m_profileSelection.getProfiles();
    m_interpolationData = new InterpolationStationData( profiles );

    m_stationPage = new InterpolationStationPage( "stationPage", m_interpolationData ); //$NON-NLS-1$
    addPage( m_stationPage );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      doInterpolation( m_waterBody, m_reach, m_interpolationData );

      final CommandableWorkspace workspace = m_profileSelection.getWorkspace();
      workspace.postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      new StatusDialog( getShell(), e.getStatus(), getWindowTitle() ).open();
    }
    catch( final Exception e )
    {
      // will never happen
      e.printStackTrace();
    }

    return true;
  }

  private void doInterpolation( final WspmWaterBody waterBody, final TuhhReach reach, final InterpolationStationData interpolationData ) throws CoreException
  {
    try
    {
      final IProfileFeature previousProfileFeature = interpolationData.getPreviousProfile();
      final IProfile previousProfile = previousProfileFeature.getProfile();
      final IProfile nextProfile = interpolationData.getNextProfile().getProfile();
      final BigDecimal newStation = interpolationData.getNewStation();
      final boolean onlyRiverChannel = interpolationData.getOnlyChannel();

      final ProfileInterpolation interpolation = new ProfileInterpolation( previousProfile, nextProfile, onlyRiverChannel );
      final IProfile newProfile = interpolation.interpolate( newStation, previousProfile.getType() );

      /* TODO we should select this new feature... but in order to this, we need access to the selection provider */
      /* final Feature featureForSelection = */ProfileUiUtils.addNewProfileAndFireEvents( newProfile, waterBody, reach, previousProfileFeature );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString( "InterpolateProfileHandler_2" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }
}