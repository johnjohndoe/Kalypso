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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.model.wspm.ui.profil.wizard.ProfilesChooserPage;
import org.kalypso.model.wspm.ui.profil.wizard.results.IResultInterpolationSettings;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public abstract class ExportProfilesWizard extends Wizard implements IWorkbenchWizard
{
  public static final String STR_CHOOSE_EXPORT_FILE_TITLE = Messages.getString( "ExportProfilesWizard_0" ); //$NON-NLS-1$

  public static final String STR_CHOOSE_EXPORT_FILE_MESSAGE = Messages.getString( "ExportProfilesWizard_1" ); //$NON-NLS-1$

  public static final String STR_EXPORT_FILE_GROUP_TEXT = Messages.getString( "ExportProfilesWizard_2" ); //$NON-NLS-1$

  private ProfilesChooserPage m_profileChooserPage;

  private ProfileSelection m_profileSelection;

  public ExportProfilesWizard( )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "ExportProfilesWizard_3" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_profileSelection = new ProfileSelection( selection );
    final Feature[] profiles = m_profileSelection.getProfiles();

    final Feature[] selectedProfiles = m_profileSelection.getSelectedProfiles();

    final String pageMessage = Messages.getString( "ExportProfilesWizard_4" ); //$NON-NLS-1$
    final int numToSelect = getMinimumSelectionCount();
    m_profileChooserPage = new ProfilesChooserPage( pageMessage, profiles, new Object[0], selectedProfiles, numToSelect, false );

    addPage( m_profileChooserPage );
  }

  /**
   * The minimal number of profiles that the user needs to select. <code>1</code> by default.<br/>
   * Overwrite to change.
   * 
   * @return <code>1</code>.
   */
  protected int getMinimumSelectionCount( )
  {
    return 1;
  }

  protected ProfileSelection getProfileSelection( )
  {
    return m_profileSelection;
  }

  public void setShowResultInterpolationSettings( final boolean showResultInterpolationSettings )
  {
    m_profileChooserPage.setShowResultInterpolationSettings( showResultInterpolationSettings );
  }

  public IResultInterpolationSettings getResultInterpolationSettings( )
  {
    return m_profileChooserPage.getResultInterpolationSettings();
  }

  private IProfileFeature[] getChosenProfiles( final Object[] profilFeatures )
  {
    final Collection<IProfileFeature> profiles = new ArrayList<IProfileFeature>( profilFeatures.length );
    for( final Object profilFeature : profilFeatures )
    {
      if( profilFeature instanceof IProfileFeature )
      {
        final IProfileFeature wspmProfil = (IProfileFeature) profilFeature;
        if( wspmProfil != null )
        {
          profiles.add( wspmProfil );
        }
      }
    }

    return profiles.toArray( new IProfileFeature[] {} );
  }

  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    final IProfileFeature[] chosenProfiles = getChosenProfiles( profilFeatures );

    final ICoreRunnableWithProgress exportJob = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws InterruptedException
      {
        try
        {
          monitor.beginTask( Messages.getString( "ExportProfilesWizard_5" ), profilFeatures.length ); //$NON-NLS-1$

          return exportProfiles( chosenProfiles, monitor );
        }
        catch( final CoreException e )
        {
          final IStatus status = e.getStatus();
          if( status.matches( IStatus.CANCEL ) )
            throw new InterruptedException();

          return status;
        }
      }
    };

    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, exportJob );
    if( !result.isOK() )
    {
      new StatusDialog( getShell(), result, getWindowTitle() ).open();
    }
    return !result.matches( IStatus.ERROR );
  }

  protected abstract IStatus exportProfiles( IProfileFeature[] profiles, IProgressMonitor monitor ) throws CoreException;
}
