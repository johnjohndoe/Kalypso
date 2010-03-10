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
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.model.wspm.ui.profil.wizard.ProfilesChooserPage;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public abstract class ExportProfilesWizard extends Wizard
{
  protected static final String STR_CHOOSE_EXPORT_FILE_TITLE = "Exportdatei w�hlen";

  protected static final String STR_CHOOSE_EXPORT_FILE_MESSAGE = "Bitte w�hlen Sie die Exportdatei aus.";

  protected static final String STR_EXPORT_FILE_GROUP_TEXT = "Target File";

  private final ArrayChooserPage m_profileChooserPage;

  public ExportProfilesWizard( final ProfileSelection selection )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( "Profilexport" );

    final Feature[] profiles = selection.getProfiles();

    final Feature[] selectedProfiles = selection.getSelectedProfiles();

    final String pageMessage = "Bitte w�hlen Sie die Profile aus, die exportiert werden sollen.";
    m_profileChooserPage = new ProfilesChooserPage( pageMessage, profiles, new Object[0], selectedProfiles, 1, false );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    addPage( m_profileChooserPage );
  }

  private IProfil[] getChosenProfiles( final Object[] profilFeatures )
  {
    final Collection<IProfil> profiles = new ArrayList<IProfil>( profilFeatures.length );

    for( final Object profilFeature : profilFeatures )
    {
      if( profilFeature instanceof Feature )
      {
        final IProfileFeature wspmProfil = (IProfileFeature) profilFeature;
        if( wspmProfil != null )
          profiles.add( wspmProfil.getProfil() );
      }
    }
    return profiles.toArray( new IProfil[] {} );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    final IProfil[] chosenProfiles = getChosenProfiles( profilFeatures );

    final ICoreRunnableWithProgress m_exportJob = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        try
        {
          monitor.beginTask( "Profile exportieren", profilFeatures.length );

          exportProfiles( chosenProfiles, monitor );

          return Status.OK_STATUS;
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
      }
    };

    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, m_exportJob );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Failed to export profiles", result );
    return !result.matches( IStatus.ERROR );
  }

  protected abstract void exportProfiles( IProfil[] profiles, IProgressMonitor monitor ) throws CoreException;
}
