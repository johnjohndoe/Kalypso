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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.action.ProfileSelection;

/**
 * @author kimwerner
 */
public class SobekExportProfilesWizard extends ExportProfilesWizard
{
  private final SobekProfileExportFileChooserPage m_profileFileChooserPage;

  public SobekExportProfilesWizard( final ProfileSelection selection )
  {
    super( selection );

    setHelpAvailable( false );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );

    m_profileFileChooserPage = new SobekProfileExportFileChooserPage();
    addPage( m_profileFileChooserPage );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.export.ExportProfilesWizard#exportProfiles(org.kalypso.model.wspm.core.profil.IProfil[],
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final ISobekProfileExportOperation[] operations = m_profileFileChooserPage.getOperations( profiles );
    final Collection<IStatus> problems = new ArrayList<IStatus>( operations.length );

    monitor.beginTask( Messages.getString("SobekExportProfilesWizard_0"), operations.length ); //$NON-NLS-1$

    for( final ISobekProfileExportOperation operation : operations )
    {
      monitor.subTask( operation.getLabel() );

      final IStatus execute = operation.execute( new SubProgressMonitor( monitor, 1 ) );
      if( !execute.isOK() )
        problems.add( execute );
    }

    final IStatus[] problemChildren = problems.toArray( new IStatus[problems.size()] );
    if( problemChildren.length > 0 )
    {
      final String message = Messages.getString("SobekExportProfilesWizard_1"); //$NON-NLS-1$
      final IStatus status = new MultiStatus( KalypsoModelWspmTuhhUIPlugin.getID(), 0, problemChildren, message, null );
      throw new CoreException( status );
    }
  }
}
