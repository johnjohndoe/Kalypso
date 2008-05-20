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
package org.kalypso.model.wspm.ui.wizard;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.core.imports.ImportTrippleHelper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.action.WspmImportProfileHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;

/**
 * A wizard to import profile data (right now just as trippel) into a WSPM Model.
 * 
 * @author Thomas Jung
 */
public class ImportProfileWizard extends Wizard implements IWizard
{
  public static String PROFIL_TYPE_PASCHE = "org.kalypso.model.wspm.tuhh.profiletype"; //$NON-NLS-1$

  protected ImportProfilePage m_ProfilePage;

  private final FeatureAssociationTypeElement m_fate;

  private final CommandableWorkspace m_workspace;

  public ImportProfileWizard( final FeatureAssociationTypeElement fate, final CommandableWorkspace workspace )
  {
    m_fate = fate;
    m_workspace = workspace;
    setWindowTitle( "Kalypso Profil Import" ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose profile data */
    m_ProfilePage = new ImportProfilePage( "chooseProfileData", Messages.ImportProfileWizard_2, null ); //$NON-NLS-1$
    m_ProfilePage.setDescription( Messages.ImportProfileWizard_3 );

    addPage( m_ProfilePage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( Messages.ImportProfileWizard_5, 2 );

        try
        {
          /* Import Trippel Data */
          monitor.subTask( Messages.ImportProfileWizard_6 );

          /* get file name from wizard */
          final File trippelFile = m_ProfilePage.getFile();
          final String separator = m_ProfilePage.getSeparator();
          final String crs = m_ProfilePage.getCoordinateSystem();

          List<IProfil> profiles = ImportTrippleHelper.importTrippelData( trippelFile, separator, PROFIL_TYPE_PASCHE );

// new AddDeviderResolution

          monitor.worked( 1 );

          /* Convert Trippel Data */
          monitor.subTask( Messages.ImportProfileWizard_7 );

          WspmImportProfileHelper.loadIntoGml( profiles, m_fate, m_workspace, crs );

          /* convert them into the profile-list */

          monitor.worked( 1 );

          return Status.OK_STATUS;
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e, "Fehler beim Laden der Profile" ); //$NON-NLS-1$
        }

        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Probleme beim Profil-Import", status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }
}
