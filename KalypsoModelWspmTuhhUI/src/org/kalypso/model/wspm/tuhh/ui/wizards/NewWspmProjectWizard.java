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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

public class NewWspmProjectWizard extends org.kalypso.afgui.wizards.NewProjectWizard
{
  public static final String WSPM_TUHH_PROJECT_TEMPLATE_CATEGORY = "org.kalypso.model.wspm.tuhh.projectTemplate"; //$NON-NLS-1$

  public NewWspmProjectWizard( )
  {
    this( WSPM_TUHH_PROJECT_TEMPLATE_CATEGORY, false );
  }

  public NewWspmProjectWizard( final String categoryId, final boolean showTemplatePage )
  {
    super( categoryId, showTemplatePage, KalypsoWspmTuhhModule.ID );
  }

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#openProject(org.eclipse.core.resources.IProject)
   */
  @Override
  public void openProject( final IProject project )
  {
    final IFile fileToOpen = project.getFile( IWspmTuhhConstants.FILE_WSPM_GMV );
    openTreeView( fileToOpen );
  }

  private void openTreeView( final IFile file )
  {
    final UIJob job = new UIJob( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectWizard.9" ) ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          final FileEditorInput input = new FileEditorInput( file );
          getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor( input, "org.kalypso.ui.editor.GmlEditor" ); //$NON-NLS-1$

          return Status.OK_STATUS;
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
      }
    };

    job.setUser( true );
    job.schedule();
  }

}
