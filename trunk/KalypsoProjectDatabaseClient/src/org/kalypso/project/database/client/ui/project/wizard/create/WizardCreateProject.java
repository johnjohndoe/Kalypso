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
package org.kalypso.project.database.client.ui.project.wizard.create;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.afgui.wizards.NewProjectWizard;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ProjectTemplatePage;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Wizard to create a new local Planer Client Project
 * 
 * @author Dirk Kuch
 */
public class WizardCreateProject extends NewProjectWizard
{

  final protected String[] m_natures;

  /**
   * @param templates
   *          list of project templates
   * @param natures
   *          list of natures which will be added to the downloaded project
   */
  public WizardCreateProject( final ProjectTemplate[] templates, final String[] natures )
  {
    super( new ProjectTemplatePage( templates ) );
    m_natures = natures;

    setWindowTitle( "Neues Projekt erzeugen" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // bad hack
    final boolean finish = super.performFinish();
    if( !finish )
      return false;

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation()
    {

      @Override
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "aktualisiere Projekt", 1 );
        KalypsoGisPlugin.getDefault();
        final IProject newProject = getNewProject();
        final IProjectDescription description = newProject.getDescription();
        description.setName( description.getComment() );

        final String[] natures = (String[]) ArrayUtils.addAll( description.getNatureIds(), m_natures );
        ArrayUtils.add( natures, RemoteProjectNature.NATURE_ID );

        // unique natures
        final Set<String> myNatures = new HashSet<String>();
        for( final String nature : natures )
        {
          myNatures.add( nature );
        }
        description.setNatureIds( myNatures.toArray( new String[] {} ) );

        newProject.setDescription( description, monitor );
        monitor.done();
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, true, operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Anlegen des Projektes fehlgeschlagen.", status );

    return status.isOK();
  }

  public ProjectTemplate getSelectedTemplate( )
  {
    final ProjectTemplatePage page = (ProjectTemplatePage) getPage( "projectTemplatePage" );
    return page.getSelectedProject();
  }

}
