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
package org.kalypso.afgui.wizards;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

/**
 * Basic wizard implementation for the various workflow/scenario based projects.<br>
 * Normally, only the location of the project-template (-zip) should be enough.<br>
 * 
 * @author Gernot Belger
 */
public class NewProjectWizard extends BasicNewProjectResourceWizard
{
  private final URL m_projectTemplateZipLocation;

  /**
   * @param projectTemplateZipLocation
   *            The location of the project template. The zip file must contain a <code>.project</code> file. All
   *            natures of the project will be configured after unzipping the template.
   */
  public NewProjectWizard( final URL projectTemplateZipLocation )
  {
    m_projectTemplateZipLocation = projectTemplateZipLocation;
  }

  @Override
  /**
   * This method was overriden in order to get rid of the 'select dependend projects' page from the
   * BasicNewProjectResourceWizard.
   */
  public IWizardPage getNextPage( final IWizardPage page )
  {
    // HACK: to do so, we just skip this particular page
    // Unfortunateley we cannot just overide 'addPages' and do not add the secod page,
    // because the BasicNewProjectResourceWizard relies on the second page to exist.
    final IWizardPage[] pages = getPages();

    if( page.equals( pages[0] ) )
      return null;

    return super.getNextPage( page );
  }

  @Override
  public boolean performFinish( )
  {
    final boolean result = super.performFinish();

    if( !result )
      return false;

    final URL zipURl = m_projectTemplateZipLocation;
    final IProject project = getNewProject();

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        final SubMonitor progress = SubMonitor.convert( monitor, "Projektstruktur wird erzeugt", 50 );

        try
        {
          /* Unpack project from template */
          ZipUtilities.unzipToContainer( zipURl, project, progress.newChild( 40 ) );
          ProgressUtilities.worked( progress, 0 );

          /* configure all natures of this project */
          final IProjectDescription description = project.getDescription();
          final String[] natureIds = description.getNatureIds();

          progress.setWorkRemaining( natureIds.length + 5 );

          for( final String natureId : natureIds )
          {
            final IProjectNature nature = project.getNature( natureId );
            nature.configure();

            ProgressUtilities.worked( progress, 1 );
          }

          /* Also activate new project */
          final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
          final Scenario caze = nature.getCaseManager().getCases().get( 0 );
          KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().setCurrentCase( caze );
          ProgressUtilities.worked( progress, 5 );
        }
        catch( final CoreException t )
        {
          // If anything went wrong, clean up the project
          progress.setWorkRemaining( 10 );
          project.delete( true, progress );

          throw t;
        }
        catch( final Throwable t )
        {
          // If anything went wrong, clean up the project
          progress.setWorkRemaining( 10 );
          project.delete( true, progress );

          throw new InvocationTargetException( t );
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus resultStatus = RunnableContextHelper.execute( getContainer(), true, true, operation );
    KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( getShell(), "Projekt Neu", "Fehler beim Erzeugen des Projekts", resultStatus );

    // REMARK: we always return here, because the BasicNewProjectWizard does not allow to create a project twice
    // So the wizard must be closed now
    return true;
  }

}
