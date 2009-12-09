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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class NewProjectImportWspwinWizard extends NewProjectWizard
{
  private WspWinImportSourcePage m_wspWinImportPage;

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_wspWinImportPage = new WspWinImportSourcePage( "pageImportWspwin" ); //$NON-NLS-1$
    addPage( m_wspWinImportPage );

    super.addPages();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( final Composite pageContainer )
  {
    // REMARK: do not create the controls now
    // in order to allow the hack in getNextPage
    // super.createPageControls(pageContainer);
  }

  /**
   * Overwritten in order to set the new project name to the name of the selected wspwin directory (= name of the wspwin
   * project).
   * 
   * @see org.eclipse.jface.wizard.Wizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  @Override
  public IWizardPage getNextPage( final IWizardPage page )
  {
    final IWizardPage nextPage = super.getNextPage( page );

    if( page == m_wspWinImportPage && nextPage instanceof WizardNewProjectCreationPage )
    {
      final File sourceDirectory = m_wspWinImportPage.getSourceDirectory();
      if( sourceDirectory != null )
      {
        final String name = sourceDirectory.getName();
        final WizardNewProjectCreationPage projectPage = (WizardNewProjectCreationPage) nextPage;
        projectPage.setInitialProjectName( name );
      }
    }

    return nextPage;
  }

  /**
   * Additionally import wspwin data into freshly created project.
   * 
   * @see org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectWizard#doFinish(org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doFinish( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectImportWspwinWizard.1"), 10 ); //$NON-NLS-1$

    super.doFinish( project, new SubProgressMonitor( monitor, 5 ) );

    final File wspwinDirectory = m_wspWinImportPage.getSourceDirectory();

    try
    {
      final IStatus status = WspWinImporter.importProject( wspwinDirectory, project, new SubProgressMonitor( monitor, 5 ) );
      if( !status.isOK() )
        throw new CoreException( status );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      // also log it, because 'performFinish' won't catch it any more
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
      throw new CoreException( status );
    }
  }
}
