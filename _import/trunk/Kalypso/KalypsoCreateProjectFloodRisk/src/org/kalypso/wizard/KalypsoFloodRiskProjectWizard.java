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

/*
 * Created on 31.01.2005
 *  
 */
package org.kalypso.wizard;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.ui.ImageProvider;

/**
 * 
 * KalypsoFloodRiskProjectWizard
 * <p>
 * Wizard for creating a floodrisk project
 * 
 * created by
 * 
 * @author Nadja Peiler (19.06.2005)
 */
public class KalypsoFloodRiskProjectWizard extends Wizard implements INewWizard
{

  static final String PROJECT_PAGE = "page_type:createNewProject";

  private WizardNewProjectCreationPage createProjectPage;

  private SelectLanduseWizardPage selectLanduseWizardPage;

  private SelectWaterlevelWizardPage selectWaterlevelWizardPage;

  //private CheckAutoGenerateWizardPage checkAutoGenerateWizardPage;

  public KalypsoFloodRiskProjectWizard()
  {
    super();
  }

  /**
   * 
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    try
    {
      createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
      createProjectPage.setDescription( "Dieser Dialog erstellt ein neues Hochwasserrisiko Projekt." );
      createProjectPage.setTitle( "Neues Hochwasserrisiko Projekt" );
      createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( createProjectPage );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    selectLanduseWizardPage = new SelectLanduseWizardPage();
    addPage( selectLanduseWizardPage );

    //checkAutoGenerateWizardPage = new CheckAutoGenerateWizardPage();
    //addPage( checkAutoGenerateWizardPage );

    selectWaterlevelWizardPage = new SelectWaterlevelWizardPage();
    addPage( selectWaterlevelWizardPage );

  }

  /**
   * This method creates the new project
   * 
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {

    final Job job = new CreateFloodRiskProjectJob( "Projekt erstellen", createProjectPage.getLocationPath(),
        createProjectPage.getProjectHandle(), selectLanduseWizardPage.getLanduseDataFile(), selectLanduseWizardPage
            .getPropertyName(), selectLanduseWizardPage.getSelectedCoordinateSystem(), selectLanduseWizardPage
            .isCheck(), selectWaterlevelWizardPage.getWaterlevelGrids(), selectWaterlevelWizardPage
            .getSelectedCoordinateSystem() );
    job.setUser( true );
    job.schedule();

    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  //nothing
  }

}