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
package org.kalypso.wizards.export2d;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.ui.KalypsoGisPlugin;

public class ExportWizard extends Wizard implements INewWizard
{
  private FileSelectWizardPage m_page1;

  protected IProject m_project = null;

  public ExportWizard( )
  {
    final IDialogSettings settings = KalypsoGisPlugin.getDefault().getDialogSettings();

    IDialogSettings section = settings.getSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    if( section == null )
      section = settings.addNewSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    m_page1 = new FileSelectWizardPage( "fileselect", new String[] { "*.2d" } );
    addPage( m_page1 );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( Composite pageContainer )
  {
    setWindowTitle( "Als Datei exportieren" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
      final SzenarioDataProvider dataProvider = Kalypso1d2dProjectPlugin.getDefault().getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
      final ITerrainModel terrainModel = dataProvider.getModel( ITerrainModel.class );
      final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class );
      final Gml2RMA10SConv converter = new Gml2RMA10SConv( new File( m_page1.getFilePath() ), discretisationModel, terrainModel, flowRelationshipModel );
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor )
        {
          monitor.beginTask( "FE Netz exportieren", IProgressMonitor.UNKNOWN );
          try
          {
            converter.toRMA10sModel();
          }
          catch( final SimulationException e )
          {
            e.printStackTrace();
            MessageDialog.openError( getShell(), "Datei kann nicht erzeugt werden", e.getLocalizedMessage() );
          }
        }
      } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      MessageDialog.openError( getShell(), "Datei kann nicht erzeugt werden", e.getLocalizedMessage() );
      return false;
    }

    if( m_project != null )
    {
      final Job refreshJob = new Job( "Projekt " + m_project.getName() + " aktualisieren" )
      {
        @Override
        protected IStatus run( IProgressMonitor monitor )
        {
          try
          {
            m_project.refreshLocal( IResource.DEPTH_INFINITE, monitor );
          }
          catch( CoreException e )
          {
            e.printStackTrace();

            return StatusUtilities.statusFromThrowable( e );
          }

          return Status.OK_STATUS;
        }
      };

      refreshJob.schedule();
    }

    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // TODO Auto-generated method stub

  }
}
