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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * @author madanago
 */
public class ImportElevationHandler extends WorkflowCommandHandler
{
  private static final String WIZARD_ID = "org.kalypso.ui.wizards.imports.elevationmodel.ImportElevationWizard";

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final ISzenarioDataProvider szenarioDataProvider = (ISzenarioDataProvider) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
    final ITerrainModel terrainModel = (ITerrainModel) szenarioDataProvider.getModel( ITerrainModel.class );
    final IFolder modelFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IFolder temFolder = modelFolder.getFolder( "models/native_tem" );

    IStructuredSelection selection = new StructuredSelection( new Object[] { terrainModel, modelFolder, temFolder } );

    // if(selection == null)
    // {
    // final IResource currentFolder = (IFolder) context.getVariable( "activeSimulationModelBaseFolder" );
    // selection = new StructuredSelection(currentFolder);
    // }
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbench workbench = (workbenchWindow).getWorkbench();

    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( WIZARD_ID );
    final INewWizard wizard = (INewWizard) wizardDescriptor.createWizard();
    final WizardDialog wizardDialog = new WizardDialog( workbenchWindow.getShell(), wizard );

//    final HashMap<String, Object> data = new HashMap<String, Object>();
    // data.put( "ScenarioFolder", currentFolder.getFullPath().toOSString() );
    // data.put( "ActiveSimulationModelBaseFolder", currentFolder.getFullPath() );

    wizard.init( workbench, selection );
    // wizard.initModelProperties( data );
    if( wizardDialog.open() == Window.OK )
      return Status.OK_STATUS;
    return Status.CANCEL_STATUS;

  }
}
