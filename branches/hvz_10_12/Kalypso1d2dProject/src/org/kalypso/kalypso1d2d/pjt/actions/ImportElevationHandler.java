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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * @author madanago
 */
public class ImportElevationHandler extends AbstractHandler
{
  private static final String WIZARD_ID = "org.kalypso.ui.wizards.imports.elevationmodel.ImportElevationWizard"; //$NON-NLS-1$

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      /* Always open the manage dtm widget */
      final SelectWidgetHandler handler = new SelectWidgetHandler();
      final Map<String, String> newParameterMap = new HashMap<String, String>();
      newParameterMap.put( SelectWidgetHandler.PARAM_WIDGET_CLASS, "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget" ); //$NON-NLS-1$
      newParameterMap.put( SelectWidgetHandler.PARAM_PLUGIN_ID, "org.kalypso.model1d2d" ); //$NON-NLS-1$
      handler.setInitializationData( null, null, newParameterMap );
      final ExecutionEvent exc = new ExecutionEvent( event.getCommand(), newParameterMap, event.getTrigger(), event.getApplicationContext() );
      handler.execute( exc );

      /* Open import elevation model wizard */
      final ITerrainModel terrainModel = szenarioDataProvider.getModel( ITerrainModel.class );
      final IFolder modelFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

      final IFolder temFolder = modelFolder.getFolder( "models/native_tem" ); //$NON-NLS-1$

      final IStructuredSelection selection = new StructuredSelection( new Object[] { terrainModel, modelFolder, temFolder } );

      final IWorkbench workbench = PlatformUI.getWorkbench();

      final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( WIZARD_ID );
      final INewWizard wizard = (INewWizard) wizardDescriptor.createWizard();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final WizardDialog wizardDialog = new WizardDialog( shell, wizard );

      wizard.init( workbench, selection );

      if( wizardDialog.open() == Window.OK )
      {
        // REMARK: small hack: in order to refresh the widget, we execute the handler again
        handler.execute( exc );
        return Status.OK_STATUS;
      }
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportElevationHandler.4" ) ); //$NON-NLS-1$
    }

    return Status.CANCEL_STATUS;
  }
}
