/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.simulation.statistics;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

public class StatisticAnalysisHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getDisplay().getActiveShell();
    final String title = HandlerUtils.getCommandName( event );

    final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    /* read data and check prerequisites */
    final StatisticCalculationData data = initData( shell, title, scenarioDataProvider );
    if( data == null )
      return null;

    // TODO: check if results are available and up-to-date

    /* Ask user for shapes and other options */
    if( !askForShape( data, shell, title ) )
      return null;


    final ICoreRunnableWithProgress operation = new StatisticCalculationOperation( data );
    // TODO: handle cancel
    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    StatusDialog.open( shell, status, title );

    if( !status.matches( IStatus.ERROR ) )
      saveControlModel( scenarioDataProvider, shell, title );

    return null;
  }


  private StatisticCalculationData initData( final Shell shell, final String title, final IScenarioDataProvider scenarioDataProvider )
  {
    try
    {
      final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.class.getName() );
      final IVectorDataModel vectorModel = scenarioDataProvider.getModel( IVectorDataModel.class.getName() );
      final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class.getName() );
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();

      final StatisticCalculationData data = new StatisticCalculationData( rasterModel, controlModel, vectorModel, scenarioFolder );
      data.init();

      return data;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      StatusDialog.open( shell, e.getStatus(), title );
      return null;
    }
  }

  private boolean askForShape( final StatisticCalculationData data, final Shell shell, final String title )
  {
    /* No dialog if no shape data is available */
    if( !data.hasShapes() )
    {
      final String message = Messages.getString("StatisticAnalysisHandler_0"); //$NON-NLS-1$
      return MessageDialog.openConfirm( shell, title, message );
    }

    final Wizard wizard = new StatisticCalculationShapeWizard(data);
    wizard.setWindowTitle( title );
    final WizardDialog dialog = new WizardDialog(shell, wizard);
    return dialog.open() == Window.OK;
  }

  private void saveControlModel( final IScenarioDataProvider scenarioDataProvider, final Shell shell, final String title )
  {
    try
    {
      scenarioDataProvider.postCommand( IRasterizationControlModel.class.getName(), new EmptyCommand( "Be dirty!", false ) ); //$NON-NLS-1$
      scenarioDataProvider.saveModel( IRasterizationControlModel.class.getName(), new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      StatusDialog.open( shell, e.getStatus(), title );
    }
    catch( final InvocationTargetException e )
    {
      // will never happen
      e.printStackTrace();
    }
  }

}