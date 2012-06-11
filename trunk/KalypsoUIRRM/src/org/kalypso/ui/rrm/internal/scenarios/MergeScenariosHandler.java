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
package org.kalypso.ui.rrm.internal.scenarios;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.util.command.WaitForFeatureChanges;

import de.renew.workflow.base.ITask;
import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioDataProvider;
import de.renew.workflow.connector.worklist.ITaskExecutionAuthority;
import de.renew.workflow.connector.worklist.ITaskExecutor;

/**
 * This handler starts the wizard for merging scenarios in the current one.
 * 
 * @author Holger Albert
 */
public class MergeScenariosHandler extends AbstractHandler
{
  /**
   * The constructor.
   */
  public MergeScenariosHandler( )
  {
  }

  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )
  {
    /* Must wait for eventually done changes to the feature. */
    final ICoreRunnableWithProgress commandWaiter = new WaitForFeatureChanges();
    ProgressUtilities.busyCursorWhile( commandWaiter );

    /* Ask the user to save. */
    final ITaskExecutionAuthority executionAuthority = KalypsoAFGUIFrameworkPlugin.getTaskExecutionAuthority();
    final ITaskExecutor taskExecutor = KalypsoAFGUIFrameworkPlugin.getTaskExecutor();
    final ITask task = taskExecutor.getActiveTask();
    if( !executionAuthority.canStopTask( task ) )
      return null;

    /* Get the shell. */
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Shell shell = display.getActiveShell();

    /* Get the current scenario. */
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final IScenario scenario = dataProvider.getScenario();

    /* Create the wizard. */
    final MergeScenariosWizard wizard = new MergeScenariosWizard( scenario );

    /* Create the dialog. */
    final WizardDialog dialog = new WizardDialog( shell, wizard );

    /* Open the dialog. */
    if( dialog.open() != Window.OK )
      return null;

    // TODO

    return null;
  }
}