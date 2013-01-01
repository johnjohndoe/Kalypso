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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
public class GenerateResultDifferenceViewHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext)event.getApplicationContext();
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow)context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final MapView mapView = (MapView)window.getActivePage().findView( MapView.ID );

    final IMapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.GenerateResultDifferenceViewHandler.0" ), Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.GenerateResultDifferenceViewHandler.1" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    try
    {
      final IScenarioResultMeta resultModel = modelProvider.getModel( IScenarioResultMeta.class.getName() );

      // open wizard
      final GenerateDifferenceResultTinWizard wizard = new GenerateDifferenceResultTinWizard( scenarioFolder, resultModel, modelProvider );
      final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, wizard );
      wizardDialog2.setRememberSize( true );
      if( wizardDialog2.open() == Window.OK )
      {
        modelProvider.saveModel( IScenarioResultMeta.class.getName(), new NullProgressMonitor() );

        return Status.OK_STATUS;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.GenerateResultDifferenceViewHandler.2" ) ); //$NON-NLS-1$
    }

    return Status.CANCEL_STATUS;
  }
}
