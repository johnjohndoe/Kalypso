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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.InitThiessenTimeseriesOperation;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public final class ThiessenLinearSumHelper
{
  private ThiessenLinearSumHelper( )
  {
    throw new UnsupportedOperationException();
  }

  public static void showWizard( final Shell shell, final LinearSumBean bean, final ITreeNodeModel model, final String windowTitle )
  {
    try
    {
      /* Init timeseries gml */
      final ICoreRunnableWithProgress operation = new InitThiessenTimeseriesOperation( bean );
      final IStatus initStatus = ProgressUtilities.busyCursorWhile( operation );
      if( !initStatus.isOK() )
      {
        StatusDialog.open( shell, initStatus, windowTitle );
        return;
      }

      final Wizard wizard = new ThiessenGeneratorWizard( bean );
      wizard.setWindowTitle( windowTitle );

      final WizardDialog2 dialog = new WizardDialog2( shell, wizard );
      dialog.setRememberSize( true );
      if( dialog.open() != Window.OK )
        return;

      try
      {
        /* Apply the changes. */
        final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
        final CommandableWorkspace generatorsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS );

        final Feature generator = bean.apply( generatorsWorkspace, (String) bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) );

        /* Refresh the tree. */
        model.refreshTree( generator );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
        StatusDialog.open( shell, status, shell.getText() );
      }
    }
    finally
    {
      /* Delete the generated stations gml. */
      LinearSumHelper.deleteStationsGmlQuietly();
    }
  }
}