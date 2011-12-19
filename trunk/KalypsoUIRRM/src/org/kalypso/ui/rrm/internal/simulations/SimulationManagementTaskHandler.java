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
package org.kalypso.ui.rrm.internal.simulations;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.featureview.views.FeatureView;

/**
 * @author Gernot Belger
 */
public class SimulationManagementTaskHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final IWorkbenchPage page = window.getActivePage();

    final FeatureView featureView = (FeatureView) page.findView( FeatureView.ID );
    if( featureView == null )
      throw new ExecutionException( "Failed to access timeseries view" ); //$NON-NLS-1$

// try
// {
    // featureView.

// final SzenarioDataProvider modelProvider = (SzenarioDataProvider) context.getVariable(
// CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
//
// final CommandableWorkspace workspace = modelProvider.getCommandableWorkSpace(
// IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS );
//
// final ICatchmentModel input = modelProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS,
// ICatchmentModel.class );
//
// // FIXME: check integrity of catchment model against model.gml
//
// managementView.setInput( workspace, input );
// }
// catch( final CoreException e )
// {
// e.printStackTrace();
//      throw new ExecutionException( "Failed to initialize timeseries view", e ); //$NON-NLS-1$
// }

    return null;
  }
}
