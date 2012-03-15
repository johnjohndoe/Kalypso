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
package org.kalypso.ui.rrm.internal.modelConstruction;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.featureview.views.FeatureView;
import org.kalypso.model.hydrology.project.ScenarioAccessor;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ui.rrm.internal.utils.WorkflowHandlerUtils;

/**
 * @author Gernot Belger
 */
public class CompleteNetElementsTaskHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window.getActivePage();

    configureFeatureView( activePage );

    /* set input to gtt tables */
    try
    {
      final SzenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final IFolder scenarioFolder = (IFolder) dataProvider.getScenarioFolder();
      final ScenarioAccessor scenario = new ScenarioAccessor( scenarioFolder );

      WorkflowHandlerUtils.setGttInput( activePage, "Nodes", scenario.getNodesCompleteGtt(), "Nodes" ); //$NON-NLS-1$
      WorkflowHandlerUtils.setGttInput( activePage, "Channels", scenario.getReachesCompleteGtt(), "Channels" ); //$NON-NLS-1$
      WorkflowHandlerUtils.setGttInput( activePage, "Catchments", scenario.getCatchmentsCompleteGtt(), "Catchments" ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      throw new ExecutionException( "Failed ot initialize tables", e ); //$NON-NLS-1$
    }

    return null;
  }

  /** Configure feature view with specialized templates */
  private void configureFeatureView( final IWorkbenchPage page )
  {
    final IViewPart part = page.findView( FeatureView.ID );
    if( !(part instanceof FeatureView) )
      return;

    final FeatureView featureView = (FeatureView) part;
    final CachedFeatureviewFactory factory = featureView.getCachedFeatureViewFactory();

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/Node_ModelCompletion.gft" ) ); //$NON-NLS-1$

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/Subcatchment_ModelCompletion.gft" ) ); //$NON-NLS-1$

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/KMChannel_ModelCompletion.gft" ) ); //$NON-NLS-1$
    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/RHBChannel_ModelCompletion.gft" ) ); //$NON-NLS-1$
    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/VChannel_ModelCompletion.gft" ) ); //$NON-NLS-1$
  }
}