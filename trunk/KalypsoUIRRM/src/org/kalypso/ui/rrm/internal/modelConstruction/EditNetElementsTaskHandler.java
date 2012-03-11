/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.featureview.views.FeatureView;
import org.kalypso.model.hydrology.project.ScenarioAccessor;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.editor.gistableeditor.GttViewPart;
import org.kalypso.ui.views.map.MapView;

/**
 * @author Gernot Belger
 */
public class EditNetElementsTaskHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window.getActivePage();
    final MapView mapView = (MapView) activePage.findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( "Unable to find map view" ); //$NON-NLS-1$

    final IMapPanel mapPanel = mapView.getMapPanel();

    /* Make sure, a theme is active */
    /* wait for map to load */
    final String windowTitle = HandlerUtils.getCommandName( event );
    // FIXME: before I18N, check all other calls to waitForAndErrorDialog and remove duplicate strings
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, windowTitle, "Loading map..." ) )
      return null;

    configureFeatureView( activePage );

    /* set input to gtt tables */
    try
    {
      final SzenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final IFolder scenarioFolder = (IFolder) dataProvider.getScenarioFolder();
      final ScenarioAccessor scenario = new ScenarioAccessor( scenarioFolder );

      setGttInput( activePage, "Nodes", scenario.getNodesNetGtt(), "Nodes" ); //$NON-NLS-1$
      setGttInput( activePage, "Reaches", scenario.getReachesNetGtt(), "Reaches" ); //$NON-NLS-1$
      setGttInput( activePage, "Catchments", scenario.getCatchmentsNetGtt(), "Catchments" ); //$NON-NLS-1$
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

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/Node_ModelConstruction.gft" ) ); //$NON-NLS-1$

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/Catchment_ModelConstruction.gft" ) ); //$NON-NLS-1$

    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/KMChannel_ModelConstruction.gft" ) ); //$NON-NLS-1$
    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/RHBChannel_ModelConstruction.gft" ) ); //$NON-NLS-1$
    factory.addView( getClass().getResource( "/org/kalypso/ui/rrm/catalog/resources/VChannel_ModelConstruction.gft" ) ); //$NON-NLS-1$
  }

  private void setGttInput( final IWorkbenchPage activePage, final String secondaryId, final IFile input, final String title )
  {
    final IViewReference viewReference = activePage.findViewReference( GttViewPart.ID, secondaryId );
    final IWorkbenchPart part = viewReference.getPart( false );
    if( part instanceof GttViewPart )
    {
      final GttViewPart gttView = (GttViewPart) part;
      gttView.setInput( new FileEditorInput( input ) );

      gttView.setPartName( title );
      gttView.setTitleToolTip( title );
    }
  }
}