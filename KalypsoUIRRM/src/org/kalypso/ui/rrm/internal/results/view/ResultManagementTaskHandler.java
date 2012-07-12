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
package org.kalypso.ui.rrm.internal.results.view;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ui.rrm.internal.diagram.RrmDiagramView;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreePropertiesView;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class ResultManagementTaskHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final IWorkbenchPage page = window.getActivePage();
    final ResultManagementView managementView = (ResultManagementView) page.findView( ResultManagementView.ID );
    if( managementView == null )
      throw new ExecutionException( "Failed to access timeseries view" ); //$NON-NLS-1$

    final ISelectionProvider selectionProvider = managementView.getViewSite().getSelectionProvider();

    try
    {
      /* Hook properties view and management view */
      final TreePropertiesView propertiesView = (TreePropertiesView) page.showView( TreePropertiesView.ID );
      if( propertiesView == null )
        throw new ExecutionException( "Failed to access properties view" ); //$NON-NLS-1$

      propertiesView.hookSelection( selectionProvider );

      /* hook rrm diagram view */
      final IViewPart diagramView = page.showView( RrmDiagramView.ID );
      if( !(diagramView instanceof RrmDiagramView) )
        throw new ExecutionException( "Failed to access diagram view" ); //$NON-NLS-1$

      final RrmDiagramView rrmDiagramView = (RrmDiagramView) diagramView;
      rrmDiagramView.hookSelection( selectionProvider );
      rrmDiagramView.setSelectionFilter( managementView.getFilterControl() );
      rrmDiagramView.setSelectionTraverseLevel( 4 );

      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final RrmScenario scenario = new RrmScenario( modelProvider.getScenarioFolder() );

      managementView.setInput( scenario );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      throw new ExecutionException( "Failed to initialize timeseries view", e ); //$NON-NLS-1$
    }

    return null;
  }
}
