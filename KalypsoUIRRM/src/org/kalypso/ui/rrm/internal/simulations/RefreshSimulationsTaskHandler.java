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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.ogc.gml.table.ILayerTableInput;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.editor.gistableeditor.command.GmlTableHandlerUtils;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * Create / Updates simulations from the simulations table.
 *
 * @author Gernot Belger
 */
public class RefreshSimulationsTaskHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final String title = HandlerUtils.getCommandName( event );

    final LayerTableViewer tableViewer = GmlTableHandlerUtils.getTableViewerChecked( event );

    final ILayerTableInput input = tableViewer.getInput();
    if( input == null )
      throw new ExecutionException( "Table contains no data" ); //$NON-NLS-1$

    final ISelection selection = HandlerUtil.getCurrentSelection( event );

    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IFolder szenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final IFolder simulationsFolder = szenarioFolder.getFolder( INaProjectConstants.FOLDER_RECHENVARIANTEN );

    final NAControl[] chosenSimulations = chooseSimulations( shell, simulationsFolder, input, selection, title );
    if( chosenSimulations == null || chosenSimulations.length == 0 )
      return null;

    if( !checkSanity( shell, title, chosenSimulations ) )
      return null;

    /* Refresh it */

    final RefreshSimulationsOperation operation = new RefreshSimulationsOperation( simulationsFolder, chosenSimulations );

    final IStatus status = ProgressUtilities.busyCursorWhile( operation, Messages.getString("RefreshSimulationsTaskHandler_0") ); //$NON-NLS-1$
    StatusDialog.open( shell, status, title );

    return null;
  }

  private NAControl[] chooseSimulations( final Shell shell, final IFolder simulationsFolder, final ILayerTableInput input, final ISelection selection, final String title )
  {
    final NAControl[] selectedSimulations = findSimulationsFromSelection( (IStructuredSelection) selection );
    final NAControl[] allSimulations = findAllSimulations( input );

    /* check names (= calc case folder names) for duplicates */
    final String duplicateName = findDuplicates( allSimulations );
    if( duplicateName != null )
    {
      final String message = String.format( Messages.getString("RefreshSimulationsTaskHandler_1"), duplicateName ); //$NON-NLS-1$
      MessageDialog.openWarning( shell, title, message );
      return null;
    }

    // TODO: use special dialog / wizard
    // TODO: show dialog, which ones to refresh + other info (which ones get refreshed, which ones get created etc.)
    final IBaseLabelProvider labelProvider = new GMLLabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        final String text = super.getText( element );

        final IFolder simulationFolder = RefreshSimulationsOperation.createFolder( simulationsFolder, (NAControl) element );
        if( simulationFolder.exists() )
          return String.format( Messages.getString("RefreshSimulationsTaskHandler_2"), text ); //$NON-NLS-1$

        return text;
      }
    };

    final ListSelectionDialog<NAControl> dialog = new ListSelectionDialog<>( shell, Messages.getString("RefreshSimulationsTaskHandler_3"), allSimulations, selectedSimulations, labelProvider, NAControl.class ); //$NON-NLS-1$
    if( dialog.open() != Window.OK )
      return null;

    return dialog.getSelectedElements();
  }

  private NAControl[] findSimulationsFromSelection( final IStructuredSelection selection )
  {
    final Collection<NAControl> simulations = new ArrayList<>( selection.size() );
    for( final Iterator< ? > iterator = selection.iterator(); iterator.hasNext(); )
    {
      final Object element = iterator.next();
      if( element instanceof NAControl )
        simulations.add( (NAControl) element );
    }

    return simulations.toArray( new NAControl[simulations.size()] );
  }

  private NAControl[] findAllSimulations( final ILayerTableInput input )
  {
    final List<Feature> features = input.getFeatures();

    final Collection<NAControl> simulations = new ArrayList<>( features.size() );

    for( final Feature feature : features )
    {
      if( feature instanceof NAControl )
        simulations.add( (NAControl) feature );
    }

    return simulations.toArray( new NAControl[simulations.size()] );
  }

  private String findDuplicates( final NAControl[] simulations )
  {
    final Set<String> allNames = new HashSet<>();

    for( final NAControl simulation : simulations )
    {
      final String name = simulation.getDescription();
      if( allNames.contains( name ) )
        return name;

      allNames.add( name );
    }

    return null;
  }

  // TODO: check sanity of selected simulations -> status of simulation
  private boolean checkSanity( final Shell shell, final String title, final NAControl[] simulations )
  {
    /* check names (= calc case folder names) for empty names */
    if( hasEmptyNames( simulations ) )
    {
      final String message = String.format( Messages.getString("RefreshSimulationsTaskHandler_4") ); //$NON-NLS-1$
      MessageDialog.openWarning( shell, title, message );
      return false;
    }

    return true;
  }

  private boolean hasEmptyNames( final NAControl[] simulations )
  {
    for( final NAControl simulation : simulations )
    {
      if( StringUtils.isEmpty( simulation.getDescription() ) )
        return true;
    }

    return false;
  }
}