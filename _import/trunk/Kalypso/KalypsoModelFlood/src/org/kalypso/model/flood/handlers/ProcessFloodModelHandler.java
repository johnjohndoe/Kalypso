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
package org.kalypso.model.flood.handlers;

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.core.FloodModelProcess;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class ProcessFloodModelHandler extends AbstractHandler implements IHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      /* Get context */
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

      final IFloodModel model = dataProvider.getModel( IFloodModel.class );
      final IFeatureWrapperCollection<IRunoffEvent> events = model.getEvents();

      /* Get the map */
      final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
      final MapView mapView = (MapView) window.getActivePage().findView( MapView.ID );
      if( mapView == null )
        throw new ExecutionException( "Kartenansicht nicht geöffnet." );

      final MapPanel mapPanel = mapView.getMapPanel();

      /* wait for map to load */
      if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "WSP-Anpassen", "Fehler beim Öffnen der Karte" ) )
        return null;

      /* ask user which events to process? */
      final IRunoffEvent[] eventsToProcess = FloodModelHelper.askUserForEvents( shell, events );

      if( eventsToProcess == null )
        return null;

      // decision dialog for user, if he wants to overwrite existing data
      final List<IRunoffEvent> eventListToProcess = new LinkedList<IRunoffEvent>();

      for( int i = 0; i < eventsToProcess.length; i++ )
      {
        final IRunoffEvent runoffEvent = eventsToProcess[i];
        final ICoverageCollection resultCoverages = runoffEvent.getResultCoverages();

        if( resultCoverages.size() != 0 )
        {
          if( MessageDialog.openQuestion( shell, "Fließtiefendaten für Ereignis " + runoffEvent.getName() + "bereits vorhanden", "Sollen vorhandene Daten überschrieben werden?" ) == true )
          {
            // clear existing results (gml and file and themes).
            IStatus status = FloodModelHelper.removeResultCoverages( shell, dataProvider, resultCoverages );
            if( status == Status.OK_STATUS )
              eventListToProcess.add( runoffEvent );
          }
        }
        else
          eventListToProcess.add( runoffEvent );

      }

      // check prerequisites
      // - event has at least 1 tin
      // - at least one grid present

      final IMapModell mapModell = mapPanel.getMapModell();
      final AbstractCascadingLayerTheme wspTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, "Wasserspiegellagen", "waterlevelThemes" );

      final IStatus processResult = runCalculation( model, eventsToProcess, dataProvider, wspTheme );

      if( processResult.isOK() )
        MessageDialog.openInformation( shell, "Flood-Modeller", "Fließtiefen wurden erfolgreich erzeugt." );
      else
        ErrorDialog.openError( shell, "Flood-Modeller", "Fließtiefen erzeugen", processResult );

      if( processResult.isOK() )
      {
        // handle results if job is successful

        // add all themes to map
        for( final IRunoffEvent runoffEvent : eventsToProcess )
        {
          final int index = FloodModelHelper.findWspTheme( runoffEvent, wspTheme );
          FloodModelHelper.addResultTheme( runoffEvent, wspTheme, index );
        }
      }

      return null;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new ExecutionException( e.getLocalizedMessage(), e );
    }
  }

  private IStatus runCalculation( final IFloodModel model, final IRunoffEvent[] eventsToProcess, final SzenarioDataProvider dataProvider, final AbstractCascadingLayerTheme wspTheme )
  {
    if( eventsToProcess.length == 0 )
      return StatusUtilities.createInfoStatus( "Keine Ereignisse prozessiert." );

    // remove themes
    // TODO: only remove event coverages
    FloodModelHelper.removeWspTheme( wspTheme );

    final FloodModelProcess process = new FloodModelProcess( model, eventsToProcess );

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final IStatus result = process.process( monitor );

          // REMARK: post an empty command in order to make the pool dirty, else save does not work.
          ICommand command = new EmptyCommand( "Feature Changed", false );
          dataProvider.postCommand( IFloodModel.class, command );

          dataProvider.saveModel( IFloodModel.class, monitor );

          return result;
        }
        catch( Exception e )
        {
          throw new InvocationTargetException( e );
        }
      }
    };

    return ProgressUtilities.busyCursorWhile( operation, "Fehler beim Berechnen der Fließtiefen" );
  }

}
