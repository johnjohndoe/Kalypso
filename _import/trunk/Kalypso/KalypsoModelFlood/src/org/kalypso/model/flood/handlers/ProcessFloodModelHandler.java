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

import javax.xml.namespace.QName;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gml.ui.map.CoverageManagmentHelper;
import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.core.FloodModelProcess;
import org.kalypso.model.flood.ui.map.operations.AddEventOperation;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
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
      final MapPanel mapPanel = mapView.getMapPanel();
      if( mapView == null )
        throw new ExecutionException( "Kartenansicht nicht geöffnet." );

      /* wait for map to load */
      if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "WSP-Anpassen", "Fehler beim Öffnen der Karte" ) )
        return null;

      /* ask user which events to process? */
      final IRunoffEvent[] eventsToProcess = askUserForEvents( shell, events, dataProvider );

      // check prerequisites
      // - event has at least 1 tin
      // - at least one grid present

      final IMapModell mapModell = mapPanel.getMapModell();
      final AbstractCascadingLayerTheme wspTheme = AddEventOperation.findWspTheme( mapModell );
      final IStatus processResult = runCalculation( model, eventsToProcess, dataProvider, wspTheme );
      ErrorDialog.openError( shell, "Flood-Modeller", "Fließtiefen erzeugen", processResult );
      if( processResult.isOK() )
      {
        // handle results if job is successful

        // add all themes to map
        for( IRunoffEvent runoffEvent : eventsToProcess )
        {
          final int index = findWspTheme( runoffEvent, wspTheme );
          addResultTheme( runoffEvent, wspTheme, index );
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

  private void addResultTheme( IRunoffEvent event, AbstractCascadingLayerTheme wspTheme, int index ) throws Exception
  {
    final StyledLayerType wspLayer = new StyledLayerType();

    wspLayer.setName( "Fliesstiefen (" + event.getName() + ")" );
    wspLayer.setFeaturePath( "#fid#" + event.getWrappedFeature().getId() + "/" + IRunoffEvent.QNAME_PROP_RESULT_COVERAGES.getLocalPart() );
    wspLayer.setLinktype( "gml" );
    wspLayer.setType( "simple" );
    wspLayer.setVisible( true );
    wspLayer.setActuate( "onRequest" );
    wspLayer.setHref( "../models/flood.gml" );
    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" );

    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
    layerPropertyThemeInfoId.setValue( CoverageThemeInfo.class.getName() + "?format=Fliesstiefen (" + event.getName() + ") %.2f NN+m" );

    final List<Property> layerPropertyList = wspLayer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );

    final List<Style> styleList = wspLayer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" );
    style.setStyle( "waterdepthUserStyle" );
    style.setActuate( "onRequest" );
    style.setHref( "../maps/result.sld" );
    style.setType( "simple" );
    styleList.add( style );

    wspTheme.insertLayer( wspLayer, index );
  }

  private int findWspTheme( IRunoffEvent runoffEvent, AbstractCascadingLayerTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        if( featureList.getParentFeatureTypeProperty().getQName() == IRunoffEvent.QNAME_PROP_TIN_MEMBER )
        {
          final Feature parentFeature = featureList.getParentFeature();
          if( parentFeature.getId().equals( runoffEvent.getWrappedFeature().getId() ) )
            return i;
        }
      }
    }

    return -1;
  }

  private IRunoffEvent[] askUserForEvents( final Shell shell, final IFeatureWrapperCollection<IRunoffEvent> events, final SzenarioDataProvider dataProvider )
  {
    final LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        final IRunoffEvent event = (IRunoffEvent) element;
        ICoverageCollection resultCoverages = event.getResultCoverages();
        if( resultCoverages.size() > 0 )
        {
          return event.getName() + " (Ergebnisse vorhanden)";
        }
        else
          return event.getName();
      }
    };

    final ListSelectionDialog dialog = new ListSelectionDialog( shell, events, new ArrayContentProvider(), labelProvider, "Welche Ereignisse sollen verarbeitet werden?" );
    dialog.setTitle( "Flood-Modeller" );

    if( dialog.open() != Window.OK )
      return null;

    Object[] eventsToProcess = dialog.getResult();

    final List<IRunoffEvent> eventListToProcess = new LinkedList<IRunoffEvent>();

    // decision dialog for user, if he wants to overwrite existing data
    for( int i = 0; i < eventsToProcess.length; i++ )
    {
      final IRunoffEvent event = (IRunoffEvent) eventsToProcess[i];
      final ICoverageCollection resultCoverages = event.getResultCoverages();

      if( resultCoverages.size() != 0 )
      {
        if( MessageDialog.openQuestion( shell, "Fließtiefendaten für Ereignis " + event.getName() + "bereits vorhanden", "Sollen vorhandene Daten überschrieben werden?" ) == true )
        {
          // clear existing results (gml and file and themes).
          IStatus status = removeResultCoverages( shell, dataProvider, resultCoverages );
          if( status == Status.OK_STATUS )
            eventListToProcess.add( event );
        }
        else
          return null;
      }
      else
        eventListToProcess.add( event );

    }

    return eventListToProcess.toArray( new IRunoffEvent[eventListToProcess.size()] );
  }

  private IStatus runCalculation( final IFloodModel model, final IRunoffEvent[] eventsToProcess, final SzenarioDataProvider dataProvider, final AbstractCascadingLayerTheme wspTheme )
  {
    if( eventsToProcess.length == 0 )
      return StatusUtilities.createInfoStatus( "Keine Ereignisse prozessiert." );

    // remove themes

    removeWspTheme( wspTheme );

    final FloodModelProcess process = new FloodModelProcess( model, eventsToProcess );

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException, InterruptedException, InvocationTargetException
      {
        final IStatus result = process.process( monitor );

        dataProvider.saveModel( IFloodModel.class, monitor );

        return result;
      }
    };

    return ProgressUtilities.busyCursorWhile( operation, "Fehler beim Berechnen der Fließtiefen" );
  }

  private void removeWspTheme( final AbstractCascadingLayerTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        final QName name = featureList.getParentFeatureTypeProperty().getQName();
        if( name.equals( IRunoffEvent.QNAME_PROP_RESULT_COVERAGES ) )
        {
          wspTheme.removeTheme( theme );
        }
      }
    }
  }

  /**
   * removes the specified coverage file
   */
  private static IStatus removeResultCoverages( final Shell shell, final SzenarioDataProvider dataProvider, final ICoverageCollection resultCoverages )
  {
    final ICoverage[] coverages = resultCoverages.toArray( new ICoverage[resultCoverages.size()] );
    try
    {
      final CommandableWorkspace workspace = dataProvider.getCommandableWorkSpace( IFloodModel.class );

      for( ICoverage coverageToDelete : coverages )
      {
        /* Delete underlying grid grid file */
        final IStatus status = CoverageManagmentHelper.deleteGridFile( coverageToDelete );
        ErrorDialog.openError( shell, "Löschen von Raster-Daten fehlgeschlagen", "Rasterdatei (" + coverageToDelete.getName() + ") konnte nicht gelöscht werden.", status );

        if( status == Status.OK_STATUS )
        {
          /* Delete coverage from collection */
          final Feature parentFeature = resultCoverages.getWrappedFeature();
          final IRelationType pt = (IRelationType) parentFeature.getFeatureType().getProperty( ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER );
          final Feature coverageFeature = coverageToDelete.getWrappedFeature();

          final DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parentFeature, pt, coverageFeature );
          workspace.postCommand( command );

          /* save the model */
          // TODO: use a flag if the model should be getting save
          dataProvider.saveModel( IFloodModel.class, new NullProgressMonitor() );
        }
        return status;
      }
      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      return StatusUtilities.statusFromThrowable( e, "Löschen von Raster-Daten fehlgeschlagen" );
    }
  }
}
