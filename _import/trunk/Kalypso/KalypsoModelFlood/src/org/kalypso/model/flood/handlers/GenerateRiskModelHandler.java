package org.kalypso.model.flood.handlers;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.actions.NewWizardShortcutAction;
import org.eclipse.ui.internal.wizards.NewWizardRegistry;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class GenerateRiskModelHandler extends AbstractHandler implements IHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      /* Get context */
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );

      /* Find risk project wizard: serves as test if risk-model is available */
      final IWizardDescriptor wizardDesc = NewWizardRegistry.getInstance().findWizard( "org.kalypso.risk.project.KalypsoRiskProjectWizard" );
      if( wizardDesc == null )
      {
        MessageDialog.openError( shell, "Risk Modell erzeugen", "Risk Modell Plug-Ins nicht verfügbar, es kann kein Risk Modell Projekt erzeugt werden." );
        return null;
      }

      /* collect the flood model data, that is needed for Risk Modeller */
      // get the data provider
      final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IFolder floodModelScenarioFolder = (IFolder) dataProvider.getScenarioFolder().findMember( "/models/" );

      // get the flood model
      final IFloodModel model = dataProvider.getModel( IFloodModel.class );

      // get all events
      final IFeatureWrapperCollection<IRunoffEvent> events = model.getEvents();

      // ask user which events to process
      final IRunoffEvent[] selectedEvents = FloodModelHelper.askUserForEvents( shell, events );

      if( selectedEvents == null || selectedEvents.length == 0 )
        return null;

      final IRunoffEvent[] eventsToProcess = checkEvents( selectedEvents, shell );
      if( eventsToProcess.length == 0 )
      {
        MessageDialog.openInformation( shell, "Risk Modell erzeugen", "Sie haben keine Ereignisse mit Ergebnissen ausgewählt. Vorgang wird abgebrochen." );
        return null;
      }

      /* Create Risk Projekt: show project new dialog */
      final IAction action = new NewWizardShortcutAction( workbenchWindow, wizardDesc );
      action.run();

      /* Check if project creation succeeded */
      final ActiveWorkContext<IScenario> activeWorkContext = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext();
      final CaseHandlingProjectNature nature = activeWorkContext.getCurrentProject();
      if( !nature.getProject().hasNature( "org.kalypso.risk.project.KalypsoRiskProjectNature" ) )
      {
        // we simply return, because that means no new project was created (maybe user cancelled the dialog)
        return null;
      }

      /* Now we can import the flodd-depth grids */
      final ICoreRunnableWithProgress importOperation = new ICoreRunnableWithProgress()
      {
        @Override
        public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
        {
          try
          {
            importEvents( floodModelScenarioFolder, eventsToProcess );
          }
          catch( final CoreException e )
          {
            throw e;
          }
          catch( final InvocationTargetException e )
          {
            throw e;
          }
          catch( final Exception e )
          {
            throw new InvocationTargetException( e );
          }
          return Status.OK_STATUS;
        }
      };
      ProgressUtilities.busyCursorWhile( importOperation, "Failed to import flood depth data into new project" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new ExecutionException( e.getLocalizedMessage(), e );
    }
    return null;

  }

  protected void importEvents( final IFolder floodModelScenarioFolder, final IRunoffEvent[] eventsToProcess ) throws CoreException, Exception, InvocationTargetException
  {
    /* The active scenario must have changed to the risk project. We can now access risk project data. */
    final SzenarioDataProvider riskDataProvider = ScenarioHelper.getScenarioDataProvider();
    final IRasterDataModel rasterDataModel = riskDataProvider.getModel( IRasterDataModel.class );
    final IFeatureWrapperCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = rasterDataModel.getWaterlevelCoverageCollection();

    /* --- demo code for accessing the depth grid coverage collections --- */
    final IContainer scenarioFolder = riskDataProvider.getScenarioFolder();
    final String rasterFolderPath = "raster/input/";
    final IFolder rasterFolder = (IFolder) scenarioFolder.findMember( "models/raster/input" );

    Assert.isNotNull( floodModelScenarioFolder );
    Assert.isNotNull( rasterFolder );

    // get the result coverage collections (depth grids) from the events
    final List<Feature> createdFeatures = new ArrayList<Feature>();
    for( final IRunoffEvent runoffEvent : eventsToProcess )
    {
      final IAnnualCoverageCollection annualCoverageCollection = waterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
      annualCoverageCollection.setName( runoffEvent.getName() );
      createdFeatures.add( annualCoverageCollection.getFeature() );

      final ICoverageCollection coverages = runoffEvent.getResultCoverages();
      int coverageCount = 0;
      for( final ICoverage coverage : coverages )
      {
        final String targetFileName = String.format( "grid_%d_%d.ascbin", annualCoverageCollection.getReturnPeriod(), coverageCount );
        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

        final String targetGridPath = rasterFolderPath + targetFileName;
        final File targetFile = new File( rasterFolder.getLocation().toFile(), targetFileName );

        final ICoverage newCoverage = GeoGridUtilities.addCoverage( annualCoverageCollection, grid, targetFile, targetGridPath, "image/bin", new NullProgressMonitor() );
        newCoverage.setName( "" + coverageCount );
        newCoverage.setDescription( String.format( "Imported from KalypsoFlood" ) );
        grid.dispose();

        coverageCount++;
      }
    }

    /* ------ */
    // TODO: maybe save other models?
    final GMLWorkspace workspace = rasterDataModel.getFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, waterlevelCoverageCollection.getFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    riskDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) );
    riskDataProvider.saveModel( IRasterDataModel.class, new NullProgressMonitor() );
  }

  /**
   * checks, if there are events selected which have no result coverages and filters them
   */
  private IRunoffEvent[] checkEvents( final IRunoffEvent[] selectedEvents, final Shell shell )
  {
    // decision dialog for user, if he wants to overwrite existing data
    final List<IRunoffEvent> eventList = new LinkedList<IRunoffEvent>();

    for( final IRunoffEvent runoffEvent : selectedEvents )
    {
      final ICoverageCollection resultCoverages = runoffEvent.getResultCoverages();

      if( resultCoverages.size() == 0 )
      {
        MessageDialog.openInformation( shell, "Risk Model erzeugen", "Keine Fließtiefendaten für Ereignis " + runoffEvent.getName() + " vorhanden. Ereignis wird nicht berücksichtigt." );
      }
      else
        eventList.add( runoffEvent );

    }
    return eventList.toArray( new IRunoffEvent[eventList.size()] );
  }
}
