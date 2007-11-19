package org.kalypso.model.flood.handlers;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.actions.NewWizardShortcutAction;
import org.eclipse.ui.internal.wizards.NewWizardRegistry;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
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
  @Override
  public Object execute( ExecutionEvent event ) throws ExecutionException
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

      /* collect the flood model data, that is needed for Risk Modeler */
      // get the data provider
      final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

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
      final ActiveWorkContext<Scenario> activeWorkContext = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext();
      CaseHandlingProjectNature nature = activeWorkContext.getCurrentProject();
      if( !nature.getProject().hasNature( "org.kalypso.risk.project.KalypsoRiskProjectNature" ) )
      {
        // we simply return, because that means no new project was created (maybe user cancelled the dialog)
        return null;
      }

      /* The active scenario must have changed to the risk project. We can now acces risk project data. */
      final SzenarioDataProvider riskDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IRasterDataModel rasterDataModel = riskDataProvider.getModel( IRasterDataModel.class );
      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = rasterDataModel.getWaterlevelCoverageCollection();

      // final Map<String, Integer> eventNameToAnnualityMap = new HashMap<String, Integer>();
      // for( final IRunoffEvent runoffEvent : eventsToProcess )
      // eventNameToAnnualityMap.put( runoffEvent.getName(), 0 );
      //      

      /* --- demo code for accessing the depth grid coverage collections --- */

      // get the result coverage collections (depth grids) from the events
      for( final IRunoffEvent runoffEvent : eventsToProcess )
      {
        final IAnnualCoverageCollection annualCoverageCollection = waterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "[" + runoffEvent.getName() + "]" );
        final ICoverageCollection coverages = runoffEvent.getResultCoverages();
        for( final ICoverage coverage : coverages )
          annualCoverageCollection.add( coverage );

        // TODO: dejan, copy/reference coverage into risk model for each event
      }

      /* ------ */

      // TODO: maybe save other models?
      riskDataProvider.saveModel( IRasterDataModel.class, new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      e.printStackTrace();

      throw new ExecutionException( e.getLocalizedMessage(), e );
    }
    return null;

  }

  /**
   * checks, if there are events selected which have no result coverages and filters them
   */
  private IRunoffEvent[] checkEvents( IRunoffEvent[] selectedEvents, final Shell shell )
  {
    // decision dialog for user, if he wants to overwrite existing data
    final List<IRunoffEvent> eventList = new LinkedList<IRunoffEvent>();

    for( int i = 0; i < selectedEvents.length; i++ )
    {
      final IRunoffEvent runoffEvent = selectedEvents[i];
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
