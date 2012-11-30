package org.kalypso.model.flood.handlers;

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
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
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;
import de.renew.workflow.connector.cases.ScenarioHandlingProjectNature;
import de.renew.workflow.connector.context.ActiveWorkContext;

public class GenerateRiskModelHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      /* Get context */
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );

      /* Find risk project wizard: serves as test if risk-model is available */
      final IWizardDescriptor wizardDesc = NewWizardRegistry.getInstance().findWizard( "org.kalypso.risk.project.KalypsoRiskProjectWizard" ); //$NON-NLS-1$
      if( wizardDesc == null )
      {
        MessageDialog.openError( shell, Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.1" ), Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }

      /* collect the flood model data, that is needed for Risk Modeller */
      // get the data provider
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IFolder floodModelScenarioFolder = (IFolder) dataProvider.getScenarioFolder().findMember( "/models/" ); //$NON-NLS-1$

      // get the flood model
      final IFloodModel model = dataProvider.getModel( IFloodModel.class.getName() );

      // get all events
      final IFeatureBindingCollection<IRunoffEvent> events = model.getEvents();

      // ask user which events to process
      final IRunoffEvent[] selectedEvents = FloodModelHelper.askUserForEvents( shell, events );

      if( selectedEvents == null || selectedEvents.length == 0 )
        return null;

      final IRunoffEvent[] eventsToProcess = checkEvents( selectedEvents, shell );
      if( eventsToProcess.length == 0 )
      {
        MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.4" ), Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }

      /* Copy event data into non-flood-modell dependend arrays */
      final String[] eventNames = new String[eventsToProcess.length];
      final String[] eventDescriptions = new String[eventsToProcess.length];
      final Integer[] eventPeriods = new Integer[eventsToProcess.length];
      final ICoverageCollection[] eventGrids = new ICoverageCollection[eventsToProcess.length];
      for( int i = 0; i < eventsToProcess.length; i++ )
      {
        eventNames[i] = eventsToProcess[i].getName();
        final String importText = Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.3", floodModelScenarioFolder.getProject().getName() ); //$NON-NLS-1$
        eventDescriptions[i] = String.format( "%s (%s)", eventsToProcess[i].getDescription(), importText ); //$NON-NLS-1$
        eventPeriods[i] = eventsToProcess[i].getReturnPeriod();
        eventGrids[i] = eventsToProcess[i].getResultCoverages();
      }

      /* Create Risk Project: show project new dialog */
      final IAction action = new NewWizardShortcutAction( workbenchWindow, wizardDesc );
      action.run();

      /* Check if project creation succeeded */
      final ActiveWorkContext activeWorkContext = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext();
      final ScenarioHandlingProjectNature nature = activeWorkContext.getCurrentProject();
      if( !nature.getProject().hasNature( "org.kalypso.risk.project.KalypsoRiskProjectNature" ) ) //$NON-NLS-1$
      {
        // we simply return, because that means no new project was created (maybe user cancelled the dialog)
        return null;
      }

      /* Now we can import the flood-depth grids */
      final ICoreRunnableWithProgress importOperation = new ICoreRunnableWithProgress()
      {
        @Override
        public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
        {
          try
          {
            // Thread.sleep( 10000 );

            Assert.isNotNull( floodModelScenarioFolder );

            // Delegate importing the grids to Raster-Code; it knows best what to do with it
            RiskModelHelper.importEvents( eventNames, eventDescriptions, eventPeriods, eventGrids, monitor );
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
      final IStatus result = ProgressUtilities.busyCursorWhile( importOperation, Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.0" ) ); //$NON-NLS-1$
      if( !result.isOK() )
      {
        // final String msg = "Failed to create Risk Model, please try again.";
        final String title = Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.4" ); //$NON-NLS-1$
        StatusDialog.open( shell, result, title );
      }

    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      final String msg = e.getStatus().getMessage();
      throw new ExecutionException( msg, e );
    }
    return null;

  }

  /**
   * checks, if there are events selected which have no result coverages and filters them
   */
  private IRunoffEvent[] checkEvents( final IRunoffEvent[] selectedEvents, final Shell shell )
  {
    // decision dialog for user, if he wants to overwrite existing data
    final List<IRunoffEvent> eventList = new LinkedList<>();

    for( final IRunoffEvent runoffEvent : selectedEvents )
    {
      final ICoverageCollection resultCoverages = runoffEvent.getResultCoverages();
      final IFeatureBindingCollection<ICoverage> resultCoveragesList = resultCoverages.getCoverages();
      if( resultCoveragesList.size() == 0 )
      {
        MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.12" ), Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.13" ) + runoffEvent.getName() + Messages.getString( "org.kalypso.model.flood.handlers.GenerateRiskModelHandler.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      else
        eventList.add( runoffEvent );

    }
    return eventList.toArray( new IRunoffEvent[eventList.size()] );
  }
}