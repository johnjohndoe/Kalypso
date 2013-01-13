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
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.MessageConstants;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenOutputZipAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenStatusLogAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenTextLogAction;
import org.kalypso.ui.rrm.internal.simulations.jobs.ValidateSimulationJob;
import org.kalypso.util.command.WaitForFeatureChanges;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.base.ITask;
import de.renew.workflow.connector.cases.IScenarioDataProvider;
import de.renew.workflow.connector.worklist.ITaskExecutionAuthority;
import de.renew.workflow.connector.worklist.ITaskExecutor;

/**
 * The simulation calculation feature control.
 * 
 * @author Holger Albert
 */
public class SimulationCalculationFeatureControl extends AbstractFeatureControl
{
  /**
   * The validation status composite.
   */
  protected StatusComposite m_validationStatusComposite;

  /**
   * The actions for opening a file.
   */
  private final List<IAction> m_actions = new ArrayList<>();

  /**
   * The validate simulation job.
   */
  private ValidateSimulationJob m_validationJob;

  private OpenStatusLogAction m_simulationAction;

  public SimulationCalculationFeatureControl( final IPropertyType ftp )
  {
    super( ftp );
  }

  public SimulationCalculationFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
  }

  @Override
  protected Control createControl( final Composite parent, final int style )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, style );
    GridLayoutFactory.swtDefaults().applyTo( main );

    try
    {
      /* Validation group */
      final Group validationGroup = new Group( main, SWT.NONE );
      GridLayoutFactory.swtDefaults().applyTo( validationGroup );
      validationGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      validationGroup.setText( Messages.getString( "SimulationCalculationFeatureControl.14" ) ); //$NON-NLS-1$

      /* Calculation validation status */
      m_validationStatusComposite = new StatusComposite( validationGroup, StatusComposite.DETAILS );
      m_validationStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      m_validationStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "SimulationCalculationFeatureControl.3" ) ) ); //$NON-NLS-1$

      /* Create a group. */
      final Group resultsGroup = new Group( main, SWT.NONE );
      GridLayoutFactory.swtDefaults().applyTo( resultsGroup );
      resultsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      resultsGroup.setText( Messages.getString( "SimulationCalculationFeatureControl.2" ) ); //$NON-NLS-1$

      /* Get the current simulation. */
      final RrmSimulation simulation = getSimulation();

      final RrmCalculationResult current = simulation.getCurrentCalculationResult();

      m_simulationAction = new OpenStatusLogAction( MessageConstants.STR_ACTION_OPEN_CALC_STATUS_TEXT, MessageConstants.STR_ACTION_OPEN_CALC_STATUS_TOOLTIP, current.getCalculationStatusGml() );
      addAction( resultsGroup, m_simulationAction );

      /* Create a empty label. */
      final Label emptyLabel2 = new Label( resultsGroup, SWT.NONE );
      emptyLabel2.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

      addAction( resultsGroup, new OpenOutputZipAction( MessageConstants.STR_ACTION_OPEN_CALC_LOG_TEXT, MessageConstants.STR_ACTION_OPEN_CALC_LOG_TOOLTIP, current.getOutputZip(), false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      addAction( resultsGroup, new OpenOutputZipAction( MessageConstants.STR_ACTION_OPEN_ERROR_LOG_TEXT, MessageConstants.STR_ACTION_OPEN_ERROR_LOG_TOOLTIP, current.getOutputZip(), true ) ); //$NON-NLS-1$ //$NON-NLS-2$
      // addAction( resultsGroup, new OpenOutputZipAction( "Output log (calculation core)", "Displays the output log.",
      // simulation, false ) );
      addAction( resultsGroup, new OpenTextLogAction( MessageConstants.STR_ACTION_OPEN_MASS_BALANCE_TEXT, MessageConstants.STR_ACTION_OPEN_MASS_BALANCE_TOOLTIP, current.getBilanzTxt() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      addAction( resultsGroup, new OpenTextLogAction( MessageConstants.STR_ACTION_OPEN_STATISTICS_TEXT, MessageConstants.STR_ACTION_OPEN_STATISTICS_TOOLTIP, current.getStatisticsCsv(), "xls" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-1$

      /* Calculation button */
      final Label emptyLabel3 = new Label( main, SWT.NONE );
      emptyLabel3.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

      final Button calculationButton = new Button( main, SWT.PUSH );
      calculationButton.setLayoutData( new GridData( SWT.END, SWT.END, true, false ) );
      calculationButton.setText( Messages.getString( "SimulationCalculationFeatureControl.12" ) ); //$NON-NLS-1$
      calculationButton.setImage( KalypsoUIRRMPlugin.getDefault().getImageProvider().getImage( UIRrmImages.DESCRIPTORS.SIMULATION ) );
      calculationButton.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          handleCalculatePressed();
        }
      } );

      updateData();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();

      final Label errorLabel = new Label( main, SWT.WRAP );
      errorLabel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      errorLabel.setText( ex.getLocalizedMessage() );
    }

    return main;
  }

  private void addAction( final Composite parent, final IAction action )
  {
    m_actions.add( action );

    final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, parent, SWT.NONE, action );
    imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    imageHyperlink.setText( action.getText() );
  }

  @Override
  public void updateControl( )
  {
  }

  public void updateData( )
  {
    /* Update the controls. */
    initialize( getSimulation() );

    /* Update the actions. */
    for( final IAction action : m_actions )
    {
      if( action instanceof IUpdateable )
        ((IUpdateable)action).update();
    }
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }

  private RrmSimulation getSimulation( )
  {
    /* Get the description of the current simulation. */
    final Feature feature = getFeature();
    final String description = feature.getDescription();

    /* Get the folder of the current simulation. */
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final IContainer scenarioFolder = dataProvider.getScenarioFolder();
    final IFolder simulationsFolder = scenarioFolder.getFolder( new Path( RrmScenario.FOLDER_SIMULATIONEN ) );
    final IFolder simulationFolder = simulationsFolder.getFolder( description );

    return new RrmSimulation( simulationFolder );
  }

  private void initialize( final RrmSimulation simulation )
  {
    if( m_validationJob != null )
      return;

    m_simulationAction.updateStatus();

    m_validationJob = new ValidateSimulationJob( simulation, (NAControl)getFeature() );
    m_validationJob.setUser( false );
    m_validationJob.addJobChangeListener( new JobChangeAdapter()
    {
      @Override
      public void done( final IJobChangeEvent event )
      {
        final Job job = event.getJob();
        if( !(job instanceof ValidateSimulationJob) )
          return;

        final IStatus result = job.getResult();
        if( !result.isOK() )
        {
          setValidationStatus( result );
          return;
        }

        final ValidateSimulationJob task = (ValidateSimulationJob)job;
        final IStatus validationStatus = task.getValidationStatus();
        setValidationStatus( validationStatus );
      }
    } );

    /* Schedule the jobs. */
    m_validationJob.schedule();
  }

  private NAControl[] findAllSimulations( )
  {
    final Collection<NAControl> results = new ArrayList<>();

    final NAControl naControl = (NAControl)getFeature();
    final SimulationCollection owner = (SimulationCollection)naControl.getOwner();

    final IFeatureBindingCollection<NAControl> simulations = owner.getSimulations();
    for( final NAControl simulation : simulations )
      results.add( simulation );

    return results.toArray( new NAControl[results.size()] );
  }

  protected void handleCalculatePressed( )
  {
    if( m_validationStatusComposite == null || m_validationStatusComposite.isDisposed() )
      return;

    /* Get the shell and the title. */
    final Shell shell = m_validationStatusComposite.getShell();
    final String title = Messages.getString( "SimulationCalculationFeatureControl.13" ); //$NON-NLS-1$

    /* Get the na control. */
    final NAControl naControl = (NAControl)getFeature();
    final NAControl[] simulations = new NAControl[] { naControl };

    /* Create the simulation handler. */
    final SimulationHandler handler = new SimulationHandler();

    /* Check for duplicates. */
    final NAControl[] allSimulations = findAllSimulations();
    final String duplicateName = handler.findDuplicates( allSimulations );
    if( duplicateName != null )
    {
      final String message = String.format( Messages.getString( "RefreshSimulationsTaskHandler_0" ), duplicateName ); //$NON-NLS-1$
      MessageDialog.openWarning( shell, title, message );
      return;
    }

    /* Check the sanity of the simulations. */
    final IStatus status = handler.checkSanity( simulations );
    if( !status.isOK() )
    {
      final String message = status.getMessage();
      MessageDialog.openWarning( shell, title, message );
      return;
    }

    /* Must wait for eventually done changes to the feature. */
    final ICoreRunnableWithProgress commandWaiter = new WaitForFeatureChanges();
    ProgressUtilities.busyCursorWhile( commandWaiter );

    /* Ask the user to save. */
    final ITaskExecutionAuthority executionAuthority = KalypsoAFGUIFrameworkPlugin.getTaskExecutionAuthority();
    final ITaskExecutor taskExecutor = KalypsoAFGUIFrameworkPlugin.getTaskExecutor();
    final ITask task = taskExecutor.getActiveTask();
    if( !executionAuthority.canStopTask( task ) )
      return;

    /* Calculate the simulations. */
    handler.calculateSimulation( shell, simulations );

    /* Update the control. */
    updateData();
  }

  protected void setValidationStatus( final IStatus validationStatus )
  {
    if( m_validationStatusComposite == null || m_validationStatusComposite.isDisposed() )
      return;

    m_validationStatusComposite.getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        m_validationStatusComposite.setStatus( validationStatus );
      }
    } );

    m_validationJob = null;
  }
}