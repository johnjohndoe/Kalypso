package org.kalypso.ui.launcher;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.kalypso.eclipse.jface.operation.IProgressRunnable;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobService;
import org.kalypso.services.calcjob.CalcJobStatus;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class CalcCaseRunnable implements IProgressRunnable
{
  private ILaunchConfiguration m_configuration;

  public CalcCaseRunnable( final ILaunchConfiguration configuration )
  {
    m_configuration = configuration;
  }

  /**
   * @see org.kalypso.eclipse.jface.operation.IProgressRunnable#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus run( final IProgressMonitor monitor )
  {
    String jobID = null;
    try
    {
      monitor.beginTask( "Berechnung durchführen", 200 );

      jobID = startCalculation( new SubProgressMonitor( monitor, 100 ) );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Starten des Rechendienstes", e );
    }

    return waitForJob( new SubProgressMonitor( monitor, 100 ), jobID );
  }

  private IStatus waitForJob( final IProgressMonitor monitor, final String jobID )
  {
    monitor.beginTask( "Berechnung wird durchgeführt", 100 );

    try
    {
      final String folderPath = m_configuration.getAttribute(
          IKalypsoLaunchConfigurationConstants.CALC_PATH, "" );

      final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(
          new Path( folderPath ) );

      final ModelNature nature = (ModelNature)folder.getProject().getNature( ModelNature.ID );

      final CalcJobService calcService = KalypsoGisPlugin.getDefault().getCalcService();

      int lastProgress = 0;

      while( true )
      {
        final CalcJobDescription jobDescription = calcService.getJobDescription( jobID );

        monitor.setTaskName( jobDescription.getDescription() );
        final int progress = jobDescription.getProgress();

        final int amount = progress - lastProgress;

        monitor.worked( amount );

        lastProgress = progress;

        switch( jobDescription.getState() )
        {
        case CalcJobStatus.RUNNING:
        case CalcJobStatus.UNKNOWN:
          break;

        case CalcJobStatus.CANCELED:
          return Status.CANCEL_STATUS;

        case CalcJobStatus.ERROR:
          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, jobDescription
              .getMessage(), null );

        case CalcJobStatus.FINISHED:
        {
          final String[] results = calcService.retrieveResults( jobID );
          // write results to calccase

          nature.putCalcCaseOutputData( folder, results );

          return Status.OK_STATUS;
        }
        }

        Thread.sleep( 100 );

        if( monitor.isCanceled() )
        {
          calcService.cancelJob( jobID );
          calcService.removeJob( jobID );
          return Status.CANCEL_STATUS;
        }
      }
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Ausführen der Berechnung", e );
    }
  }

  private String startCalculation( final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( "Berechnung starten", 1000 );

    try
    {
      final String calcType = m_configuration.getAttribute(
          IKalypsoLaunchConfigurationConstants.CALC_TYPE, "" );
      final String description = m_configuration.getAttribute(
          IKalypsoLaunchConfigurationConstants.CALC_LABEL, "" );
      final String folderPath = m_configuration.getAttribute(
          IKalypsoLaunchConfigurationConstants.CALC_PATH, "" );

      // die Dateien suchen und erzeugen
      final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(
          new Path( folderPath ) );

      final ModelNature nature = (ModelNature)folder.getProject().getNature( ModelNature.ID );
      final String[] input = nature.getCalcCaseInputData( folder, new SubProgressMonitor( monitor,
          500 ) );

      // start job
      final CalcJobService calcService = KalypsoGisPlugin.getDefault().getCalcService();
      final String jobID = calcService.createJob( calcType, description, input );

      monitor.worked( 500 );

      return jobID;
    }
    finally
    {
      monitor.done();
    }
  }
}