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
    monitor.beginTask( "Berechnung wird durchgeführt", 2000 );

    try
    {
      final String folderPath = m_configuration.getAttribute(
          IKalypsoLaunchConfigurationConstants.CALC_PATH, "" );

      final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(
          new Path( folderPath ) );

      final ModelNature nature = (ModelNature)folder.getProject().getNature( ModelNature.ID );

      int lastProgress = 0;
      while( true )
      {
        final CalcJobDescription jobDescription = nature.checkCalculation( jobID ); 

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
          final String message = jobDescription.getMessage();
          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, message == null ? ""
              : message, null );

        case CalcJobStatus.FINISHED:
        {
          nature.retrieveCalculation( jobID );
          return Status.OK_STATUS;
        }
        }

        Thread.sleep( 100 );

        if( monitor.isCanceled() )
        {
          nature.stopCalculation( jobID );
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
    final String folderPath = m_configuration.getAttribute(
        IKalypsoLaunchConfigurationConstants.CALC_PATH, "" );
    final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(
        new Path( folderPath ) );

    final ModelNature nature = (ModelNature)folder.getProject().getNature( ModelNature.ID );
    return nature.startCalculation( folder, monitor );
  }
}