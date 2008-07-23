package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;

import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.StreamGobbler;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationSobek2PIWorker implements ISimulation
{

  private final PrintStream m_nofdpStream;

  private final PrintStream m_sobekStream;

  public SimulationSobek2PIWorker( PrintStream nofdpStream, PrintStream sobekStream )
  {
    m_nofdpStream = nofdpStream;
    m_sobekStream = sobekStream;
  }

  public URL getSpezifikation( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    ConsoleHelper.writeLine( m_nofdpStream, String.format( "---> Converting Sobek model into PI Model..." ) );

    /*******************************************************************************************************************
     * PROCESSING
     ******************************************************************************************************************/
    /* The command for execution. */
    final File directory = new File( tmpdir, "Sobek-IDSS/batch" );

    final String[] command = new String[3];
    command[0] = "cmd.exe";
    command[1] = "/C";
    command[2] = "3_sobek2PI.bat";

    /* Execute the process. */

    Process exec;
    try
    {
      exec = Runtime.getRuntime().exec( command, null, directory );
    }
    catch( final IOException e1 )
    {
      e1.printStackTrace();
      throw new SimulationException( e1.getMessage() );
    }

    final InputStream errorStream = exec.getErrorStream();
    final InputStream inputStream = exec.getInputStream();

    final StreamGobbler error = new StreamGobbler( errorStream, "Report: ERROR_STREAM", false, m_sobekStream ); //$NON-NLS-1$
    final StreamGobbler input = new StreamGobbler( inputStream, "Report: INPUT_STREAM", false, m_sobekStream ); //$NON-NLS-1$

    error.start();
    input.start();

    int exitValue = 0;
    int timeRunning = 0;

    /* It is running until the job has finished or the timeout of 5 minutes is reached. */
    while( true )
    {
      try
      {
        exitValue = exec.exitValue();
        break;
      }
      catch( final RuntimeException e )
      {
        /* The process has not finished. */
      }

      /* Wait a few millisec, before continuing. */
      try
      {
        Thread.sleep( 100 );
        timeRunning = timeRunning + 100;
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    ConsoleHelper.writeLine( m_nofdpStream, String.format( "---> Sobek model converted." ) );
    ConsoleHelper.writeLine( m_nofdpStream, "" );

    /* add calculation points conversion log file */
    File logCalculationPoints = new File( tmpdir, ISobekCalculationJobConstants.LOG_SOBEK2PI_POINTS_PATH );
    if( !logCalculationPoints.exists() )
      throw new SimulationException( "Sobek2Pi Calculation Points log file doesn't exists..." );

    resultEater.addResult( ISobekCalculationJobConstants.LOG_SOBEK2PI_POINTS, logCalculationPoints );

    /* add structure nodes conversion log file */
    File logStructureNodes = new File( tmpdir, ISobekCalculationJobConstants.LOG_SOBEK2PI_STRUCTURES_PATH );
    if( !logStructureNodes.exists() )
      throw new SimulationException( "Sobek2Pi Structure Nodes log file doesn't exists..." );

    resultEater.addResult( ISobekCalculationJobConstants.LOG_SOBEK2PI_STRUCTURES, logStructureNodes );

  }
}
