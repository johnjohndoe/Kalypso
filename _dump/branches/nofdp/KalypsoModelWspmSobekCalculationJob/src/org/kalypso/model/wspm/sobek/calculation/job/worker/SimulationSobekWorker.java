package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.kalypso.contribs.java.io.StreamGobbler;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationSobekWorker implements ISimulation
{

  public URL getSpezifikation( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    /*******************************************************************************************************************
     * PROCESSING
     ******************************************************************************************************************/
    /* The command for execution. */
    final File directory = new File( tmpdir, "Sobek-IDSS/batch" );

    final String[] command = new String[3];
    command[0] = "cmd.exe";
    command[1] = "/C";
    command[2] = "runOpenMI.cmd";

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

    final StreamGobbler error = new StreamGobbler( errorStream, "Report: ERROR_STREAM", true ); //$NON-NLS-1$
    final StreamGobbler input = new StreamGobbler( inputStream, "Report: INPUT_STREAM", true ); //$NON-NLS-1$

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

      // TODO adjust timeout
      if( timeRunning >= 300000 )
      {
        exec.destroy();
        throw new SimulationException( "timeout" );
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

      // TODO check return value
    }
  }
}
