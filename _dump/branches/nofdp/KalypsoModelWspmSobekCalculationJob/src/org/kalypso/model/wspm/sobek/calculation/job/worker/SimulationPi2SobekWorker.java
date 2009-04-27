package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.MyPrintStream;
import org.kalypso.contribs.java.io.StreamGobbler;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.model.wspm.sobek.calculation.job.i18n.Messages;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationPi2SobekWorker implements ISimulation
{

  private final MyPrintStream m_outputStream;

  private final MyPrintStream m_sobekStream;

  public SimulationPi2SobekWorker( MyPrintStream nofdpStream, MyPrintStream sobekStream )
  {
    m_outputStream = nofdpStream;
    m_sobekStream = sobekStream;
  }

  public URL getSpezifikation( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationPi2SobekWorker_0 ) );

    /*******************************************************************************************************************
     * PROCESSING
     ******************************************************************************************************************/
    /* The command for execution. */
    final File directory = new File( tmpdir, ISobekCalculationJobConstants.PATH_SOBEK_BATCH_DIR );

    final String[] command = new String[3];
    command[0] = "cmd.exe"; //$NON-NLS-1$
    command[1] = "/C"; //$NON-NLS-1$
    command[2] = "1_PI2sobek.bat"; //$NON-NLS-1$

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
        if( monitor.isCanceled() )
          throw new SimulationException( "Computation Canceld" ); //$NON-NLS-1$

        break;
      }
      catch( final RuntimeException e )
      {
        /* The process has not finished. */
      }

      /* Wait a few milliseconds, before continuing. */
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

    ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationPi2SobekWorker_4 ) );
    ConsoleHelper.writeLine( m_outputStream, "" ); //$NON-NLS-1$

    /* add pi conversion log to result eater */
    File logFile = new File( tmpdir, ISobekCalculationJobConstants.LOG_PI2SOBEK_PATH );
    if( !logFile.exists() )
      throw new SimulationException( Messages.SimulationPi2SobekWorker_6 );

    resultEater.addResult( ISobekCalculationJobConstants.LOG_PI2SOBEK, logFile );

    if( !checkLogFile( logFile ) )
      throw new SimulationException( Messages.SimulationPi2SobekWorker_7 );
  }

  /**
   * In case of a valid conversion, the log file contains the following phrase: "The conversion was successfull" (typo
   * is correct!)
   */
  private boolean checkLogFile( File logFile ) throws SimulationException
  {
    FileInputStream inputStream = null;
    BufferedReader reader = null;
    try
    {
      inputStream = new FileInputStream( logFile );
      reader = new BufferedReader( new InputStreamReader( inputStream ) );
      String line = null;

      while( (line = reader.readLine()) != null )
      {
        if( line.contains( "The conversion was successfull" ) ) // //$NON-NLS-1$
          return true;
      }

    }
    catch( FileNotFoundException e )
    {
      throw new SimulationException( Messages.SimulationPi2SobekWorker_9 );
    }
    catch( IOException e )
    {
      throw new SimulationException( Messages.SimulationPi2SobekWorker_10 );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
      IOUtils.closeQuietly( reader );
    }

    return false;
  }
}
