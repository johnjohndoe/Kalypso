package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.URL;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.StreamGobbler;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationSobekOpenMIWorker implements ISimulation
{

  private final PrintStream m_nofdpStream;

  private final PrintStream m_sobekStream;

  public SimulationSobekOpenMIWorker( PrintStream nofdpStream, PrintStream sobekStream )
  {
    m_nofdpStream = nofdpStream;
    m_sobekStream = sobekStream;
  }

  public URL getSpezifikation( )
  {
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {

    ConsoleHelper.writeLine( m_nofdpStream, String.format( "---> Starting OpenMI to process model..." ) );

    /*******************************************************************************************************************
     * PROCESSING
     ******************************************************************************************************************/
    /* The command for execution. */
    final File directory = new File( tmpdir, "Sobek-IDSS/batch" );

    final String[] command = new String[3];
    command[0] = "cmd.exe";
    command[1] = "/C";
    command[2] = "2_runOpenMI.cmd";

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
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    ConsoleHelper.writeLine( m_nofdpStream, String.format( "---> Model processed." ) );
    ConsoleHelper.writeLine( m_nofdpStream, "" );

    /* add openmi control log file */
    File logOpenMi = new File( tmpdir, ISobekCalculationJobConstants.LOG_OPENMI_CONTROL_PATH );
    if( !logOpenMi.exists() )
      throw new SimulationException( "OpenMI control log file doesn't exists..." );

    /* add sobek computation log file */
    File logSobek = new File( tmpdir, ISobekCalculationJobConstants.LOG_SOBEK_PATH );
    if( !logSobek.exists() )
      throw new SimulationException( "Sobek Calculation Core computation log file doesn't exists..." );

    resultEater.addResult( ISobekCalculationJobConstants.LOG_OPENMI_CONTROL, logOpenMi );
    resultEater.addResult( ISobekCalculationJobConstants.LOG_SOBEK, logSobek );

    if( !checkLogFiles( logOpenMi, logSobek ) )
      throw new SimulationException( "Calculation failed..." );
  }

  /**
   * @param logOpenMi
   *            if calculation was successful file contains phrase "Simulation finished successfuly" (typo is correct!)
   * @param logSobek
   *            if calculation was successful file contains phrase "Normal end of Sobeksim"
   * @return
   * @throws SimulationException
   */
  private boolean checkLogFiles( File logOpenMi, File logSobek ) throws SimulationException
  {
    boolean openmi = false;
    boolean sobek = false;

    FileInputStream inputStream = null;
    BufferedReader reader = null;
    try
    {
      inputStream = new FileInputStream( logOpenMi );
      reader = new BufferedReader( new InputStreamReader( inputStream, "UTF-16" ) );
      String line = null;

      Pattern p = Pattern.compile( ".*Simulation finished successfuly.*" );

      while( (line = reader.readLine()) != null )
      {
        Matcher m = p.matcher( line );
        if( m.matches() )
        {
          openmi = true;
          break;
        }

      }

    }
    catch( FileNotFoundException e )
    {
      throw new SimulationException( "OpenMi Control log file not found." );
    }
    catch( IOException e )
    {
      throw new SimulationException( "Error reading OpenMi Control log file." );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
      IOUtils.closeQuietly( reader );
    }

    try
    {
      inputStream = new FileInputStream( logSobek );
      reader = new BufferedReader( new InputStreamReader( inputStream ) );
      String line = null;

      while( (line = reader.readLine()) != null )
      {
        if( line.contains( "Normal end of Sobeksim" ) )
        {
          sobek = true;
          break;
        }

      }
    }
    catch( FileNotFoundException e )
    {
      throw new SimulationException( "Sobek calculation core log file not found." );
    }
    catch( IOException e )
    {
      throw new SimulationException( "Error reading Sobek calculation core log file." );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
      IOUtils.closeQuietly( reader );
    }

    if( openmi == false || sobek == false )
      return false;

    return true;
  }
}
