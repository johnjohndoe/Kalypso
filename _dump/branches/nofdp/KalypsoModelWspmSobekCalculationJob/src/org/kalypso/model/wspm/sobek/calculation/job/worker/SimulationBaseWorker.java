package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationBaseWorker implements ISimulation
{

  private final PrintStream m_outputStream;

  public SimulationBaseWorker( PrintStream outputStream )
  {
    m_outputStream = outputStream;
  }

  public URL getSpezifikation( )
  {
    throw new NotImplementedException();
  }

  /**
   * extract basic result processing stuff to tempdir
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      ConsoleHelper.writeLine( m_outputStream, String.format( "---> Preparing Sobek Calculation Core..." ) );

      /* extract computation base directories */
      extractCalculationCore( tmpdir );

      ConsoleHelper.writeLine( m_outputStream, String.format( "---> Sobek Calculation Core prepared." ) );
      ConsoleHelper.writeLine( m_outputStream, "" );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new SimulationException( e.getMessage() );
    }
  }

  private void extractCalculationCore( final File tmpdir ) throws IOException
  {
    InputStream zipStream = null;
    zipStream = getClass().getResourceAsStream( "/org/kalypso/model/wspm/sobek/calculation/job/resources/calculationCore.zip" ); //$NON-NLS-1$
    if( zipStream != null )
    {
      ZipUtilities.unzipApache( zipStream, tmpdir, true, "IBM850" ); //$NON-NLS-1$
      zipStream.close();
    }
  }

}
