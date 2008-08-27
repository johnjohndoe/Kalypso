package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.MyPrintStream;
import org.kalypso.model.wspm.sobek.calculation.job.i18n.Messages;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationBaseWorker implements ISimulation
{

  private final MyPrintStream m_outputStream;

  public SimulationBaseWorker( MyPrintStream outputStream )
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
      ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationBaseWorker_0 ) );

      /* extract computation base directories */
      extractCalculationCore( tmpdir );

      ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationBaseWorker_1 ) );
      ConsoleHelper.writeLine( m_outputStream, Messages.SimulationBaseWorker_2 );
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
