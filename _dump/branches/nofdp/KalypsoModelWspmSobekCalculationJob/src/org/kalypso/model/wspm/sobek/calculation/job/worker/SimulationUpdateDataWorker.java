package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationUpdateDataWorker implements ISimulation
{

  private final PrintStream m_outputStream;

  public SimulationUpdateDataWorker( PrintStream outputStream )
  {
    m_outputStream = outputStream;
  }

  public URL getSpezifikation( )
  {
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {

    ConsoleHelper.writeLine( m_outputStream, String.format( "Perparing calculation core model input data..." ) );

    /* extract computation data */
    extractData( tmpdir, inputProvider );

    ConsoleHelper.writeLine( m_outputStream, String.format( "Model input data prepared." ) );
    ConsoleHelper.writeLine( m_outputStream, "" );
  }

  private void extractData( final File tmpdir, final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    try
    {
      final URL urlCalcCase = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.CALC_CASE_PATH );
      final URL urlFlowNetwork = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.FLOW_NETWORK_PATH );

      /* src directories */
      final File folderCalcCase = new File( urlCalcCase.getFile() );
      if( !folderCalcCase.exists() )
        throw new SimulationException( String.format( "Missing data folder - %s", urlCalcCase.toExternalForm() ) );

      final File folderFlowNetwork = new File( urlFlowNetwork.getFile() );
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( "Missing data folder - %s", urlFlowNetwork.toExternalForm() ) );

      /* destination directory */
      final File destination = new File( tmpdir, "Sobek-IDSS" );
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( "Missing destination folder - %s", destination.getAbsolutePath() ) );

      /* copy src folders to destination dir */
      FileUtils.copyDirectory( folderFlowNetwork, destination );
      FileUtils.copyDirectory( folderCalcCase, destination );
    }
    catch( final IOException e )
    {
      throw new SimulationException( String.format( "Overwriting data directories failed. cause: %s", e.getMessage() ) );
    }
  }

}
