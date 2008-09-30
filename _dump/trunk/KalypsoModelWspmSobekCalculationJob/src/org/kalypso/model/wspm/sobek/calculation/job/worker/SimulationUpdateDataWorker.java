package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.MyPrintStream;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.model.wspm.sobek.calculation.job.i18n.Messages;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationUpdateDataWorker implements ISimulation
{

  private final MyPrintStream m_outputStream;

  public SimulationUpdateDataWorker( MyPrintStream outputStream )
  {
    m_outputStream = outputStream;
  }

  public URL getSpezifikation( )
  {
    return null;
  }

  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {

    ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationUpdateDataWorker_0 ) );

    /* extract computation data */
    extractData( tmpdir, inputProvider );

    ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationUpdateDataWorker_1 ) );
    ConsoleHelper.writeLine( m_outputStream, "" ); //$NON-NLS-1$
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
        throw new SimulationException( String.format( Messages.SimulationUpdateDataWorker_3, urlCalcCase.toExternalForm() ) );

      final File folderFlowNetwork = new File( urlFlowNetwork.getFile() );
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( Messages.SimulationUpdateDataWorker_4, urlFlowNetwork.toExternalForm() ) );

      /* destination directory */
      final File destination = new File( tmpdir, "Sobek-IDSS" ); //$NON-NLS-1$
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( Messages.SimulationUpdateDataWorker_6, destination.getAbsolutePath() ) );

      /* copy src folders to destination dir */
      FileUtils.copyDirectory( folderFlowNetwork, destination );
      FileUtils.copyDirectory( folderCalcCase, destination );
    }
    catch( final IOException e )
    {
      throw new SimulationException( String.format( Messages.SimulationUpdateDataWorker_7, e.getMessage() ) );
    }
  }

}
