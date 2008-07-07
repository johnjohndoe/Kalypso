package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationUpdateDataWorker implements ISimulation
{

  public URL getSpezifikation( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor ) throws SimulationException
  {
    /* extract computation data */
    extractData( tmpdir, inputProvider );
  }

  private void extractData( File tmpdir, ISimulationDataProvider inputProvider ) throws SimulationException
  {
    try
    {
      URL urlCalcCase = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.CALC_CASE_PATH );
      URL urlFlowNetwork = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.FLOW_NETWORK_PATH );

      /* src directories */
      File folderCalcCase = new File( urlCalcCase.getFile() );
      if( !folderCalcCase.exists() )
        throw new SimulationException( String.format( "Missing data folder - %s", urlCalcCase.toExternalForm() ) );

      File folderFlowNetwork = new File( urlFlowNetwork.getFile() );
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( "Missing data folder - %s", urlFlowNetwork.toExternalForm() ) );

      /* destination directory */
      File destination = new File( tmpdir, "Sobek-IDSS" );
      if( !folderFlowNetwork.exists() )
        throw new SimulationException( String.format( "Missing destination folder - %s", destination.getAbsolutePath() ) );

      /* copy src folders to destination dir */
      FileUtils.copyDirectory( folderFlowNetwork, destination );
      FileUtils.copyDirectory( folderCalcCase, destination );
    }
    catch( IOException e )
    {
      throw new SimulationException( String.format( "Overwriting data directories failed. cause: %s", e.getMessage() ) );
    }
  }

}
