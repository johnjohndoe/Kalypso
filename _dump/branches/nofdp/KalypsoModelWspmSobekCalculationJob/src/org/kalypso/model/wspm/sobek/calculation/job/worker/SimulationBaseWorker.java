package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.sobek.calculation.job.ISobekCalculationJobConstants;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class SimulationBaseWorker implements ISimulation
{

  public URL getSpezifikation( )
  {
    throw new NotImplementedException();
  }

  /**
   * extract basic result processing stuff to tempdir
   */
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      /* extract computation base directories */
      extractCalculationCore( tmpdir );

      /* extract computation data */
      extractData( tmpdir, inputProvider );
    }
    catch( IOException e )
    {
      e.printStackTrace();

      throw new SimulationException( e.getMessage() );
    }
  }

  private void extractData( File tmpdir, ISimulationDataProvider inputProvider ) throws SimulationException
  {
    URL urlCalcCase = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.CALC_CASE_PATH );
    URL urlFlowNetwork = (URL) inputProvider.getInputForID( ISobekCalculationJobConstants.FLOW_NETWORK_PATH );

    throw new NotImplementedException();
  }

  private void extractCalculationCore( File tmpdir ) throws IOException
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
