package org.kalypso.model.hydrology.internal.simulation.planerclient.preprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * This class prepares the rainfall-runoff data for the PLC postprocessing; it creates the "IstZustand" folder structure if it does
 * not exist.
 * 
 * @author Dejan Antanaskovic
 */
public class NA_PreprocessingJob extends AbstractInternalStatusJob implements ISimulation
{
  private final static String INPUT_STATUSQUO_RESULTSFOLDER = "StatusQuoResultsFolder"; //$NON-NLS-1$

  private final static String INPUT_DIFFERENCE_FOLDER = "StatusQuoResultsFolder"; //$NON-NLS-1$

  private final static String INPUT_ACTUAL_RESULTSFOLDER = "ActualResultsFolder"; //$NON-NLS-1$

  private final static String OUTPUT_FOLDER = "OutputFolder"; //$NON-NLS-1$

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    boolean statusQuoFolderExists = false;
    boolean differenceFolderExists = false;
    if( inputProvider.hasID( INPUT_STATUSQUO_RESULTSFOLDER ) )
    {
      final File folder = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_STATUSQUO_RESULTSFOLDER ) );
      statusQuoFolderExists = folder.exists();
    }
    if( inputProvider.hasID( INPUT_DIFFERENCE_FOLDER ) )
    {
      final File folder = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_DIFFERENCE_FOLDER ) );
      differenceFolderExists = folder.exists();
    }
    try
    {
      if( !statusQuoFolderExists )
      {
        final File actualResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_ACTUAL_RESULTSFOLDER ) );
        if( !actualResultsFolder.exists() )
        {
          setStatus( STATUS.ERROR, "Actual results folder does not exist!" ); //$NON-NLS-1$
          return;
        }
        final File statusQuoFolder = new File( tmpdir, "IstZustand" ); //$NON-NLS-1$
        statusQuoFolder.mkdirs();
        FileUtils.copyDirectory( actualResultsFolder, statusQuoFolder );
        if( !differenceFolderExists )
        {
          final File differenceFolder = new File( tmpdir, "Difference" ); //$NON-NLS-1$
          differenceFolder.mkdirs();
          final File dummyFile = new File( differenceFolder, "control.ctl" ); //$NON-NLS-1$
          dummyFile.createNewFile();
        }
      }
    }
    catch( final IOException e )
    {
      setStatus( STATUS.ERROR, "Error creating data structure." ); //$NON-NLS-1$
      return;
    }
    resultEater.addResult( OUTPUT_FOLDER, tmpdir );
    setStatus( STATUS.OK, "Success" ); //$NON-NLS-1$
  }
}
