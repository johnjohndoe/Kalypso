package org.kalypso.na.preprocessing;

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
 * This class is the pre-processor for standard rainfall-runoff calculation. The behaviour of it depends on the
 * information given by model specification:
 * <ul>
 * <li>If the "CalculationNature" parameter is set to "PLC", this class prepares the data for the PLC postprocessing; it
 * creates the "IstZustand" folder structure if it does not exist.</li>
 * <li>If the value of optional parameter "CalculationNature" is <code>null</code> or different than "PLC", the class
 * does nothing.</li>
 * </ul>
 * This behaviour can be easily extended for different purposes.<br />
 * <br />
 * 
 * @author Dejan Antanaskovic
 */

public class NA_PreprocessingJob extends AbstractInternalStatusJob implements ISimulation
{
  private final static String INPUT_CALCULATION_NATURE = "CalculationNature";

  private final static String INPUT_STATUSQUO_RESULTSFOLDER = "StatusQuoResultsFolder";

  private final static String INPUT_DIFFERENCE_FOLDER = "StatusQuoResultsFolder";

  private final static String INPUT_ACTUAL_RESULTSFOLDER = "ActualResultsFolder";

  private final static String OUTPUT_FOLDER = "OutputFolder";

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final boolean hasCalculationNature = inputProvider.hasID( INPUT_CALCULATION_NATURE );
    if( !hasCalculationNature )
    {
      setStatus( STATUS.OK, "Success" );
      return;
    }

    // TODO: Workaround for PLC Ticket #374, switch after fixing to the commented line
    // final String calculationNature = (String) inputProvider.getInputForID( "CalculationNature" );
    String calculationNature = (String) inputProvider.getInputForID( INPUT_CALCULATION_NATURE );
    calculationNature = calculationNature.substring( calculationNature.lastIndexOf( "/" ) + 1 );

    if( "PLC".equals( calculationNature ) )
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
            setStatus( STATUS.ERROR, "Actual results folder does not exist!" );
            return;
          }
          final File statusQuoFolder = new File( tmpdir, "IstZustand" );
          statusQuoFolder.mkdirs();
          FileUtils.copyDirectory( actualResultsFolder, statusQuoFolder );
          if( !differenceFolderExists )
          {
            final File differenceFolder = new File( tmpdir, "Difference" );
            differenceFolder.mkdirs();
            final File dummyFile = new File( differenceFolder, "control.ctl" );
            dummyFile.createNewFile();
          }
        }
      }
      catch( final IOException e )
      {
        setStatus( STATUS.ERROR, "Error creating data structure." );
        return;
      }
      resultEater.addResult( OUTPUT_FOLDER, tmpdir );
    }
    setStatus( STATUS.OK, "Success" );
  }
}
