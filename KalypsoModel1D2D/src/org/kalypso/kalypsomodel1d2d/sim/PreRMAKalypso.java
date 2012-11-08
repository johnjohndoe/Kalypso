package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.OutputStream;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.IPreRMAData;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAData;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAFiles;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAKalypsoWorker;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Convert from GML to RMAKalypso format
 * 
 * @author kurzbach
 */
public class PreRMAKalypso implements ISimulation, IRMAPreprocessing
{
//  public static final String INPUT_RESTART_FILE_PREFIX = "restartFile"; //$NON-NLS-1$
//
//  public static final String INPUT_ROUGHNESS = "roughness"; //$NON-NLS-1$
//
//  public static final String INPUT_FLOW_RELATIONSHIPS = "flowRelationships"; //$NON-NLS-1$
//
//  public static final String INPUT_WIND_RELATIONSHIPS = "wind"; //$NON-NLS-1$
//
//  public static final String INPUT_CALCULATION_UNIT_ID = "calculationUnitID"; //$NON-NLS-1$
//
//  public static final String INPUT_MESH = "mesh"; //$NON-NLS-1$
//
//  public static final String INPUT_CONTROL = "control"; //$NON-NLS-1$
//
//  public static final String OUTPUT_MESH = ISimulation1D2DConstants.MODEL_2D;
//
//  public static final String OUTPUT_BC_WQ = ISimulation1D2DConstants.BC_WQ_File;
//
//  public static final String OUTPUT_BUILDINGS = ISimulation1D2DConstants.BUILDING_File;
//
//  public static final String OUTPUT_WIND = ISimulation1D2DConstants.WIND_RMA10_File;
//
//  public static final String OUTPUT_WIND_COORD = ISimulation1D2DConstants.WIND_RMA10_COORDS_File;
//
//  public static final String OUTPUT_CONTROL = ISimulation1D2DConstants.R10_File;
//
//  public static final String INPUT_RESTART_FILE = "restartFile0"; //$NON-NLS-1$
//
//  public static final String OUTPUT_RMA_VERSION = "rmaVersion"; //$NON-NLS-1$

  private static final String MODEL_SPEC = "resource/preRMAKalypso.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.simulation.rma.preRMAKalypso"; //$NON-NLS-1$

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( MODEL_SPEC );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final SimulationMonitorAdaptor progressMonitor = new SimulationMonitorAdaptor( monitor );

    final IGeoLog log = initializeLog();

    final OutputStream logOS = null;
    final OutputStream errorOS = null;
    try
    {
      final IPreRMAData preData = initData( inputProvider );

      final PreRMAFiles outputFiles = new PreRMAFiles( tmpdir );

      final PreRMAKalypsoWorker worker = new PreRMAKalypsoWorker( preData, log, tmpdir, outputFiles );
      worker.execute( progressMonitor );

      resultEater.addResult( OUTPUT_MESH, outputFiles.getMeshFile() );
      resultEater.addResult( OUTPUT_CONTROL, outputFiles.getControlFile() );
      resultEater.addResult( OUTPUT_BUILDINGS, outputFiles.getBuildingFile() );
      resultEater.addResult( OUTPUT_BC_WQ, outputFiles.getBcWqFile() );
      resultEater.addResult( OUTPUT_WIND, outputFiles.getWindFile() );
      resultEater.addResult( OUTPUT_WIND_COORD, outputFiles.getWindCoordFile() );
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "PreRMAKalypso.1" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      progressMonitor.done();
    }
  }

  private IGeoLog initializeLog( ) throws SimulationException
  {
    try
    {
      return new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }
  }

  private IPreRMAData initData( final ISimulationDataProvider inputProvider )
  {
    final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    return new PreRMAData( caseDataProvider, inputProvider );
  }
}