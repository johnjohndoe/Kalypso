package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.IPreRMAData;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAData;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAFiles;
import org.kalypso.kalypsomodel1d2d.sim.preprocessing.PreRMAKalypsoWorker;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

import com.google.common.base.Charsets;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Convert from GML to RMAKalypso format
 * 
 * @author kurzbach
 */
public class PreRMAKalypso implements ISimulation, IRMAPreprocessing
{
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

    final PreRMAFiles outputFiles = new PreRMAFiles( tmpdir );

    /* initialize logging */
    final File logFile = outputFiles.getLogFile();

    final IGeoLog log = initializeLog();

    try
    {
      final IPreRMAData preData = initData( inputProvider );

      final PreRMAKalypsoWorker worker = new PreRMAKalypsoWorker( preData, log, tmpdir, outputFiles );
      worker.execute( progressMonitor );

      resultEater.addResult( OUTPUT_MESH, outputFiles.getMeshFile() );
      resultEater.addResult( OUTPUT_CONTROL, outputFiles.getControlFile() );
      resultEater.addResult( OUTPUT_BUILDINGS, outputFiles.getBuildingFile() );
      resultEater.addResult( OUTPUT_BC_WQ, outputFiles.getBcWqFile() );
      resultEater.addResult( OUTPUT_WIND, outputFiles.getWindFile() );
      resultEater.addResult( OUTPUT_WIND_COORD, outputFiles.getWindCoordFile() );
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      monitor.setFinishInfo( status.getSeverity(), status.getMessage() );
      return;
    }
    catch( final SimulationException e )
    {
      log.log( IStatus.ERROR, 0, e.toString(), null, null );
      monitor.setFinishInfo( IStatus.ERROR, e.toString() );
      return;
      // throw e;
    }
    catch( final IOException e )
    {
      log.log( IStatus.ERROR, 0, e.toString(), null, null );
      monitor.setFinishInfo( IStatus.ERROR, e.toString() );
      return;
    }
    finally
    {
      saveLog( log, logFile );
      resultEater.addResult( OUTPUT_LOG, logFile );

      progressMonitor.done();
    }
  }

  private void saveLog( final IGeoLog log, final File logFile )
  {
    try
    {
      final IStatusCollection statusCollection = log.getStatusCollection();
      final GMLWorkspace workspace = statusCollection.getWorkspace();
      GmlSerializer.serializeWorkspace( logFile, workspace, Charsets.UTF_8.name() );
    }
    catch( IOException | GmlSerializeException e )
    {
      e.printStackTrace();
    }
  }

  private IGeoLog initializeLog( ) throws SimulationException
  {
    try
    {
      return new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final GMLSchemaException e )
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