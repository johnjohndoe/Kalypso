package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.OutputStream;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Convert from RMAKalypso to GML results format
 * 
 * @author kurzbach
 */
public class PostRMAKalypso implements ISimulation
{
  public static final String INPUT_RESULTS = "results"; //$NON-NLS-1$

  public static final String INPUT_MESH = "mesh"; //$NON-NLS-1$

  public static final String INPUT_CONTROL = "control"; //$NON-NLS-1$

  public static final String INPUT_FLOW_RELATIONSHIPS = "flowRelationships"; //$NON-NLS-1$

  public static final String INPUT_SCENARIO_RESULT_META = "scenarioResultMeta"; //$NON-NLS-1$

  public static final String INPUT_SINGLE_RESULT_DATE = "singleResultDate"; //$NON-NLS-1$

  public static final String OUTPUT_SCENARIO_RESULT_META = "scenarioResultMeta"; //$NON-NLS-1$

  public static final String OUTPUT_RESULTS = "results"; //$NON-NLS-1$

  private static final String MODEL_SPEC = "resource/postRMAKalypso.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.simulation.rma.postRMAKalypso"; //$NON-NLS-1$

  private GeoLog m_geoLog;

  public static DateFormat DATE_FORMAT = new SimpleDateFormat( ResultMeta1d2dHelper.FULL_DATE_TIME_FORMAT_RESULT_STEP ); //$NON-NLS-1$

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( MODEL_SPEC );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final SimulationMonitorAdaptor progressMonitor = new SimulationMonitorAdaptor( monitor );
    try
    {
      m_geoLog = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    final OutputStream logOS = null;
    final OutputStream errorOS = null;
    FileSystemManagerWrapper manager = null;
    try
    {
      manager = VFSUtilities.getNewManager();

      final Object resultDirRMAUri = inputProvider.getInputForID( INPUT_RESULTS );
      final FileObject resultDirRMA = VFSUtilities.getNewManager().resolveFile( resultDirRMAUri.toString() );

      final URL controlUrl = (URL) inputProvider.getInputForID( INPUT_CONTROL );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlUrl, null );
      final IControlModelGroup controlModelGroup = (IControlModelGroup) controlWorkspace.getRootFeature().getAdapter( IControlModelGroup.class );
      final IControlModel1D2D controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

      final URL meshUrl = (URL) inputProvider.getInputForID( INPUT_MESH );
      final GMLWorkspace discWorkspace = GmlSerializer.createGMLWorkspace( meshUrl, null );
      final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) discWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );

      final URL flowRelURL = (URL) inputProvider.getInputForID( INPUT_FLOW_RELATIONSHIPS );
      final GMLWorkspace flowRelWorkspace = GmlSerializer.createGMLWorkspace( flowRelURL, null );
      final IFlowRelationshipModel flowRelationshipModel = (IFlowRelationshipModel) flowRelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );

      final URL scenarioResultMetaURL = (URL) inputProvider.getInputForID( INPUT_SCENARIO_RESULT_META );
      final GMLWorkspace scenarioResultMetaWorkspace = GmlSerializer.createGMLWorkspace( scenarioResultMetaURL, null );
      final IScenarioResultMeta scenarioResultMeta = (IScenarioResultMeta) scenarioResultMetaWorkspace.getRootFeature().getAdapter( IScenarioResultMeta.class );
      final ResultManager resultManager = new ResultManager( resultDirRMA, null, discModel, controlModel, flowRelationshipModel, scenarioResultMeta, m_geoLog );

      final ProcessResultsBean bean = new ProcessResultsBean();
      bean.deleteAll = true;
      bean.deleteFollowers = true;
      bean.evaluateFullResults = true;

      if( inputProvider.hasID( INPUT_SINGLE_RESULT_DATE ) )
      {
        final Date singleResultDate = DATE_FORMAT.parse( (String) inputProvider.getInputForID( INPUT_SINGLE_RESULT_DATE ) );
        bean.userCalculatedSteps = new Date[] { singleResultDate };
      }
      else
        bean.userCalculatedSteps = resultManager.findCalculatedSteps();

      resultManager.setStepsToProcess( bean.userCalculatedSteps, resultManager.getControlModel() );

      final ResultProcessingOperation processingOperation = new ResultProcessingOperation( resultManager, bean );
      IStatus resultStatus = processingOperation.execute( progressMonitor );

      // if OK move the new results data to the results folder
      // this operation is not cancelable
      if( resultStatus.isOK() )
      {
        // processing finished without problems, prepare the data-operation
        // this is where the name of the result folder is actually set
        final ICalcUnitResultMeta calcUnitMeta = processingOperation.getCalcUnitMeta();
        final String calcUnitId = calcUnitMeta.getCalcUnit();
        // TODO this causes a problem if called with an uninitialized workspace
        final String[] lResultsToRemove = {}; // processingOperation.getOriginalStepsToDelete();

        final File unitFolder = new File( tmpdir, "results/" + calcUnitId ); //$NON-NLS-1$
        final ResultManagerOperation dataOperation = new ResultManagerOperation( resultManager, unitFolder, Status.OK_STATUS, processingOperation.getOutputDir(), calcUnitMeta, lResultsToRemove );
        dataOperation.setBoolRemoveRawResult( false );
        resultStatus = dataOperation.execute( progressMonitor );

        resultEater.addResult( OUTPUT_RESULTS, unitFolder );

        final File outScenarioResultMeta = new File( tmpdir, "scenarioResultMeta.gml" ); //$NON-NLS-1$
        GmlSerializer.serializeWorkspace( outScenarioResultMeta, scenarioResultMetaWorkspace, "UTF-8" ); //$NON-NLS-1$
        resultEater.addResult( OUTPUT_SCENARIO_RESULT_META, outScenarioResultMeta );
      }

      if( !resultStatus.isOK() )
        throw new CoreException( resultStatus );
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "PreRMAKalypso.1" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      if( manager != null )
        manager.close();

      progressMonitor.done();
    }
  }
}
