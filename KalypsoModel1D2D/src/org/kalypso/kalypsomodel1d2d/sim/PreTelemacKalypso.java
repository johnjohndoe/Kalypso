package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.Date;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverterTelemac;
import org.kalypso.kalypsomodel1d2d.conv.Gml2TelemacConv;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Convert from GML to Telemac-Kalypso format
 * 
 * @author ig
 */
public class PreTelemacKalypso implements ISimulation
{
  private static final String SERVER_INPUT_LOCAL = "InputLocal"; //$NON-NLS-1$

  public static final String INPUT_RESTART_FILE_PREFIX = "restartFile"; //$NON-NLS-1$
  
  private static final String MODEL_SPEC = "resource/preTelemacKalypso.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.simulation.telemac.preTelemacKalypso"; //$NON-NLS-1$

  public static final String OUTPUT_PATH_Telemac = "simulationPathTelemac"; //$NON-NLS-1$

  public static final String INPUT_PATH_RESULT_META = "scenarioResultMeta"; //$NON-NLS-1$

  public static final String CALC_CORE_EXE = "calcCoreExeTelemac"; //$NON-NLS-1$

  public static final String HOT_START_FILE = "restartFile"; //$NON-NLS-1$

  public static final String ADDITIONAL_DATA_FILE = "additionalData"; //$NON-NLS-1$

  private IGeoLog m_log;

  private IControlModel1D2D m_controlModel;

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private IFlowRelationshipModel m_flowRelationshipModel;

  private IWindModel m_windRelationshipModel;

  private Date[] m_calculatedSteps;

  private double m_doubleShiftX;

  private double m_doubleShiftY;

  private Map<IFELine, Integer> m_mapContiLineWithTelemacBoundaryToCondition;

//  private Map<GM_Position, Integer> m_mapGMPositions;

  private IScenarioResultMeta m_scenarioMetaData;

//  private String m_input;
  
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( MODEL_SPEC );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final SimulationMonitorAdaptor progressMonitor = new SimulationMonitorAdaptor( monitor );
//    m_input = System.getProperty( "org.kalypso.service.wps.input" ); //$NON-NLS-1$
//    IContainer scenarioFolder = null;
//    if( m_input == null || m_input.equals( "" ) || SERVER_INPUT_LOCAL.equals( m_input ) )
//    {
//      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
//      scenarioFolder = caseDataProvider.getScenarioFolder();
//    }

    try
    {
      m_log = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    OutputStream logOS = null;
    OutputStream errorOS = null;
    FileSystemManagerWrapper manager = null;
    try
    {
      manager = VFSUtilities.getNewManager();

      final URL controlUrl = (URL) inputProvider.getInputForID( "control" ); //$NON-NLS-1$
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlUrl, null );
      final IControlModelGroup controlModelGroup = (IControlModelGroup) controlWorkspace.getRootFeature().getAdapter( IControlModelGroup.class );
      m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();
      
      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      
      m_discretisationModel = null;
      try
      {
        m_discretisationModel = caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
      }
      catch( Exception e )
      {
      }
      if( m_discretisationModel == null )
      {
        final URL meshUrl = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_MESH );
        final GMLWorkspace discWorkspace = GmlSerializer.createGMLWorkspace( meshUrl, null );
        m_discretisationModel = (IFEDiscretisationModel1d2d) discWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
      }

      m_flowRelationshipModel = null;
      try
      {
        m_flowRelationshipModel = caseDataProvider.getModel( IFlowRelationshipModel.class.getName() );
      }
      catch( Exception e )
      {
      }
      if( m_flowRelationshipModel == null )
      {
        final URL flowRelURL = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_FLOW_RELATIONSHIPS );
        final GMLWorkspace flowRelWorkspace = GmlSerializer.createGMLWorkspace( flowRelURL, null );
        m_flowRelationshipModel = (IFlowRelationshipModel) flowRelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
      }

      m_scenarioMetaData = null;
      try
      {
        m_scenarioMetaData = caseDataProvider.getModel( IScenarioResultMeta.class.getName() );
      }
      catch( Exception e )
      {
      }
      if( m_scenarioMetaData == null )
      {
        final URL resultMetaURL = (URL) inputProvider.getInputForID( PreTelemacKalypso.INPUT_PATH_RESULT_META );
        final GMLWorkspace resultMetaWorkspace = GmlSerializer.createGMLWorkspace( resultMetaURL, null );
        m_scenarioMetaData = (IScenarioResultMeta) resultMetaWorkspace.getRootFeature().getAdapter( IScenarioResultMeta.class );
      }

      try
      {
        m_windRelationshipModel = caseDataProvider.getModel( IWindModel.class.getName() );
      }
      catch( Exception e )
      {
      }
      if( m_windRelationshipModel == null )
      {
        final URL windURL = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_WIND_RELATIONSHIPS );
        final GMLWorkspace windWorkspace = GmlSerializer.createGMLWorkspace( windURL, null );
        m_windRelationshipModel = (IWindModel) windWorkspace.getRootFeature().getAdapter( IWindModel.class );
      }

      final FileObject lFileObjWorkingDir = manager.toFileObject( tmpdir );

      final RestartNodes restartNodes;

      if( m_controlModel.getRestart() )
      {
        restartNodes = new RestartNodes();
        for( int i = 0; i < 3; i++ )
        {
          final String restartFileInputName = INPUT_RESTART_FILE_PREFIX + i;
          if( inputProvider.hasID( restartFileInputName ) )
          {
            final URL restartURL = (URL) inputProvider.getInputForID( restartFileInputName );
            restartNodes.addResultUrl( restartURL );
          }
        }
      }
      else
      {
        restartNodes = null;
      }
      writeTelemacFiles( lFileObjWorkingDir, restartNodes, progressMonitor );

      resultEater.addResult( OUTPUT_PATH_Telemac, new File( tmpdir.toURI() ) );

    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "PreTelemacKalypso.0" ), e ); //$NON-NLS-1$
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

  private void writeAdditionaData( final FileObject pFileObjWorkingDir, final ResultManager pResultManager, final SubMonitor progress ) throws IOException
  {
    final SubMonitor subProgress = SubMonitor.convert( progress, 100 );
    ProgressUtilities.worked( subProgress, 1 );

  }

  private void writeContolFile( final String projectFileName, final FileObject pFileObjWorkingDir, final ResultManager pResultManager ) throws FileSystemException, IOException
  {
    final FileObject lTelemacControlFile = pFileObjWorkingDir.resolveFile( projectFileName + Gml2TelemacConv.CASE_FILE_EXTENTION );
    final Control1D2DConverterTelemac controlConverter = new Control1D2DConverterTelemac( pFileObjWorkingDir, projectFileName, m_discretisationModel, m_controlModel, m_flowRelationshipModel, m_log, pResultManager, m_mapContiLineWithTelemacBoundaryToCondition, m_doubleShiftX, m_doubleShiftY );
    controlConverter.writeControlFile( lTelemacControlFile.getContent().getOutputStream() );
    lTelemacControlFile.close();
  }

  private String writeMesh( final FileObject pFileObjWorkingDir, final RestartNodes restartNodes ) throws FileSystemException
  {
    /* Telemac Mesh(*.slf) */
    final Gml2TelemacConv converter2D = new Gml2TelemacConv( m_discretisationModel, m_flowRelationshipModel, m_controlModel, restartNodes, m_log );
    converter2D.writeTelemacModel( pFileObjWorkingDir );
    return converter2D.getProjectFileName();
  }

  
  private void writeTelemacFiles( final FileObject pFileObjWorkingDir, final RestartNodes restartNodes, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    ProgressUtilities.worked( progress, 1 );
    progress.beginTask( "", 100 ); //$NON-NLS-1$
    try
    {
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.3" ) ); //$NON-NLS-1$

      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.4" ) ); //$NON-NLS-1$
      // ProgressUtilities.worked( progress, 20 );

      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.6" ) ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.7" ) ); //$NON-NLS-1$
      String projectFileName = writeMesh( pFileObjWorkingDir, restartNodes );

      ProgressUtilities.worked( progress, 10 );

      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.9" ) ); //$NON-NLS-1$

      ProgressUtilities.worked( progress, 60 );
      // TelemacWindDataWriter lWindWriter = writeWindData( pFileObjWorkingDir );

      ProgressUtilities.worked( progress, 10 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.9" ) ); //$NON-NLS-1$

      writeContolFile( projectFileName, pFileObjWorkingDir, null );

      ProgressUtilities.worked( progress, 10 );

    }
    catch( final Exception e )
    {
      final String msg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.TelemacCalculation.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, e ) );
    }
    finally
    {
      progress.done();
    }
  }

}
