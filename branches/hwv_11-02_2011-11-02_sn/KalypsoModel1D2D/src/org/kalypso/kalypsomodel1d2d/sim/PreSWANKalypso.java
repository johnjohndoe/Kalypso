package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverterSWAN;
import org.kalypso.kalypsomodel1d2d.conv.Gml2SWANConv;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.SWANAdditionalDataConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.SimpleNodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.wind.SWANWindDataWriter;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
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
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Convert from GML to SWAN-Kalypso format
 * 
 * @author ig
 */
public class PreSWANKalypso implements ISimulation
{
  private static final String MODEL_SPEC = "resource/preSWANKalypso.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.simulation.swan.preSWANKalypso"; //$NON-NLS-1$

  public static final String OUTPUT_PATH_SWAN = "simulationPathSWAN"; //$NON-NLS-1$

  public static final String OUTPUT_PATH_RMA = "simulationPathRMA"; //$NON-NLS-1$

  public static final String INPUT_PATH_RESULT_META = "scenarioResultMeta"; //$NON-NLS-1$

  public static final String CALC_CORE_EXE = "calcCoreExeSWAN"; //$NON-NLS-1$

  public static final String HOT_START_FILE = "hotFile"; //$NON-NLS-1$

  public static final String ADDITIONAL_DATA_FILE = "additionalData"; //$NON-NLS-1$

  private IGeoLog m_log;

  private IControlModel1D2D m_controlModel;

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private IFlowRelationshipModel m_flowRelationshipModel;

  private IWindModel m_windRelationshipModel;

  private Date[] m_calculatedSteps;

  private double m_doubleShiftX;

  private double m_doubleShiftY;

  private Map<IFELine, Integer> m_mapContiLineWithSWANBoundaryToCondition;

  private Map<GM_Position, Integer> m_mapGMPositions;

  private IScenarioResultMeta m_scenarioMetaData;

  private URL m_additionalDataURL = null;

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
      // next block checks if the models are already in the memory(this is the case by local calculations) and does not
      // reload such models.
      manager = VFSUtilities.getNewManager();
      final String lStrURL = (String) inputProvider.getInputForID( OUTPUT_PATH_RMA );
      final FileObject lFileObjPreResultsDir = manager.resolveFile( lStrURL );

      final URL controlUrl = (URL) inputProvider.getInputForID( "control" ); //$NON-NLS-1$
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlUrl, null );
      final IControlModelGroup controlModelGroup = (IControlModelGroup) controlWorkspace.getRootFeature().getAdapter( IControlModelGroup.class );
      m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

      m_discretisationModel = null;
      try
      {
        final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();
        m_discretisationModel = caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName(), IFEDiscretisationModel1d2d.class );
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
        final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();
        m_flowRelationshipModel = caseDataProvider.getModel( IFlowRelationshipModel.class.getName(), IFlowRelationshipModel.class );
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
        final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();
        m_scenarioMetaData = caseDataProvider.getModel( IScenarioResultMeta.class.getName(), IScenarioResultMeta.class );
      }
      catch( Exception e )
      {
      }
      if( m_scenarioMetaData == null )
      {
        final URL resultMetaURL = (URL) inputProvider.getInputForID( PreSWANKalypso.INPUT_PATH_RESULT_META );
        final GMLWorkspace resultMetaWorkspace = GmlSerializer.createGMLWorkspace( resultMetaURL, null );
        m_scenarioMetaData = (IScenarioResultMeta) resultMetaWorkspace.getRootFeature().getAdapter( IScenarioResultMeta.class );
      }

      try
      {
        final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();
        m_windRelationshipModel = caseDataProvider.getModel( IWindModel.class.getName(), IWindModel.class );
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

      if( inputProvider.hasID( ADDITIONAL_DATA_FILE ) )
        m_additionalDataURL = (URL) inputProvider.getInputForID( ADDITIONAL_DATA_FILE );

      final FileObject lFileObjWorkingDir = manager.toFileObject( tmpdir );

      writeSWANFiles( lFileObjWorkingDir, lFileObjPreResultsDir, progressMonitor );

      resultEater.addResult( OUTPUT_PATH_SWAN, new File( tmpdir.toURI() ) );

    }
    catch( final Exception e )
    {
      throw new SimulationException( "Problem running PreRMAKalypso", e );
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
    try
    {
      ProgressUtilities.worked( subProgress, 1 );
    }
    catch( CoreException e1 )
    {
      e1.printStackTrace();
    }

    /* Process all remaining .2d files. */
    Map<Date, FileObject> lMapDatesFiles = pResultManager.getDateFileMap();

    String lStrFileNameWLData = ISimulation1D2DConstants.SIM_SWAN_WATER_LEVEL_DATA_FILE;
    String lStrFileNameCurrentData = ISimulation1D2DConstants.SIM_SWAN_CURRENT_DATA_FILE;

    lStrFileNameWLData += ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
    lStrFileNameCurrentData += ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
    FileObject lModelWLFile = pFileObjWorkingDir.resolveFile( lStrFileNameWLData );
    FileObject lModelCurrentFile = pFileObjWorkingDir.resolveFile( lStrFileNameCurrentData );
    Formatter lFormatterWLData = new Formatter( lModelWLFile.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
    Formatter lFormatterCurrentData = new Formatter( lModelCurrentFile.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );

    for( int i = 0; i < m_calculatedSteps.length; i++ )
    {
      RMA10S2GmlConv lRMA2GmlConverter = new RMA10S2GmlConv();
      SimpleNodeResultsHandler lRMAResultsSimpleHandler = new SimpleNodeResultsHandler( m_mapGMPositions );
      lRMA2GmlConverter.setRMA10SModelElementHandler( lRMAResultsSimpleHandler );
      final FileObject lResultRMAFile = lMapDatesFiles.get( m_calculatedSteps[i] );

      lRMA2GmlConverter.parse( lResultRMAFile.getContent().getInputStream() );
      lRMAResultsSimpleHandler.updateLastRecordForDate( m_calculatedSteps[i] );

      final SWANAdditionalDataConverter lRma2SwanConverter = new SWANAdditionalDataConverter( pResultManager, lRMAResultsSimpleHandler, pFileObjWorkingDir, new Date[] { m_calculatedSteps[i] } );
      Map<String, List<String>> lMapWrittenFiles = lRma2SwanConverter.writeDataFiles();
      for( final String lStrWLFileName : lMapWrittenFiles.get( SWANAdditionalDataConverter.WATER_LEVEL_SERIES_NAMES_KEY ) )
      {
        lFormatterWLData.format( "%s\n", lStrWLFileName );//$NON-NLS-1$
      }
      for( final String lStrCurrentFileName : lMapWrittenFiles.get( SWANAdditionalDataConverter.CURRENTS_SERIES_NAMES_KEY ) )
      {
        lFormatterCurrentData.format( "%s\n", lStrCurrentFileName );//$NON-NLS-1$
      }

      try
      {
        ProgressUtilities.worked( subProgress, 100 / m_calculatedSteps.length );
      }
      catch( CoreException e )
      {
        m_log.log( StatusUtilities.statusFromThrowable( e ) );
        // e.printStackTrace();
      }
    }
    lFormatterWLData.close();
    lFormatterCurrentData.close();
  }

  private void writeContolFile( FileObject pFileObjWorkingDir, ResultManager pResultManager, SWANWindDataWriter pWindWriter ) throws FileSystemException, IOException
  {
    final FileObject lSWANControlFile = pFileObjWorkingDir.resolveFile( ISimulation1D2DConstants.SIM_SWAN_CONTROL_FILE );
    final Control1D2DConverterSWAN controlConverter = new Control1D2DConverterSWAN( pFileObjWorkingDir, m_discretisationModel, m_controlModel, m_flowRelationshipModel, m_log, pResultManager, m_mapContiLineWithSWANBoundaryToCondition, pWindWriter.getListWritenDates(), pWindWriter.getWrittenGrid(), m_doubleShiftX, m_doubleShiftY );
    controlConverter.writeControlFile( lSWANControlFile.getContent().getOutputStream() );
    lSWANControlFile.close();
  }

  private void writeMesh( final FileObject pFileObjWorkingDir ) throws FileSystemException
  {
    final FileObject lModelNodesFile = pFileObjWorkingDir.resolveFile( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".node" );//$NON-NLS-1$
    final FileObject lModelElementsFile = pFileObjWorkingDir.resolveFile( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".ele" );//$NON-NLS-1$
    final FileObject lModelBotFile = pFileObjWorkingDir.resolveFile( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".bot" );//$NON-NLS-1$
    final FileObject lModelPosFile = pFileObjWorkingDir.resolveFile( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".txt" );//$NON-NLS-1$

    /* .2d Mesh */
    final Gml2SWANConv converter2D = new Gml2SWANConv( m_discretisationModel, m_flowRelationshipModel, m_controlModel.getCalculationUnit(), m_log );

    URL lUrlFileAdditionalCoord = m_additionalDataURL;
    if( lUrlFileAdditionalCoord == null )
    {
      try
      {
        lUrlFileAdditionalCoord = new URL( m_controlModel.getInputFileAdditionalCoordSWAN() );
      }
      catch( MalformedURLException e )
      {
        // cannot interpret given URL or file
      }
    }
    FileObject lAdditionalCoordFile = null;
    List<GM_Position> lListAdditionalCoord = null;
    if( lUrlFileAdditionalCoord != null && !"".equals( lUrlFileAdditionalCoord ) )
    {
      if( lUrlFileAdditionalCoord.getFile().endsWith( ".zip" ) )
      {
        try
        {
          lListAdditionalCoord = readAdditionalCoordinates( ZipUtilities.getInputStreamForSingleFile( lUrlFileAdditionalCoord, lUrlFileAdditionalCoord.getQuery() ) );
        }
        catch( Exception e )
        {
        }
      }
      else
      {
        try
        {
          if( lAdditionalCoordFile == null )
          {
            lListAdditionalCoord = readAdditionalCoordinates( lUrlFileAdditionalCoord.openStream() );
          }
          else
          {
            lListAdditionalCoord = readAdditionalCoordinates( lAdditionalCoordFile.getContent().getInputStream() );
          }
          converter2D.setListAdditionalOuputCoord( lListAdditionalCoord );
        }
        catch( IOException e )
        {
          // cannot interpret given URL or file
          // e.printStackTrace();
        }
        catch( Exception e )
        {
        }
      }
    }
    m_mapGMPositions = converter2D.writeSWANModel( lModelNodesFile.getContent().getOutputStream(), lModelElementsFile.getContent().getOutputStream(), lModelBotFile.getContent().getOutputStream(), lModelPosFile.getContent().getOutputStream() );
    m_doubleShiftX = converter2D.getDoubleGlobalMinX();
    m_doubleShiftY = converter2D.getDoubleGlobalMinY();
    m_mapContiLineWithSWANBoundaryToCondition = converter2D.getMapContiLineWithSWANBoundaryToCondition();

    lModelNodesFile.close();
    lModelElementsFile.close();
    lModelBotFile.close();
    lModelPosFile.close();
  }

  private List<GM_Position> readAdditionalCoordinates( final InputStream lAdditionalCoordInputStream )
  {
    List<GM_Position> lListPositions = new ArrayList<GM_Position>();
    try
    {
      Scanner scannerFile = new Scanner( lAdditionalCoordInputStream );
      while( scannerFile.hasNextLine() )
      {
        String lStrNextLine = scannerFile.nextLine();
        Scanner scannerLine = new Scanner( lStrNextLine );
        scannerLine.useDelimiter( " " ); //$NON-NLS-1$
        String lStrX = scannerLine.next();
        String lStrY = scannerLine.next();
        double doubleX = NumberUtils.parseQuietDouble( lStrX );
        double doubleY = NumberUtils.parseQuietDouble( lStrY );
        scannerLine.close();
        GM_Position lPosition = GeometryFactory.createGM_Position( doubleX, doubleY );
        lListPositions.add( lPosition );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
    return lListPositions;
  }

  private void writeSWANFiles( final FileObject pFileObjWorkingDir, final FileObject pFileObjPreResultsDir, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    ProgressUtilities.worked( progress, 1 );
    progress.beginTask( "", 100 );
    try
    {
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.3" ) ); //$NON-NLS-1$

      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.4" ) ); //$NON-NLS-1$
      //progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.5" ) ); //$NON-NLS-1$
      // ProgressUtilities.worked( progress, 20 );

      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.6" ) ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.7" ) ); //$NON-NLS-1$
      writeMesh( pFileObjWorkingDir );

      ProgressUtilities.worked( progress, 10 );

      /* RMA Results parsed and written as input files for SWAN */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.9" ) ); //$NON-NLS-1$

      final ResultManager resultManager = new ResultManager( pFileObjPreResultsDir, pFileObjWorkingDir, m_discretisationModel, m_controlModel, m_flowRelationshipModel, m_scenarioMetaData, m_log );

      m_calculatedSteps = resultManager.findCalculatedSteps();
      resultManager.setStepsToProcess( m_calculatedSteps, resultManager.getControlModel() );
      if( m_controlModel.isUnsteadySelected() )
      {
        m_calculatedSteps = SWANAdditionalDataConverter.removeSteadyDates( m_calculatedSteps, null );
      }
      else
      {
        m_calculatedSteps = SWANAdditionalDataConverter.removeSteadyDates( m_calculatedSteps, ISimulation1D2DConstants.MAXI_DATE );
      }
      ProgressUtilities.worked( progress, 10 );

      writeAdditionaData( pFileObjWorkingDir, resultManager, progress );

      ProgressUtilities.worked( progress, 60 );
      SWANWindDataWriter lWindWriter = writeWindData( pFileObjWorkingDir );

      ProgressUtilities.worked( progress, 10 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.9" ) ); //$NON-NLS-1$
      writeContolFile( pFileObjWorkingDir, resultManager, lWindWriter );

      ProgressUtilities.worked( progress, 10 );

    }
    catch( final Exception e )
    {
      // e.printStackTrace();
      final String msg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, e ) );
    }
    finally
    {
      progress.done();
    }
  }

  private SWANWindDataWriter writeWindData( FileObject pFileObjWorkingDir ) throws IOException
  {
    GM_Envelope lGmEnvelope = CalcUnitOps.getBoundingBox( m_controlModel.getCalculationUnit() );
    SWANWindDataWriter lWindWriter = new SWANWindDataWriter( pFileObjWorkingDir, lGmEnvelope, m_calculatedSteps, m_windRelationshipModel.getWindDataModelSystems() );
    lWindWriter.setWindDataModel( m_windRelationshipModel );
    lWindWriter.write( m_controlModel.isConstantWindSWAN() );
    return lWindWriter;
  }
}
