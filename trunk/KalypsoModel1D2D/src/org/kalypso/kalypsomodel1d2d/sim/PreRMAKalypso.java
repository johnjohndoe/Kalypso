package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.WQboundaryConditions1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Convert from GML to RMAKalypso format
 * 
 * @author kurzbach
 */
public class PreRMAKalypso implements ISimulation
{

  public static final String INPUT_RESTART_FILE_PREFIX = "restartFile"; //$NON-NLS-1$

  public static final String INPUT_ROUGHNESS = "roughness"; //$NON-NLS-1$

  public static final String INPUT_FLOW_RELATIONSHIPS = "flowRelationships"; //$NON-NLS-1$

  public static final String INPUT_CALCULATION_UNIT_ID = "calculationUnitID"; //$NON-NLS-1$

  public static final String INPUT_MESH = "mesh"; //$NON-NLS-1$

  public static final String INPUT_CONTROL = "control"; //$NON-NLS-1$

  public static final String OUTPUT_MESH = ISimulation1D2DConstants.MODEL_2D;

  public static final String OUTPUT_BC_WQ = ISimulation1D2DConstants.BC_WQ_File;

  public static final String OUTPUT_BUILDINGS = ISimulation1D2DConstants.BUILDING_File;

  public static final String OUTPUT_CONTROL = ISimulation1D2DConstants.R10_File;

  public static final String OUTPUT_RMA_VERSION = "rmaVersion"; //$NON-NLS-1$

  private static final String MODEL_SPEC = "resource/preRMAKalypso.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.simulation.rma.preRMAKalypso"; //$NON-NLS-1$

  private IGeoLog m_log;

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
    catch( final GMLSchemaException e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    final OutputStream logOS = null;
    final OutputStream errorOS = null;
    FileSystemManagerWrapper manager = null;
    try
    {
      manager = VFSUtilities.getNewManager();

      final URL controlUrl = (URL) inputProvider.getInputForID( INPUT_CONTROL );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlUrl, null );
      final IControlModelGroup controlModelGroup = (IControlModelGroup) controlWorkspace.getRootFeature().getAdapter( IControlModelGroup.class );
      final IControlModel1D2D controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

      final URL meshUrl = (URL) inputProvider.getInputForID( INPUT_MESH );
      final GMLWorkspace discWorkspace = GmlSerializer.createGMLWorkspace( meshUrl, null );
      final IFEDiscretisationModel1d2d discretisationModel = (IFEDiscretisationModel1d2d) discWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );

      // specified calculation unit overrides control model calc unit
      ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
      if( inputProvider.hasID( INPUT_CALCULATION_UNIT_ID ) )
      {
        final String calcUnitID = (String) inputProvider.getInputForID( INPUT_CALCULATION_UNIT_ID );
        if( calculationUnit instanceof ICalculationUnit1D2D )
        {
          final IFeatureWrapperCollection<ICalculationUnit> changedSubUnits = ((ICalculationUnit1D2D) calculationUnit).getChangedSubUnits();
          for( final ICalculationUnit subUnit : changedSubUnits )
          {
            if( subUnit.getGmlID().equals( calcUnitID ) )
              calculationUnit = subUnit;
          }
        }
      }

      final URL flowRelURL = (URL) inputProvider.getInputForID( INPUT_FLOW_RELATIONSHIPS );
      final GMLWorkspace flowRelWorkspace = GmlSerializer.createGMLWorkspace( flowRelURL, null );
      final IFlowRelationshipModel flowRelationshipModel = (IFlowRelationshipModel) flowRelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );

      final URL roughnessURL = (URL) inputProvider.getInputForID( INPUT_ROUGHNESS );
      final GMLWorkspace roughnessWorkspace = GmlSerializer.createGMLWorkspace( roughnessURL, null );
      final IRoughnessClsCollection roughnessModel = (IRoughnessClsCollection) roughnessWorkspace.getRootFeature().getAdapter( IRoughnessClsCollection.class );

      final RestartNodes restartNodes;
      if( controlModel.getRestart() )
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

      final FileObject workingDir = manager.toFileObject( tmpdir );
      writeRma10Files( workingDir, progressMonitor, discretisationModel, flowRelationshipModel, roughnessModel, restartNodes, controlModel, calculationUnit );

      resultEater.addResult( OUTPUT_MESH, new File( tmpdir, OUTPUT_MESH ) );
      resultEater.addResult( OUTPUT_CONTROL, new File( tmpdir, OUTPUT_CONTROL ) );
      resultEater.addResult( OUTPUT_BUILDINGS, new File( tmpdir, OUTPUT_BUILDINGS ) );
      resultEater.addResult( OUTPUT_BC_WQ, new File( tmpdir, OUTPUT_BC_WQ ) );
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

  private void writeRma10Files( final FileObject workingDir, final IProgressMonitor monitor, final IFEDiscretisationModel1d2d discretisationModel, final IFlowRelationshipModel flowRelationshipModel, final IRoughnessClsCollection roughnessModel, final RestartNodes restartNodes, final IControlModel1D2D controlModel, final ICalculationUnit calculationUnit ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.3" ) ); //$NON-NLS-1$

      /* Read restart data */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.4" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.5" ) ); //$NON-NLS-1$

      ProgressUtilities.worked( progress, 10 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.6" ) ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.7" ) ); //$NON-NLS-1$
      final FileObject modelFile = workingDir.resolveFile( OUTPUT_MESH );

      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( discretisationModel, flowRelationshipModel, calculationUnit, roughnessModel, restartNodes, false, true, m_log );
      converter2D.writeRMA10sModel( modelFile.getContent().getOutputStream() );
      modelFile.close();
      ProgressUtilities.worked( progress, 60 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.9" ) ); //$NON-NLS-1$
      final FileObject r10file = workingDir.resolveFile( OUTPUT_CONTROL );
      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( controlModel, calculationUnit, flowRelationshipModel, roughnessModel, converter2D, buildingProvider, m_log );
      controlConverter.writeR10File( r10file.getContent().getOutputStream() );
      r10file.close();
      ProgressUtilities.worked( progress, 10 );

      /* Building File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.10" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.11" ) ); //$NON-NLS-1$
      final FileObject buildingFile = workingDir.resolveFile( OUTPUT_BUILDINGS );
      final Building1D2DConverter buildingConverter = new Building1D2DConverter( buildingProvider );
      buildingConverter.writeBuildingFile( buildingFile.getContent().getOutputStream() );
      buildingFile.close();
      ProgressUtilities.worked( progress, 10 );

      /* W/Q BC File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.12" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.13" ) ); //$NON-NLS-1$
      final FileObject bcWQFile = workingDir.resolveFile( OUTPUT_BC_WQ );
      final WQboundaryConditions1D2DConverter bc1D2DConverter = new WQboundaryConditions1D2DConverter( controlConverter.getBoundaryConditionsIDProvider() );
      bc1D2DConverter.writeWQbcFile( bcWQFile.getContent().getOutputStream() );
      bcWQFile.close();
      ProgressUtilities.worked( progress, 10 );
    }
    catch( final IOException e )
    {
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.14", e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, e ) );
    }
  }

}
