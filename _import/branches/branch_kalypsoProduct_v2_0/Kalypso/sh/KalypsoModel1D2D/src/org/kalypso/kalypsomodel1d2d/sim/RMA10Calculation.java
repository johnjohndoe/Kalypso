/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.contribs.java.lang.ProgressCancelable;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.WQboundaryConditions1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Represents a calculation with a rma10s.exe. Helps generation of the ASCII input files and to start the process.
 */
public class RMA10Calculation implements ISimulation1D2DConstants
{
  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final IFlowRelationshipModel m_flowRelationshipModel;

  private final IRoughnessClsCollection m_roughnessModel;

  private final IControlModel1D2D m_controlModel;

  private final File m_tmpDir;

  private final IGeoLog m_log;

  private final IContainer m_scenarioFolder;

  private IStatus m_simulationStatus;

  private IterationInfo m_iterationInfo;

  public RMA10Calculation( final File tmpDir, final IGeoLog geoLog, final IFEDiscretisationModel1d2d discModel, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IRoughnessClsCollection roughnessModell, final IContainer folder )
  {
    m_tmpDir = tmpDir;
    m_log = geoLog;
    m_discretisationModel = discModel;
    m_flowRelationshipModel = flowModel;
    m_roughnessModel = roughnessModell;
    m_controlModel = controlModel;
    m_scenarioFolder = folder;
  }

  public IFlowRelationshipModel getFlowModel( )
  {
    return m_flowRelationshipModel;
  }

  public IFEDiscretisationModel1d2d getDiscModel( )
  {
    return m_discretisationModel;
  }

  public IControlModel1D2D getControlModel( )
  {
    return m_controlModel;
  }

  /**
   * Runs < rma10s calculation. The following steps are processed:
   * <ul>
   * <li>write rma10s ASCII files to temporary directory according to provided gml-models</li>
   * <li>write .exe to temporary directory</li>
   * <li>execute the .exe</li>
   * <li>read .2d files and process them to the output directory</li>
   * </ul>
   */
  public IStatus runCalculation( final IProgressMonitor monitor )
  {
    m_log.formatLog( IStatus.INFO, CODE_RUNNING, "Start der Simulation" );

    final IStatus simulationStatus = doRunCalculation( monitor );
    m_simulationStatus = evaluateSimulationResult( simulationStatus );

    // check if status is allready in collection
    final IStatusCollection statusCollection = m_log.getStatusCollection();
    if( !statusCollection.contains( simulationStatus ) )
      m_log.log( m_simulationStatus );

    return m_simulationStatus;
  }

  private IStatus doRunCalculation( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      writeRma10Files( progress.newChild( 10 ) );
      return startCalcCore( progress.newChild( 90 ) );
    }
    catch( final CoreException ce )
    {
      return ce.getStatus();
    }
  }

  private IStatus evaluateSimulationResult( final IStatus simulationStatus )
  {
    // TODO: distinguish different type of problems:
    // - exe/data errors: calculation could not be started at all
    // - no results
    // - some results

    // TODO: show dialog to user where he can
    // - choose, which steps to process
    // - choose, which results to be deleted

    if( simulationStatus.isOK() )
      return StatusUtilities.createStatus( IStatus.OK, CODE_RUNNING, "Simulation erfolgreich beendet", null );

    if( simulationStatus.matches( IStatus.CANCEL ) )
      return StatusUtilities.createStatus( IStatus.CANCEL, CODE_RUNNING, "Simulation durch Benutzer abgebrochen.", null );

    if( simulationStatus.matches( IStatus.ERROR ) )
      return simulationStatus;

    // if( simulationStatus.matches( IStatus.WARNING ) )
    return simulationStatus; // StatusUtilities.createStatus( IStatus.WARNING, CODE_RUNNING, "Simulation mit Warnung
    // beendet.",
    // null );
  }

  private void writeRma10Files( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe RMA10s-ASCII Dateien für FE-Simulation" );

      /* Read restart data */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Restart-Daten werden gelesen" );
      progress.subTask( "Schreibe ASCII-Daten: Restart-Daten werden gelesen..." );

      RestartNodes m_restartNodes;

      if( m_controlModel.getRestart() )
        m_restartNodes = RestartNodes.createRestartNodes( m_scenarioFolder, m_controlModel );
      else
        m_restartNodes = null;

      ProgressUtilities.worked( progress, 20 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe Finite Elemente Netz" );
      monitor.subTask( "Schreibe ASCII-Daten: Finite Elemente Netz..." );
      final File modelFile = new File( m_tmpDir, MODEL_2D );
      final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( m_discretisationModel, m_flowRelationshipModel, calculationUnit, m_roughnessModel, m_restartNodes, false, true, m_log );
      converter2D.writeRMA10sModel( modelFile );
      ProgressUtilities.worked( progress, 20 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe Randbedingungen und Berechnungssteuerung" );
      progress.subTask( "Schreibe ASCII-Daten: Randbedingungen und Berechnungssteuerung..." );
      final File r10file = new File( m_tmpDir, ISimulation1D2DConstants.R10_File );
      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( m_controlModel, m_flowRelationshipModel, m_roughnessModel, converter2D, buildingProvider, m_log );
      controlConverter.writeR10File( r10file );
      ProgressUtilities.worked( progress, 20 );

      /* Building File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe Bauwerke" );
      progress.subTask( "Schreibe ASCII-Daten: Bauwerke..." );
      final File buildingFile = new File( m_tmpDir, ISimulation1D2DConstants.BUILDING_File );
      final Building1D2DConverter buildingConverter = new Building1D2DConverter( buildingProvider );
      buildingConverter.writeBuildingFile( buildingFile );
      ProgressUtilities.worked( progress, 20 );

      /* W/Q BC File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe W/Q-Randbedingungen" );
      progress.subTask( "Schreibe ASCII-Daten: W/Q-Randbedingungen..." );
      final File bcWQFile = new File( m_tmpDir, ISimulation1D2DConstants.BC_WQ_File );
      final WQboundaryConditions1D2DConverter bc1D2DConverter = new WQboundaryConditions1D2DConverter( controlConverter.getBoundaryConditionsIDProvider() );
      bc1D2DConverter.writeWQbcFile( bcWQFile );
      ProgressUtilities.worked( progress, 20 );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Fehler beim Schreiben einer RMA10s-ASCII Datei: %s", e.getLocalizedMessage() );
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, CODE_PRE, msg, e ) );
    }
    finally
    {
      progress.done();
    }
  }

  /**
   * Runs the rma10s simulation, i.e. starts the rma10s.exe in the temp-dir and waits until the process returns.
   */
  private IStatus startCalcCore( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, m_controlModel.getNCYC() );
    progress.subTask( "RMA10s wird ausgeführt..." );

    /* Create the result folder for the .exe file, must be same as in Control-Converter */
    final File resultDir = new File( m_tmpDir, Control1D2DConverter.RESULT_DIR_NAME );
    final File itrDir = new File( m_tmpDir, "iterObs" );
    resultDir.mkdirs();
    itrDir.mkdirs();

    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      final File exeFile = findRma10skExe();

      final String commandString = exeFile.getAbsolutePath();

      // Run the Calculation
      logOS = new FileOutputStream( new File( m_tmpDir, "exe.log" ) );
      errorOS = new FileOutputStream( new File( m_tmpDir, "exe.err" ) );

      final ICancelable progressCancelable = new ProgressCancelable( progress );
      final Runnable idleRunnable = new Runnable()
      {
        public void run( )
        {
          updateIteration( progress );
        }
      };

      /* Initialize Iteration Job */
      m_iterationInfo = new IterationInfo( new File( resultDir, OUTPUT_ITR ), itrDir, m_controlModel );

      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "RMA10s wird ausgeführt: %s", commandString );
      ProcessHelper.startProcess( commandString, new String[0], m_tmpDir, progressCancelable, 0, logOS, errorOS, null, 250, idleRunnable );

      // TODO: specific error message if exe was not found

      // TODO: read other outputs:
      // - error-log
      // - border conditions-log

      /* Update the iteration one last time, it should be complete now... */
      updateIteration( progress );
      m_iterationInfo.finish(); // save the last observation

      // Check for success
      final File errorFile = findErrorFile( m_tmpDir );
      if( errorFile == null )
        return StatusUtilities.createOkStatus( "Simulation wurde erfolgreich beendet." );

      /* ERROR: return contents of error file as error message */
      final String errorMessage = FileUtils.readFileToString( errorFile, Charset.defaultCharset().name() );
      final IStatus errorStatus = errorToStatus( errorMessage );

      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_RMA10S, new IStatus[] { errorStatus }, "Fehlermeldung vom Rechenkern (ERROR.DAT)", null );
    }
    catch( final CoreException ce )
    {
      throw ce;
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, "Fehler beim Ausführen von RMA10SK", e );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      ProgressUtilities.done( progress );
    }
  }

  private File findRma10skExe( ) throws CoreException
  {
    // Determine exe filename
    final String version = m_controlModel.getVersion();
    if( version == null || version.length() == 0 )
      // REMARK: maybe could instead use a default or the one with the biggest version number?
      throw new CoreException( StatusUtilities.createErrorStatus( "RMA10SK Version nicht gesetzt. Wählen Sie die Version in den Berechnungseinstellungen." ) );

    final String exeName = ISimulation1D2DConstants.SIM_EXE_FILE_PREFIX + version + ".exe";

    // REMARK: This is OS dependent; we use should use a pattern according to OS
    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    final File exeDir = new File( installDir, "bin" );
    final File exeFile = new File( exeDir, exeName );
    if( exeFile.exists() )
      return exeFile;

    final String exeMissingMsg = String.format( "Die Ausführbare Datei (%s) ist nicht vorhanden.\nRMA10SK ist nicht Teil von Kalypso sondern muss gesondert erworden werden. Weitere Informationen finden Sie unter http://kalypso.sourceforge.net.", exeFile.getAbsolutePath() );
    throw new CoreException( StatusUtilities.createErrorStatus( exeMissingMsg ) );
  }

  /**
   * Will be called while the rma10s process is running.<br>
   * Updates the calculation progress monitor and reads the Output.itr.
   */
  protected void updateIteration( final IProgressMonitor monitor )
  {
    final int oldStepNr = m_iterationInfo.getStepNr();

    m_iterationInfo.readIterFile();

    final int stepNr = m_iterationInfo.getStepNr();
    if( oldStepNr != stepNr )
    {
      String msg = "";
      if( stepNr == 0 )
        msg = String.format( "RMA10s wird ausgeführt - stationärer Schritt" );
      else
        msg = String.format( "RMA10s wird ausgeführt - instationärer Schritt %d (%d)", stepNr, m_controlModel.getNCYC() );
      monitor.subTask( msg );
      monitor.worked( stepNr - oldStepNr );
    }
  }

  /**
   * Checks, if an error file exists and returns it.
   * 
   * @return <code>null</code>, if no existing error file was found.
   */
  private static File findErrorFile( final File dir )
  {
    final File errorDatFile = new File( dir, "ERROR.DAT" );
    final File errorOutFile = new File( dir, "ERROR.OUT" );
    final File errorErrFile = new File( dir, "exe.err" );

    if( errorDatFile.exists() )
      return errorDatFile;

    if( errorOutFile.exists() )
      return errorOutFile;

    if( errorErrFile.exists() && errorErrFile.length() > 0 )
      return errorErrFile;

    return null;
  }

  private IStatus errorToStatus( final String errorMessage )
  {
    // TODO: error or warning depends, if any steps where calculated; the rma10s should determine if result processing
    // makes sense

    final String[] lines = errorMessage.split( "\n" );
    if( lines.length != 7 )
      return StatusUtilities.createStatus( IStatus.WARNING, CODE_RMA10S, errorMessage, null );

    final int severity = IStatus.WARNING;
    final String message = lines[0];

    final GM_Object location;
    if( lines[5].length() < 52 )
      location = null;
    else
    {
      final BigDecimal rw = new BigDecimal( lines[5].substring( 13, 26 ).trim() );
      final BigDecimal hw = new BigDecimal( lines[5].substring( 38, 51 ).trim() );

      location = GeometryFactory.createGM_Point( rw.doubleValue(), hw.doubleValue(), KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    }

    return m_log.log( severity, CODE_RMA10S, message, location, null );
  }

  public IterationInfo getIterationInfo( )
  {
    return m_iterationInfo;
  }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

}
