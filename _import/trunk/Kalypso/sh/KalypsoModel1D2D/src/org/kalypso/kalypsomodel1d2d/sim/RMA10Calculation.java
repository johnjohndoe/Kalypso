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
import java.net.URL;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.contribs.java.lang.ProgressCancelable;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.WQboundaryConditions1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Represents a calculation with a rma10s.exe. Helps generation of the ASCII input files and to start the process.
 */
public class RMA10Calculation implements ISimulation1D2DConstants
{
  public static final String MODEL_2D = "model.2d";

  /** Name of rma10s.exe used for calculation */
  private static final String RMA10S_KALYPSO_EXE = "RMA10sKalypso.exe";

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final IFlowRelationshipModel m_flowRelationshipModel;

  private final IRoughnessClsCollection m_roughnessModel;

  private final IControlModel1D2D m_controlModel;

  private final File m_tmpDir;

  private final IGeoLog m_log;

  private final IContainer m_scenarioFolder;

  private IStatus m_simulationStatus;

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

  private String getKalypso1D2DKernelPath( ) throws CoreException
  {
    final String version = m_controlModel.getVersion();

    if( version.equals( "Version3.5" ) )
      return ISimulation1D2DConstants.SIM_EXE_FILE_3_5;

    throw new CoreException( StatusUtilities.createErrorStatus( "Rechenkern-Version nicht unterstützt: " + version ) );
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

    m_log.log( m_simulationStatus );

    return m_simulationStatus;
  }

  private IStatus doRunCalculation( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      writeRma10Files( progress.newChild( 10 ) );
      copyExecutable( progress.newChild( 1 ) );
      return startCalcCore( progress.newChild( 89 ) );
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
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_RUNNING, "Simulation mit Fehler beendet. Ergebnisauswertung nicht möglich.", null );

    // if( simulationStatus.matches( IStatus.WARNING ) )
    return StatusUtilities.createStatus( IStatus.WARNING, CODE_RUNNING, "Simulation mit Warnung beendet.", null );
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
      final RestartNodes m_restartNodes = RestartNodes.createRestartNodes( m_scenarioFolder, m_controlModel );
      ProgressUtilities.worked( progress, 20 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe Finite Elemente Netz" );
      monitor.subTask( "Schreibe ASCII-Daten: Finite Elemente Netz..." );
      final File modelFile = new File( m_tmpDir, MODEL_2D );
      final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( m_discretisationModel, m_flowRelationshipModel, calculationUnit, m_roughnessModel, m_restartNodes, false, true );
      converter2D.writeRMA10sModel( modelFile );
      ProgressUtilities.worked( progress, 20 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Schreibe Randbedingungen und Berechnungssteuerung" );
      progress.subTask( "Schreibe ASCII-Daten: Randbedingungen und Berechnungssteuerung..." );
      final File r10file = new File( m_tmpDir, ISimulation1D2DConstants.R10_File );
      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( m_controlModel, m_flowRelationshipModel, m_roughnessModel, converter2D, buildingProvider );
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

  private void copyExecutable( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Kopiere Rechenkern in temporäres Verzeichnis..." );
      monitor.subTask( "Kopiere Rechenkern in temporäres Verzeichnis..." );

      final String exeResource = ISimulation1D2DConstants.RMA10S_BASE + getKalypso1D2DKernelPath();
      final File destFile = new File( m_tmpDir, RMA10S_KALYPSO_EXE );
      final URL exeUrl = getClass().getResource( exeResource );
      FileUtils.copyURLToFile( exeUrl, destFile );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Fehler beim Kopieren der rma10s.exe aus den Programm-Resourcen: %s", e.toString() );
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, CODE_PRE, msg, e ) );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }

  }

  /**
   * Runs the rma10s simulation, i.e. starts the rma10s.exe in the temp-dir and waits until the process returns.
   */
  private IStatus startCalcCore( final IProgressMonitor monitor ) throws CoreException
  {
    final IObservation<TupleResult> obs = m_controlModel.getTimeSteps();
    final TupleResult timeSteps = obs.getResult();
    final int numberOfSteps = timeSteps.size(); // TODO: adjust by restart?!

    final SubMonitor progress = SubMonitor.convert( monitor, numberOfSteps );
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
      // Run the Calculation
      logOS = new FileOutputStream( new File( m_tmpDir, "exe.log" ) );
      errorOS = new FileOutputStream( new File( m_tmpDir, "exe.err" ) );

      final File exeFile = new File( m_tmpDir, RMA10S_KALYPSO_EXE );
      final String commandString = exeFile.getAbsolutePath();
      final ICancelable progressCancelable = new ProgressCancelable( progress );
      // TODO: somehow monitor progress of executable via its output...

      final ItrReadJob itrReadJob = new ItrReadJob( "Read Output.itr", new File( resultDir, "Output.itr" ), numberOfSteps, itrDir, progress );
      itrReadJob.setPriority( Job.INTERACTIVE );
      itrReadJob.setSystem( true );
      itrReadJob.schedule();

      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "RMA10s wird ausgeführt: %s", commandString );
      ProcessHelper.startProcess( commandString, new String[0], m_tmpDir, progressCancelable, 0, logOS, errorOS, null, 250, null );

      // TODO: read other outputs: error-log

      try
      {
        /* Stop the itr read job and then read the file one last time, it should be complete now... */
        itrReadJob.cancel();
        itrReadJob.join();
        itrReadJob.readFile();
      }
      catch( final InterruptedException e )
      {
        final IStatus itrStatus = StatusUtilities.createStatus( IStatus.WARNING, "Fehler beim Auswerten des Iterations-Log", e );
        m_log.log( itrStatus );
      }

      // Check for success
      final File errorFile = findErrorFile( m_tmpDir );
      if( errorFile == null )
        return StatusUtilities.createOkStatus( "Simulation wurde erfolgreich beendet." );

      /* ERROR: return contents of error file as error message */
      final String errorMessage = FileUtils.readFileToString( errorFile, Charset.defaultCharset().name() );
      final IStatus errorStatus = errorToStatus( errorMessage );

      final String message = String.format( "Fehlermeldung vom Rechenkern (ERROR.DAT):%n%s", errorStatus.getMessage() );
      return StatusUtilities.createStatus( IStatus.WARNING, CODE_RMA10S, message, null );
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, "Fehler beim Ausführen der " + RMA10S_KALYPSO_EXE, e );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      ProgressUtilities.done( progress );
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

    if( errorDatFile.exists() )
      return errorDatFile;

    if( errorOutFile.exists() )
      return errorOutFile;

    return null;
  }

  private IStatus errorToStatus( final String errorMessage )
  {
    final String[] lines = errorMessage.split( "\n" );
    if( lines.length != 7 )
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, "RMA10s Fehlerausgabe konnte nicht geparst werden: " + errorMessage, null );

    final int severity = IStatus.ERROR; // at the moment, all outputs are ERROR's
    final String message = lines[0];

    final GM_Object location;
    if( lines[5].length() < 52 )
      location = null;
    else
    {
      final BigDecimal rw = new BigDecimal( lines[5].substring( 13, 26 ).trim() );
      final BigDecimal hw = new BigDecimal( lines[5].substring( 38, 51 ).trim() );

      location = GeometryFactory.createGM_Point( rw.doubleValue(), hw.doubleValue(), KalypsoCorePlugin.getDefault().getCoordinatesSystem() );
    }

    return m_log.log( severity, CODE_RMA10S, message, location, null );
  }

}
