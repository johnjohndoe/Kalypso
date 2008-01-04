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
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.contribs.java.lang.ProgressCancelable;
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

/**
 * 
 */
public class RMA10Calculation
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

  private final IFolder m_scenarioFolder;

  public RMA10Calculation( final File tmpDir, final IGeoLog geoLog, final IFEDiscretisationModel1d2d discModel, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IRoughnessClsCollection roughnessModell, final IFolder scenarioFolder )
  {
    m_tmpDir = tmpDir;
    m_log = geoLog;
    m_discretisationModel = discModel;
    m_flowRelationshipModel = flowModel;
    m_roughnessModel = roughnessModell;
    m_controlModel = controlModel;
    m_scenarioFolder = scenarioFolder;
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
      return RMA10SimModelConstants.SIM_EXE_FILE_3_5;

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
  public IStatus runCalculation( final IProgressMonitor monitor ) throws CoreException
  {
    final String simMsg = String.format( "Simulation von '%s'", m_controlModel.getName() );
    final SubMonitor progress = SubMonitor.convert( monitor, simMsg, 100 );

    writeRma10Files( progress.newChild( 10 ) );
    copyExecutable( progress.newChild( 1 ) );
    return startCalcCore( progress.newChild( 89 ) );
  }

  private void writeRma10Files( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      m_log.formatLog( IStatus.INFO, "Schreibe RMA10s-ASCII Dateien für FE-Simulation" );

      /* Read restart data */
      m_log.formatLog( IStatus.INFO, "Restart-Daten werden gelesen" );
      progress.subTask( "Schreibe ASCII-Daten: Restart-Daten werden gelesen..." );
      final RestartNodes m_restartNodes = RestartNodes.createRestartNodes( m_scenarioFolder, m_controlModel );
      ProgressUtilities.worked( progress, 20 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, "Schreibe Finite Elemente Netz" );
      monitor.subTask( "Schreibe ASCII-Daten: Finite Elemente Netz..." );
      final File modelFile = new File( m_tmpDir, MODEL_2D );
      final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( m_discretisationModel, m_flowRelationshipModel, calculationUnit, m_roughnessModel, m_restartNodes, false, true );
      converter2D.writeRMA10sModel( modelFile );
      ProgressUtilities.worked( progress, 20 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, "Schreibe Randbedingungen und Berechnungssteuerung" );
      progress.subTask( "Schreibe ASCII-Daten: Randbedingungen und Berechnungssteuerung..." );
      final File r10file = new File( m_tmpDir, RMA10SimModelConstants.R10_File );
      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( m_controlModel, m_flowRelationshipModel, m_roughnessModel, converter2D, buildingProvider );
      controlConverter.writeR10File( r10file );
      ProgressUtilities.worked( progress, 20 );

      /* Building File */
      m_log.formatLog( IStatus.INFO, "Schreibe Bauwerke" );
      progress.subTask( "Schreibe ASCII-Daten: Bauwerke..." );
      final File buildingFile = new File( m_tmpDir, RMA10SimModelConstants.BUILDING_File );
      final Building1D2DConverter buildingConverter = new Building1D2DConverter( buildingProvider );
      buildingConverter.writeBuildingFile( buildingFile );
      ProgressUtilities.worked( progress, 20 );

      /* W/Q BC File */
      m_log.formatLog( IStatus.INFO, "Schreibe W/Q-Randbedingungen" );
      progress.subTask( "Schreibe ASCII-Daten: W/Q-Randbedingungen..." );
      final File bcWQFile = new File( m_tmpDir, RMA10SimModelConstants.BC_WQ_File );
      final WQboundaryConditions1D2DConverter bc1D2DConverter = new WQboundaryConditions1D2DConverter( controlConverter.getBoundaryConditionsIDProvider() );
      bc1D2DConverter.writeWQbcFile( bcWQFile );
      ProgressUtilities.worked( progress, 20 );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Fehler beim Schreiben einer RMA10s-ASCII Datei: %s", e.getLocalizedMessage() );
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, e ) );
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
      m_log.formatLog( IStatus.INFO, "Kopiere Rechenkern in temporäres Verzeichnis..." );
      monitor.subTask( "Kopiere Rechenkern in temporäres Verzeichnis..." );

      final String exeResource = RMA10SimModelConstants.RMA10S_BASE + getKalypso1D2DKernelPath();
      final File destFile = new File( m_tmpDir, RMA10S_KALYPSO_EXE );
      final URL exeUrl = getClass().getResource( exeResource );
      FileUtils.copyURLToFile( exeUrl, destFile );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Fehler beim Kopieren der rma10s.exe aus den Programm-Resourcen: %s", e.toString() );
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, e ) );
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
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( "RMA10s wird ausgeführt..." );

    /* Create the result folder for the .exe file, must be same as in Control-Converter */
    new File( m_tmpDir, Control1D2DConverter.RESULT_DIR_NAME ).mkdirs();

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

      m_log.formatLog( IStatus.INFO, "RMA10s wird ausgeführt: %s", commandString );

      ProcessHelper.startProcess( commandString, new String[0], m_tmpDir, progressCancelable, 0, logOS, errorOS, null, 500, null );

      // TODO: read other outputs: error-log; iteration-log

      // Check for success
      final File errorFile = findErrorFile( m_tmpDir );
      if( errorFile == null )
      {
        m_log.formatLog( IStatus.INFO, "Simulation wurde erfolgreich beendet." );
        return StatusUtilities.createOkStatus( "Simulation wurde erfolgreich beendet." );
      }

      /* ERROR: return contents of error file as error message */
      final String errorMessage = FileUtils.readFileToString( errorFile, Charset.defaultCharset().name() );
      final String message = String.format( "Fehlermeldung vom Rechenkern (ERROR.DAT):%n---%n%s%n---%n%n", errorMessage );
      m_log.formatLog( IStatus.WARNING, message );

      return StatusUtilities.createWarningStatus( message );
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Fehler beim Ausführen der " + RMA10S_KALYPSO_EXE, e );
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

  public Date[] getCalculatedSteps( )
  {

    // TODO Auto-generated method stub
    return null;
  }
}
