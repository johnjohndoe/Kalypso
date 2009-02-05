/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.simulation.core.ISimulationService;
import org.kalypso.simulation.core.calccase.CalcJobHandler;
import org.kalypso.simulation.core.internal.local.SingleSimulationService;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Represents a calculation with a rma10s.exe. Helps generation of the ASCII input files and to start the process.
 */
public class RMAKalypsoSimulationRunner implements ISimulation1D2DConstants
{
  private final IControlModel1D2D m_controlModel;

  private final IGeoLog m_log;

  private IStatus m_simulationStatus;

  private IterationInfo m_iterationInfo;

  /*
   * this will be null if calculation has not been run, contains absolute path to simulation results folder
   */
  private File m_resultTmpDir = null;

  private IFolder m_scenarioFolder;

  public RMAKalypsoSimulationRunner( final IGeoLog geoLog, final SzenarioDataProvider caseDataProvider ) throws CoreException
  {
    m_log = geoLog;
    final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class );
    m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();
    m_scenarioFolder = (IFolder) caseDataProvider.getScenarioFolder();
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
    m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0") ); //$NON-NLS-1$

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
    final SubMonitor progress = SubMonitor.convert( monitor, "", 1000 );

    // finish this job in finally block if not null
    IterationInfoJob iterationJob = null;
    try
    {
      final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      final String calcUnitID = calculationUnit.getGmlID();

      // result temp dir is "[scenarioFolder]/results/[calcUnitID]/temp"
      final IFolder scenarioResultsFolder = m_scenarioFolder.getFolder( ISimulation1D2DConstants.OUTPUT_DIR_NAME );
      final IFolder resultTempFolder = scenarioResultsFolder.getFolder( calcUnitID ).getFolder( "temp" );

      // remember result temp dir for later result processing (absolute path)
      m_resultTmpDir = new File( resultTempFolder.getLocationURI() );

      // simulation temp dir
      // will be deleted at the end of simulation!!!
      // final File simulationTmpDir = SimulationUtilitites.createSimulationTmpDir( "rmaKalypso-" + calcUnitID );
      final File simulationTmpDir = new File( m_resultTmpDir, "rma" );
      simulationTmpDir.mkdirs();

      // modeldata
      final ArrayList<String> outputDef = new ArrayList<String>();
      final Modeldata modeldata = SimulationUtilitites.createModelData( new HashMap<String, String>(), outputDef );
      final String typeID = RMAKalypsoSimulation.ID;
      modeldata.setTypeID( typeID );

      // fill modeldata
      // addExpectedResultFiles( modeldata, resultTempFolder );
      final Modeldata.Output e = new Modeldata.Output();
      final String resultFile = resultTempFolder.getProjectRelativePath().toOSString();
      e.setPath( resultFile );
      e.setId( "results" );
      modeldata.getOutput().add( e );

      // set up iteration monitoring
      iterationJob = setUpIterationInfoJob( monitor, simulationTmpDir );

      // run calculation locally
      final ISimulationService calcService = new SingleSimulationService( simulationTmpDir );
      final CalcJobHandler cjHandler = new CalcJobHandler( modeldata, calcService );
      cjHandler.runJob( m_scenarioFolder.getProject(), progress.newChild( 800, SubMonitor.SUPPRESS_NONE ) );

      // TODO: read other outputs
      // - error-log
      // - border conditions-log

      // Check for success
      final File errorFile = findErrorFile( simulationTmpDir );
      if( errorFile == null )
        return StatusUtilities.createOkStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.20") ); //$NON-NLS-1$

      /* ERROR: return contents of error file as error message */
      final String errorMessage = FileUtils.readFileToString( errorFile, Charset.defaultCharset().name() );
      final IStatus errorStatus = errorToStatus( errorMessage );

      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_RMA10S, new IStatus[] { errorStatus }, Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.21"), null ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
    catch( final Throwable e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.22"), e ); //$NON-NLS-1$
    }
    finally
    {
      // clean up iteration job
      if( iterationJob != null )
        iterationJob.finish();
    }
  }

  private IterationInfoJob setUpIterationInfoJob( final IProgressMonitor monitor, final File simulationTmpDir )
  {
    IterationInfoJob iterationJob;
    // The iteration job (and its info) monitor the content of the "Output.itr" file
    // and inform the user about the current progress of the process.
    // watch iteration observation directly in temp dir (only possible for local simulation)
    final File iterObsFile = new File( simulationTmpDir, OUTPUT_ITR );
    // and put processed iterations GML into folder "iterObs" in result temp dir
    final File iterObsDir = new File( m_resultTmpDir, "iterObs" );
    iterObsDir.mkdirs();
    m_iterationInfo = new IterationInfo( iterObsFile, iterObsDir, m_controlModel );
    iterationJob = new IterationInfoJob( m_iterationInfo, m_controlModel, monitor );
    iterationJob.setSystem( true );
    iterationJob.schedule();
    return iterationJob;
  }

  /**
   * Checks, if an error file exists and returns it.
   * 
   * @return <code>null</code>, if no existing error file was found.
   */
  private static File findErrorFile( final File dir )
  {
    // TODO: @Nico: we should stick to one defined error file in the future; whic one is it?
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
      return StatusUtilities.createStatus( IStatus.OK, CODE_RUNNING, Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.1"), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.CANCEL ) )
      return StatusUtilities.createStatus( IStatus.CANCEL, CODE_RUNNING, Messages.getString("org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.2"), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.ERROR ) )
      return simulationStatus;

    // if( simulationStatus.matches( IStatus.WARNING ) )
    return simulationStatus; // StatusUtilities.createStatus( IStatus.WARNING, CODE_RUNNING, "Simulation mit Warnung
    // beendet.",
    // null );
  }

  public IterationInfo getIterationInfo( )
  {
    return m_iterationInfo;
  }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

  public File getTempDir( )
  {
    return m_resultTmpDir;
  }
}
