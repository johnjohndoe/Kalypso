/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.text.DateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import java.util.logging.XMLFormatter;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * Implements the {@link ISimulation} interface to provide the simulation job for the 1d2d model
 * 
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Patrice Congo
 */
public class SimMode1D2DCalcJob implements ISimulation
{
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Logger logger = Logger.getAnonymousLogger();
    final Formatter f = new XMLFormatter();

    try
    {
      final File loggerFile = new File( tmpDir, "simulation.log" );

      final Handler h = new StreamHandler( new BufferedOutputStream( new FileOutputStream( loggerFile ) ), f );
      logger.addHandler( h );
    }
    catch( final FileNotFoundException e1 )
    {
      e1.printStackTrace();
      logger.fine( e1.getLocalizedMessage() );
    }

    /* Start logging */
    final Date startTime = new Date();
    final String nowString = DateFormat.getDateTimeInstance().format( startTime );
    logger.log( Level.INFO, "Starte Berechnung: " + nowString + " (Serverzeit)\n" );

    ResultManager resultRunner = null;
    try
    {
      monitor.setMessage( "Generiere Ascii Files für FE-Simulation..." );
      if( monitor.isCanceled() )
        return;

      final RMA10Calculation calculation = new RMA10Calculation( inputProvider );

      /* Prepare for any results */
      final File outputDir = new File( tmpDir, RMA10SimModelConstants.OUTPUT_DIR_NAME );
      resultEater.addResult( RMA10SimModelConstants.RESULT_DIR_NAME_ID, outputDir );
      final ICalculationUnit calculationUnit = calculation.getControlModel().getCalculationUnit();
      final String calcUnitID = calculationUnit.getWrappedFeature().getId();
      final File calcUnitOutputDir = new File( outputDir, calcUnitID );
      calcUnitOutputDir.mkdirs();

      resultRunner = new ResultManager( tmpDir, calcUnitOutputDir, "A", inputProvider, calculation, startTime );

      /** convert discretisation model stuff... */
      // write merged *.2d file for calc core / Dejan
      final File modelFile = new File( tmpDir, "model.2d" );
      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( modelFile, calculation );

      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "Generiere 2D Netz..." );
      converter2D.toRMA10sModel();

      /** convert control/resistance stuff... */
      // first this because we need roughness classes IDs for creating 2D net later
      monitor.setMessage( "Generiere Randbedingungen und Berechnungssteuerung..." );
      if( monitor.isCanceled() )
        return;

      PrintWriter r10pw = null;
      PrintWriter weirPw = null;
      try
      {
        /* Control model */
        r10pw = new PrintWriter( new File( tmpDir, RMA10SimModelConstants.R10_File ) );
        final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
        final LinkedHashMap<String, Integer> roughnessIDProvider = converter2D.getRoughnessIDProvider();
        final LinkedHashMap<String, Integer> nodesIDProvider = converter2D.getNodesIDProvider();
        final Control1D2DConverter controlConverter = new Control1D2DConverter( calculation, nodesIDProvider, roughnessIDProvider, buildingProvider );
        controlConverter.writeR10File( r10pw );
        r10pw.close();

        /* Weir File */
        weirPw = new PrintWriter( new File( tmpDir, RMA10SimModelConstants.BUILDING_File ) );
        final Building1D2DConverter weirConverter = new Building1D2DConverter( buildingProvider );
        weirConverter.writeBuildingFile( new java.util.Formatter( weirPw ) );
        weirPw.close();
      }
      finally
      {
        /* Always close stream in a finally block */
        IOUtils.closeQuietly( r10pw );
        IOUtils.closeQuietly( weirPw );
      }

      /** start calculation... */
      monitor.setMessage( "Starte Rechenkern..." );
      if( monitor.isCanceled() )
        return;

      monitor.setProgress( 20 );

      copyExecutable( tmpDir, calculation.getKalypso1D2DKernelPath() );

      resultRunner.calculationAboutToStart();
      startCalculation( tmpDir, monitor, resultRunner, calculation );

      /* check succeeded and load results */
      handleError( tmpDir, calcUnitOutputDir, monitor, logger );
    }
    catch( final SimulationException se )
    {
      throw se;
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      final String localizedMessage = e.getLocalizedMessage();
      final String msg = localizedMessage == null ? e.toString() : localizedMessage;
      throw new SimulationException( "Simulation couldn't be finished: " + msg, e );
    }
    finally
    {
      final Handler[] handlers = logger.getHandlers();
      for( final Handler handl : handlers )
        handl.close();

      /* Run a last time so nothing is forgotten... */
      /* Save result model after all results are processed */
      if( resultRunner != null )
        resultRunner.finish();
    }
  }

  private void handleError( final File tmpDir, final File outputDir, final ISimulationMonitor monitor, final Logger logger ) throws IOException
  {
    final File errorDatFile = new File( tmpDir, "ERROR.DAT" );
    final File errorOutFile = new File( tmpDir, "ERROR.OUT" );
    final File errorFile;
    if( errorDatFile.exists() )
      errorFile = errorDatFile;
    else if( errorOutFile.exists() )
      errorFile = errorOutFile;
    else
      errorFile = null;

    if( errorFile != null )
    {
      final String message = "Fehler bei der Berechnung - übertrage Zwischenergebnisse und Logdateien...";
      monitor.setMessage( message );
      logger.log( Level.FINEST, message );

      handleErrorOut( errorFile, outputDir, monitor );
    }
  }

  private void handleErrorOut( final File errorFile, final File outputDir, final ISimulationMonitor monitor ) throws IOException
  {
    final String errorMessage = FileUtils.readFileToString( errorFile, null );
    // TODO: translate error message to Kalypso
    monitor.setFinishInfo( IStatus.ERROR, errorMessage );

    FileUtils.copyFile( errorFile, new File( outputDir, "ERROR.DAT" ) );
  }

  /**
   * copy the executable to from the resources
   */
  private void copyExecutable( final File tmpdir, final String simulationExeName )
  {
    final String exeResource = RMA10SimModelConstants.RESOURCEBASE + simulationExeName;
    final File destFile = new File( tmpdir, simulationExeName );
    if( !destFile.exists() )
    {
      try
      {
        final URL exeUrl = getClass().getResource( exeResource );
        FileUtils.copyURLToFile( exeUrl, destFile );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        // TODO: use monitor/logger
        System.out.println( "ERR: " + exeResource + " may not exist" );
      }
      finally
      {
      }
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( RMA10SimModelConstants.CALCJOB_SPEC );
  }

  /**
   * starts 2D simulation
   */
  private void startCalculation( final File basedir, final ISimulationMonitor monitor, final Runnable resultRunner, final RMA10Calculation calculation ) throws SimulationException
  {
    /*
     * Creates the result folder for the .exe file, must be same as in Control-Converter (maybe give as an argument?)
     */
    new File( basedir, "result" ).mkdirs();

    final File exeFile = new File( basedir, calculation.getKalypso1D2DKernelPath() );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( basedir, "exe.log" ) );
      errorOS = new FileOutputStream( new File( basedir, "exe.err" ) );
      ProcessHelper.startProcess( commandString, new String[0], exeDir, monitor, RMA10SimModelConstants.PROCESS_TIMEOUT, logOS, errorOS, null, 500, resultRunner );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausfuehren der Berechnung", e );
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );
    }
  }
}