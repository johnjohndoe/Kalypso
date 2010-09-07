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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author Monika Thül
 * @author Gernot Belger
 */
public class WspmTuhhCalcJob implements ISimulation
{
  public static final String KALYPSO_1D_EXE_PATTERN = "Kalypso-1D(.*).exe";//$NON-NLS-1$ 

  public static final String KALYPSO_1D_EXE_FORMAT = "Kalypso-1D%s.exe";//$NON-NLS-1$ 

  public static final String CALCJOB_SPEC = "WspmTuhhCalcJob_spec.xml"; //$NON-NLS-1$

  public static final String INPUT_OVW_MAP_SPECIAL = "OVW_MAP_SPECIAL"; //$NON-NLS-1$

  public static final String INPUT_OVW_MAP_GENERAL = "OVW_MAP_GENERAL"; //$NON-NLS-1$

  public static final String INPUT_EPS_THINNING = "EPS_THINNING"; //$NON-NLS-1$

  public static final String INPUT_CALC_PATH = "CALC_PATH"; //$NON-NLS-1$

  public static final String INPUT_MODELL_GML = "MODELL_GML"; //$NON-NLS-1$

  public static final String OUTPUT_QINTERVALL_RESULT_GMV = "qIntervallResultGmv"; //$NON-NLS-1$

  public static final String OUTPUT_QINTERVALL_RESULT = "qIntervallResultGml"; //$NON-NLS-1$

  public static final String OUTPUT_SIMULATION_LOG = "SimulationLog"; //$NON-NLS-1$

  private final PrintStream m_calcOutConsumer;

  public WspmTuhhCalcJob( )
  {
    this( System.out );
  }

  public WspmTuhhCalcJob( final PrintStream calcOutConsumer )
  {
    m_calcOutConsumer = calcOutConsumer;
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL modellGmlURL = (URL) inputProvider.getInputForID( INPUT_MODELL_GML );
    final String calcXPath = (String) inputProvider.getInputForID( INPUT_CALC_PATH );
    final String epsThinning = (String) inputProvider.getInputForID( INPUT_EPS_THINNING );
    final URL ovwMapURL = findOvwMapUrl( inputProvider );

    final File simulogFile = new File( tmpDir, "simulation.log" ); //$NON-NLS-1$
    resultEater.addResult( OUTPUT_SIMULATION_LOG, simulogFile );

    PrintWriter pwSimuLog = null;
    final InputStream zipInputStream = null;
    OutputStream strmKernelErr = null;
    try
    {
      final PrintStream calcOutConsumer = m_calcOutConsumer;
      final OutputStream osSimuLog = new BufferedOutputStream( new FileOutputStream( simulogFile ) )
      {
        // REMARK: also stream stuff into System.out in order to have a log in the console.view
        /**
         * @see java.io.BufferedOutputStream#write(byte[], int, int)
         */
        @Override
        public synchronized void write( final byte[] b, final int off, final int len ) throws IOException
        {
          super.write( b, off, len );

          calcOutConsumer.write( b, off, len );
        }

        /**
         * @see java.io.BufferedOutputStream#write(int)
         */
        @Override
        public synchronized void write( final int b ) throws IOException
        {
          super.write( b );

          calcOutConsumer.write( b );
        }
      };
      pwSimuLog = new PrintWriter( new OutputStreamWriter( osSimuLog, "CP1252" ) ); //$NON-NLS-1$

      final LogHelper log = new LogHelper( pwSimuLog, monitor, calcOutConsumer );
      log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.7" ), calcXPath ); //$NON-NLS-1$

      final GMLXPath calcpath = new GMLXPath( calcXPath, null );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL, null );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.8" ) + calcObject ); //$NON-NLS-1$
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.9" ), calcXPath, calcObject ); //$NON-NLS-1$
        return;
      }

      final TuhhCalculation calculation = (TuhhCalculation) calcObject;

      monitor.setProgress( 10 );

      log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.10" ) ); //$NON-NLS-1$

      // write calculation to tmpDir
      WspWinExporter.writeForTuhhKernel( calculation, tmpDir );

      // ensure availability of DATH directory (for results)
      final File dathDir = new File( tmpDir, "dath" ); //$NON-NLS-1$
      dathDir.mkdirs();

      // prepare kernel logs (log and err)
      final File fleKernelErr = new File( tmpDir, "kernel.err" ); //$NON-NLS-1$
      resultEater.addResult( "KernelErr", fleKernelErr ); //$NON-NLS-1$
      strmKernelErr = new BufferedOutputStream( new FileOutputStream( fleKernelErr ) );

      final File iniFile = new File( tmpDir, "kalypso-1D.ini" ); //$NON-NLS-1$

      monitor.setProgress( 20 );

      /* Start kalypso-1d.exe */
      log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.11" ) ); //$NON-NLS-1$

      if( log.checkCanceled() )
        return;

      final File exeFile = getExecuteable( calculation, KALYPSO_1D_EXE_FORMAT, KALYPSO_1D_EXE_PATTERN, monitor );
      if( exeFile == null )
        return;

      final String sCmd = "\"" + exeFile.getAbsolutePath() + "\" n \"" + iniFile.getAbsolutePath() + "\""; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, 0, osSimuLog, strmKernelErr, null );

      if( log.checkCanceled() )
        return;

      final WspmTuhhPostProcessor postProcessor = new WspmTuhhPostProcessor( calculation, tmpDir, dathDir, epsThinning, ovwMapURL, resultEater, log, pwSimuLog );
      postProcessor.run( monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei der Berechnung", e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( pwSimuLog );
      IOUtils.closeQuietly( zipInputStream );
      IOUtils.closeQuietly( strmKernelErr );
    }
  }

  private URL findOvwMapUrl( final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    if( inputProvider.hasID( INPUT_OVW_MAP_GENERAL ) )
      return (URL) inputProvider.getInputForID( INPUT_OVW_MAP_GENERAL );

    if( inputProvider.hasID( INPUT_OVW_MAP_SPECIAL ) )
      return (URL) inputProvider.getInputForID( INPUT_OVW_MAP_SPECIAL );

    return null;
  }

  public final static File getExecuteable( final TuhhCalculation calculation, final String exeFormat, final String exePattern, final ISimulationMonitor monitor )
  {
    try
    {
      final String version = calculation.getVersion();
      final File exeFile = CalcCoreUtils.findExecutable( version, exeFormat, exePattern );
      if( exeFile == null )
        return null;
      return exeFile;
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      monitor.setFinishInfo( status.getSeverity(), status.getMessage() );
      return null;
    }
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALCJOB_SPEC );
  }
}
