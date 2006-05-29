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
package org.kalypso.model.wspm.tuhh.simulation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.ui.model.wspm.abstraction.TuhhCalculation;
import org.kalypso.ui.model.wspm.core.wspwin.WspWinExporter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author thuel2
 */
public class WspmTuhhCalcJob implements ISimulation
{
  public static final String CALCJOB_SPEC = "WspmTuhhCalcJob_spec.xml";

  public static final String WSPMTUHH_CODEPAGE = "Cp1252";

  // Timeout beim Rechnen([ms])
  public static final int PROCESS_TIMEOUT = 600000;

  public static final String MESS_BERECHNUNG_ABGEBROCHEN = "Modell: Berechnung abgebrochen";

  public WspmTuhhCalcJob( )
  {
    // will not be instantiated
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    long lTimeout = PROCESS_TIMEOUT;
    final URL modellGmlURL = (URL) inputProvider.getInputForID( "MODELL_GML" );
    final String calcXPath = (String) inputProvider.getInputForID( "CALC_PATH" );

    final File simulogFile = new File( tmpDir, "simulation.log" );
    resultEater.addResult( "SimulationLog", simulogFile );

    PrintWriter pwSimuLog = null;
    InputStream zipInputStream = null;
    FileOutputStream strmErr = null;
    PrintWriter pwErr = null;
    PrintWriter pwInParams = null;
    StringWriter swLog = null;

    try
    {
      pwSimuLog = new PrintWriter( new BufferedWriter( new FileWriter( simulogFile ) ) );

      pwSimuLog.println( "Parsing GMLXPath: " + calcXPath );
      monitor.setMessage( "Parsing GMLXPath: " + calcXPath );
      final GMLXPath calcpath = new GMLXPath( calcXPath );

      // load gml
      pwSimuLog.println( "Loading GML: " + modellGmlURL );
      monitor.setMessage( "Loading GML: " + modellGmlURL );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL );

      // get calculation via path
      pwSimuLog.println( "Loading Calculation: " + calcXPath );
      monitor.setMessage( "Loading Calculation: " + calcXPath );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, "GMLXPath points to no feature: " + calcObject );
        return;
      }

      final Feature calculationFeature = (Feature) calcObject;
      final TuhhCalculation calculation = new TuhhCalculation( calculationFeature );

      monitor.setProgress( 10 );
      pwSimuLog.println( "Writing files for tuhh-mode" );
      monitor.setMessage( "Writing kernel data" );

      // write calculation to tmp dir
      WspWinExporter.writeForTuhhKernel( calculation, tmpDir, modellGmlURL );

      // ensure availability of DATH directory (for results)
      final File dathDir = new File( tmpDir, "dath" );
      dathDir.mkdirs();

      // unpack kernel
      zipInputStream = WspmTuhhCalcJob.class.getResourceAsStream( "resources/rechenkern.zip" );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );

      monitor.setProgress( 20 );
      monitor.setMessage( "Executing model" );

      // start calculation
      // prepare error log - kernel logs only to SystemOut (and doesn't use SystemErr)
      final File fleErr = new File( tmpDir, "kernel.log" );
      resultEater.addResult( "KernelLog", fleErr );
      strmErr = new FileOutputStream( fleErr );
      pwErr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( strmErr, WSPMTUHH_CODEPAGE ) ) );

      // TODO
      // input Stream: n, prof/calc.properties
      final File fleInParams = new File( tmpDir, "input.txt" );
      pwInParams = new PrintWriter( new BufferedWriter( new FileWriter( fleInParams ) ) );
      pwInParams.println( "n" );
      pwInParams.println( tmpDir.getAbsolutePath() + File.separator + "prof" + File.separator + "calc.properties" );
      pwInParams.close();

      if( monitor.isCanceled() )
      {
        pwSimuLog.println( MESS_BERECHNUNG_ABGEBROCHEN );
      }

      String sCmd = tmpDir.getAbsolutePath() + File.separator + "Kalypso-1D.exe < " + fleInParams.getAbsolutePath();
      swLog = new StringWriter();
      // ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, lTimeout, swLog, pwErr );
      ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, lTimeout, pwErr, pwErr );
      if( monitor.isCanceled() )
      {
        pwSimuLog.println( MESS_BERECHNUNG_ABGEBROCHEN );
      }

      // load results + copy to result folder + unzip templates
      monitor.setProgress( 80 );
      monitor.setMessage( "Retrieving results" );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Unable to load model", e );
    }
    finally
    {
      IOUtils.closeQuietly( pwSimuLog );
      IOUtils.closeQuietly( zipInputStream );
      IOUtils.closeQuietly( strmErr );
      IOUtils.closeQuietly( pwErr );
      IOUtils.closeQuietly( pwInParams );
    }
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALCJOB_SPEC );
  }

}
