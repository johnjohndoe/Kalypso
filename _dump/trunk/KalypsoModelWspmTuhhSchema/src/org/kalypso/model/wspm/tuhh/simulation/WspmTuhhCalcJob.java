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
import java.io.FileWriter;
import java.io.PrintWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
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

  public WspmTuhhCalcJob( )
  {
    // will not be instantiated
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL modellGmlURL = (URL) inputProvider.getInputForID( "MODELL_GML" );
    final String calcXPath = (String) inputProvider.getInputForID( "CALC_PATH" );

    final File simulogFile = new File( tmpdir, "simulation.log" );
    resultEater.addResult( "SimulationLog", simulogFile );

    PrintWriter pw = null;
    try
    {
      pw = new PrintWriter( new BufferedWriter( new FileWriter( simulogFile ) ) );

      pw.println( "Parsing GMLXPath: " + calcXPath );
      monitor.setMessage( "Parsing GMLXPath: " + calcXPath );
      final GMLXPath calcpath = new GMLXPath( calcXPath );

      // load gml
      pw.println( "Loading GML: " + modellGmlURL );
      monitor.setMessage( "Loading GML: " + modellGmlURL );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL );

      // get calculation via path
      pw.println( "Loading Calculation: " + calcXPath );
      monitor.setMessage( "Loading Calculation: " + calcXPath );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, "GMLXPath points to no feature: " + calcObject );
        return;
      }

      final Feature calculationFeature = (Feature) calcObject;
      // workspace.getFeature( calcXPath );
      final TuhhCalculation calculation = new TuhhCalculation( calculationFeature );

      monitor.setProgress( 10 );
      pw.println( "Writing files for tuhh-mode" );
      monitor.setMessage( "Writing kernel data" );

      // write calculation to tmp dir
      WspWinExporter.writeForTuhhKernel( calculation, tmpdir, modellGmlURL );

      // unpack + start exe

      monitor.setProgress( 20 );
      monitor.setMessage( "Executing model" );

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
      IOUtils.closeQuietly( pw );
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
