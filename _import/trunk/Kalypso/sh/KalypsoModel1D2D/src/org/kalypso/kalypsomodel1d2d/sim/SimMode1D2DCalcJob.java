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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.text.DateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import java.util.logging.XMLFormatter;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import test.org.kalypso.kalypsomodel1d2d.TestWorkspaces;

/**
 * Implements the {@link ISimulation} interface to provide the simulation job for the 1d2d model
 * 
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Patrice Congo
 */
public class SimMode1D2DCalcJob implements ISimulation
{
  private RMA10Calculation m_calculation;

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Logger logger = Logger.getAnonymousLogger();
    final Formatter f = new XMLFormatter();

    Handler h = null;
    try
    {
      final File loggerFile = new File( tmpDir, "simulation.log" );

      h = new StreamHandler( new BufferedOutputStream( new FileOutputStream( loggerFile ) ), f );
      logger.addHandler( h );
      // why flush??
      h.flush();
    }
    catch( final FileNotFoundException e1 )
    {
      e1.printStackTrace();
      logger.fine( e1.getLocalizedMessage() );
    }

    final String nowString = DateFormat.getDateTimeInstance().format( new Date() );
    logger.log( Level.INFO, "Starte Berechnung: " + nowString + " (Serverzeit)\n" );

    try
    {
      monitor.setMessage( "Generiere Ascii Files zur 2D Simulation..." );
      if( monitor.isCanceled() )
        return;

      m_calculation = new RMA10Calculation( inputProvider );

      /** convert discretisation model stuff... */
      // TODO write merged *.2d file for calc core / Dejan starts like this I (Jessica) think
      monitor.setMessage( "Generiere 2D Netz..." );
      if( monitor.isCanceled() )
        return;

      final FE1D2DDiscretisationModel sourceModel = new FE1D2DDiscretisationModel( m_calculation.getDisModelWorkspace().getRootFeature() );
      final URL modelURL = (new File( tmpDir, "model.2d" )).toURL();
      IPositionProvider positionProvider = new XYZOffsetPositionProvider( ConvenienceCSFactory.getInstance().getOGCCSByName( TestWorkspaces.CS_KEY_GAUSS_KRUEGER ), 35 * 100000, 35 * 100000, 0 );
      Gml2RMA10SConv converter = new Gml2RMA10SConv( sourceModel, modelURL, positionProvider );
      // converter.toRMA10sModel();

      /** convert control/resistance stuff... */
      monitor.setMessage( "Generiere Randbedingungen und Berechnungssteuerung..." );
      if( monitor.isCanceled() )
        return;
      // TODO write control and boundary conditions calc core (*.R10 file)
      PrintWriter pw = new PrintWriter( new BufferedWriter( new FileWriter( new File( tmpDir, RMA10SimModelConstants.R10_File ) ) ) );
      Control1D2DConverter.writeR10File( m_calculation, pw );
      pw.close();

      /** start calculation... */
      monitor.setMessage( "Starte Rechenkern..." );
      if( monitor.isCanceled() )
        return;
      monitor.setProgress( 20 );
      m_calculation.setKalypso1D2DKernelPath();
      copyExecutable( tmpDir, m_calculation.getKalypso1D2DKernelPath() );
      startCalculation( tmpDir, monitor );

      /** check succeeded and load results */

      // unpack a demo-.2d-file in order to test result loading
      // TODO: remove thios as soon as we do the real calculation
      InputStream resultStream = null;
      try
      {
        resultStream = getClass().getResourceAsStream( RMA10SimModelConstants.RESOURCEBASE + "pseudoResult.zip" );
        ZipUtilities.unzip( resultStream, tmpDir );
        resultStream.close();
      }
      finally
      {
        IOUtils.closeQuietly( resultStream );
      }

      // Commented in order to test result-reading
      // comment in, if calculatino works
      // if( isSucceeded( tmpDir ) )
      {
        monitor.setMessage( "Simulation erfolgreich beendet - lade Ergebnisse..." );
        logger.log( Level.FINEST, "Simulation erfolgreich beendet - lade Ergebnisse" );
        loadResults( tmpDir, monitor, logger, resultEater, tmpDir );
      }
      // else
      // {
      // monitor.setFinishInfo( IStatus.WARNING, "Simulation konnte nicht erfolgreich beendet werden, sehen Sie die
      // Log-Dateien ein." );
      // logger.log( Level.SEVERE, "Simulation konnte nicht erfolgreich beendet werden!" );
      // }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new SimulationException( "Simulation couldn't be finished", e );
    }
    final Handler[] handlers = logger.getHandlers();
    for( final Handler handl : handlers )
      handl.close();
  }

  private void loadResults( File tmpdir, ISimulationMonitor monitor, final Logger logger, ISimulationResultEater resultEater, File tmpDir ) throws SimulationException
  {
    // TODO: check this here and add more handling, if result model is ready (Jessica)
    monitor.setMessage( "Lese Ergebnisse..." );

    final File outputDir = new File( tmpDir, RMA10SimModelConstants.OUTPUT_DIR_NAME );
    outputDir.mkdirs();

    final FileFilter suffixFileFilter = FileFilterUtils.suffixFileFilter( ".2d" );
    final File[] files = tmpDir.listFiles( suffixFileFilter );

    monitor.setProgress( 80 );

    try
    {
      /* zip all .2d files */
      final File outputZip2d = new File( tmpdir, "test.zip" );
      monitor.setProgress( 99 );

      ZipUtilities.zip( outputZip2d, files, outputDir );
      resultEater.addResult( RMA10SimModelConstants.RESULT_2d_ZIP_ID, outputZip2d );

      /* Read all .2d files into NodeResults */
      final File result2dFile = files[0];
      final File gmlResultfile = read2DIntoNodeResult( result2dFile, outputDir );
      resultEater.addResult( "NodeResultModel", gmlResultfile );
    }
    catch( final Throwable e )
    {
      throw new SimulationException( "Fehler beim Lesen der Ergebnisdaten", e );
    }
  }

  private File read2DIntoNodeResult( final File result2dFile, final File outputDir ) throws IOException, InvocationTargetException, GmlSerializeException
  {
    final TimeLogger logger = new TimeLogger( "Start: lese .2d Ergebnisse" );

    final File gmlResultFile = new File( outputDir, "results.gml" );

    InputStream is = null;
    try
    {
      is = new FileInputStream( result2dFile );

      /* GMLWorkspace für Ergebnisse anlegen */

      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResultCollection" ), gmlResultFile.toURL(), null );

      /* .2d Datei lesen und GML füllen */
      final RMA10S2GmlConv conv = new RMA10S2GmlConv();
      final IRMA10SModelElementHandler handler = new NodeResultsHandler( resultWorkspace );
      conv.setRMA10SModelElementHandler( handler );

      conv.parse( is );

      is.close();

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Lesen in : " );

      /* GML in Datei schreiben */
      GmlSerializer.serializeWorkspace( gmlResultFile, resultWorkspace, "UTF-8" );
      
      return gmlResultFile;
    }
    finally
    {
      IOUtils.closeQuietly( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Schreiben in : " );
    }
  }

  /**
   * copy the executable to from the resources
   */
  private void copyExecutable( File tmpdir, String simulationExeName )
  {
    final String exeResource = RMA10SimModelConstants.RESOURCEBASE + simulationExeName;
    final File destFile = new File( tmpdir, simulationExeName );
    if( !destFile.exists() )
    {
      InputStream inputStream = null;
      try
      {
        inputStream = getClass().getResourceAsStream( exeResource );
        FileUtilities.makeFileFromStream( false, destFile, inputStream );
        inputStream.close();
        System.out.println( " ...copied" );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        System.out.println( "ERR: " + exeResource + " may not exist" );
      }
      finally
      {
        IOUtils.closeQuietly( inputStream );
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
  private void startCalculation( final File basedir, ISimulationMonitor monitor ) throws SimulationException
  {
    final File exeFile = new File( basedir, m_calculation.getKalypso1D2DKernelPath() );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( basedir, "exe.log" ) );
      errorOS = new FileOutputStream( new File( basedir, "exe.err" ) );
      ProcessHelper.startProcess( commandString, new String[0], exeDir, monitor, RMA10SimModelConstants.PROCESS_TIMEOUT, logOS, errorOS, null );
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

  /**
   * checks if calculation of simulations is succeeded
   */
  public boolean isSucceeded( final File baseDir )
  {
    final File finalResultFile = new File( baseDir, "steady.2d" );
    return finalResultFile.exists();
  }
}