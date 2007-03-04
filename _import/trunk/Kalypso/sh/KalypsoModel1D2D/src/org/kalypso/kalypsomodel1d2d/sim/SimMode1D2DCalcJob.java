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

import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;

import test.org.kalypso.kalypsomodel1d2d.TestWorkspaces;

/**
 * Implements the {@link ISimulation} interface to provide the simulation job for the 1d2d model
 * 
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Patrice Congo
 */
public class SimMode1D2DCalcJob implements ISimulation
{
  public static final String DISCRETISATIOMODEL_ID = "DiscretisationModel";

  public static final String TERRAINMODEL_ID = "TerrainModel";

  public static final String OPERATIONALMODEL_ID = "OperationalModel";

  public static final String FLOWRESISTANCEMODEL_ID = "FlowResistanceModel";

  public static final String FLOWRELATIONSHIPMODEL_ID = "FlowRelationshipModel";

  public static final String SIMULATIONRESULTMODEL_ID = "SimulationResultModel";

  public static final String CONTROL_ID = "SimulationControlModel";

  public static final String ROUGHNESS_ID = "Roughness";

  public static final String RESULT_DIR_NAME = "result";

  public static final String CALCJOB_SPEC = "resource/1D2Dcalcjob_spec.xml";

  private final String m_resourceBase = "resource/template/";

  private String R10_File = "control.R10";

  private final String SIM_EXE_FILE_3_5 = "RMA10S35Kalypso.exe";

  private final String SIM_EXE_FILE_TEST = "RMA10S35Kalypso.exe";

  private String m_kalypso1D2DKernelPath;

  /**
   * class logger
   */
  final static private Logger logger = Logger.getLogger( SimMode1D2DCalcJob.class.getName() );

  private boolean m_succeeded = false;

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File exedir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Date date = new Date( Calendar.getInstance().getTimeInMillis() );
    logger.log( Level.INFO, "Zeitpunkt Start Berechnung: " + date.toString() + " (Serverzeit)\n" );
    final File resultDir = new File( exedir, RESULT_DIR_NAME );

    try
    {
      monitor.setMessage( "richte Berechnungsverzeichnis ein..." );
      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "Generiere Ascii Files zur 2D Simulation..." );
      if( monitor.isCanceled() )
        return;

      // Simulation Model stuff...
      // Terrainmodell not needed, because mapping to the nodes by the user in the workflow
      // final URL flowResModelURL = (URL) inputProvider.getInputForID( FLOWRESISTANCEMODEL_ID );
      // final URL flowRelModelURL = (URL) inputProvider.getInputForID( FLOWRELATIONSHIPMODEL_ID );
      final URL disModelURL = (URL) inputProvider.getInputForID( DISCRETISATIOMODEL_ID );
      final GMLWorkspace disModelWorkspace = GmlSerializer.createGMLWorkspace( disModelURL, null );
      final FE1D2DDiscretisationModel sourceModel = new FE1D2DDiscretisationModel( disModelWorkspace.getRootFeature() );
      final URL modelURL = (new File( exedir, "model.2d" )).toURL();
      monitor.setMessage( "Generiere 2D Netz..." );
      // TODO Netz schreiben / Dejan starts like this I think
      IPositionProvider positionProvider = new XYZOffsetPositionProvider( ConvenienceCSFactory.getInstance().getOGCCSByName( TestWorkspaces.CS_KEY_GAUSS_KRUEGER ), 35 * 100000, 35 * 100000, 0 );
      Gml2RMA10SConv converter = new Gml2RMA10SConv( sourceModel, modelURL, positionProvider, null );
      // converter.toRMA10sModel();

      // OperationalModel stuff...
//      final URL operationalModelURL = (URL) inputProvider.getInputForID( OPERATIONALMODEL_ID );
      monitor.setMessage( "Generiere Randbedingungen..." );

      // SimulationControlModel stuff
      final URL controlURL = (URL) inputProvider.getInputForID( CONTROL_ID );
      final GMLWorkspace controlWS = GmlSerializer.createGMLWorkspace( controlURL, null );
      monitor.setMessage( "Generiere Berechnungssteuerung..." );
      // TODO Randbedingungen schreiben
      // TODO Berechnungssteuerung schreiben
      Control1D2DConverter control2asci = new Control1D2DConverter( controlWS );
      StringBuffer controlSb = control2asci.writeR10ControlData();
      OutputStreamWriter controlWriter = new OutputStreamWriter( new FileOutputStream( new File( exedir, R10_File ) ), "UTF-8" );
      controlWriter.write( controlSb.toString() );
      controlWriter.close();
      monitor.setMessage( "generating boundary conditions ascii file" );

      // choose simulation kernel
      chooseSimulationExe( controlWS );
      copyExecutable( exedir );

      monitor.setMessage( "starting 2D simulation..." );

      if( monitor.isCanceled() )
        return;
      monitor.setProgress( 20 );
      startCalculation( exedir, monitor );
      checkSucceeded( exedir );

      if( isSucceeded() )
      {
        monitor.setMessage( "Simulation erfolgreich beendet - lade Ergebnisse..." );
        logger.log( Level.FINEST, "Simulation erfolgreich beendet - lade Ergebnisse" );
        // final URL simResModelURL = (URL) inputProvider.getInputForID( SIMULATIONRESULTMODEL_ID );
        // final GMLWorkspace simResWorkspace = GmlSerializer.createGMLWorkspace( flowRelModelURL, null );
        // loadResults( tmpdir, exeDir, simResWorkspace, monitor, logger, resultDir, resultEater );
        System.out.println( "Finished 2D Simulation successfully." );

        System.out.println( "This is the end ;-)" );
      }
      else
        System.out.println( "Finished 2D Simulation not successfully." );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "simulation couldn't be finished", e );
    }
  }

  private void loadResults( File tmpdir, File exeDir, GMLWorkspace simResWorkspace, ISimulationMonitor monitor, Logger logger, File resultDir, ISimulationResultEater resultEater )
  {
    resultDir.mkdirs();
    FileFilter suffixFileFilter = FileFilterUtils.suffixFileFilter( ".2d" );
    File[] files = exeDir.listFiles( suffixFileFilter );

    monitor.setProgress( 80 );
    for( int i = 0; i < files.length; i++ )
    {
      System.out.println( "name of file_" + i + ":: " + files[i].getName() );
      if( !files[i].getName().equalsIgnoreCase( "erg.2d" ) && !files[i].getName().equalsIgnoreCase( "fehler.2d" ) && !files[i].getName().equalsIgnoreCase( "out.2d" )
          && !files[i].getName().equalsIgnoreCase( "marsh.2d" ) )
      {
        // TODO write results to the resultsworkspace
      }
      monitor.setProgress( 99 );
    }
    try
    {
      resultEater.addResult( SIMULATIONRESULTMODEL_ID, resultDir );
    }
    catch( SimulationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  private void copyExecutable( File tmpdir )
  {
    final String exeResource = m_resourceBase + m_kalypso1D2DKernelPath;
    final File destFile = new File( tmpdir, m_kalypso1D2DKernelPath );
    if( !destFile.exists() )
    {
      try
      {
        final InputStream inputStream = getClass().getResourceAsStream( exeResource );
        FileUtilities.makeFileFromStream( false, destFile, inputStream );
        System.out.println( " ...copied" );
      }
      catch( Exception e )
      {
        e.printStackTrace();

        System.out.println( "ERR: " + exeResource + " may not exist" );
      }
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    /**
     * Relative path to the job specification
     */
    return getClass().getResource( CALCJOB_SPEC );
  }

  /**
   * @return true if the job was succesfully executed otherwise false
   */
  public boolean isSucceeded( )
  {

    return m_succeeded;
  }

  /**
   * starts 2D simulation
   * 
   * @param monitor
   */
  private void startCalculation( final File basedir, ISimulationMonitor monitor ) throws SimulationException
  {
    final File exeFile = new File( basedir, m_kalypso1D2DKernelPath );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    long timeOut = 1000l * 60l * 60l * 24l; // max 60 * 24 minutes = 1 day
    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( basedir, "exe.log" ) ); // TODO wie heißen die im 1D2D???
      errorOS = new FileOutputStream( new File( basedir, "exe.err" ) );
      ProcessHelper.startProcess( commandString, new String[0], exeDir, monitor, timeOut, logOS, errorOS, null );
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
  public void checkSucceeded( final File baseDir )
  {
    Reader logFileReader = null;
    LineNumberReader reader = null;
    try
    {
      final File logFile = new File( baseDir, "out.2d" );
      logFileReader = new FileReader( logFile );
      reader = new LineNumberReader( logFileReader );
      String line;
      while( (line = reader.readLine()) != null )
      {
        if( line.indexOf( " KALYPSO-2D: Berechnung ordnungsgemaess gelaufen" ) >= 0 ) // TODO:check it in
          // the new 1D2D
          m_succeeded = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
      IOUtils.closeQuietly( logFileReader );
    }
  }

  public void chooseSimulationExe( GMLWorkspace controlWS )
  {
    Feature controlRootFE = controlWS.getRootFeature();
    String kalypso1D2DVersion = (String) controlRootFE.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VERSION );
    if( kalypso1D2DVersion.equals( "test" ) )
      m_kalypso1D2DKernelPath = SIM_EXE_FILE_TEST;
    else if( kalypso1D2DVersion.equals( "neueste" ) || kalypso1D2DVersion.equals( "latest" ) )
      m_kalypso1D2DKernelPath = SIM_EXE_FILE_3_5;
    else if( kalypso1D2DVersion.equals( "Version3.5" ) )
      m_kalypso1D2DKernelPath = SIM_EXE_FILE_3_5;
    else
    {
      logger.log( Level.WARNING, "Sie haben keine Version des Fortran Codes angegeben oder \n" + " die von Ihnen angegebene Version wird nicht weiter unterstützt.\n"
          + " Es wird mit der neuesten Version gerechnet." );
      m_kalypso1D2DKernelPath = SIM_EXE_FILE_3_5;
    }
  }
}