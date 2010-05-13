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
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.MODE;
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
import org.kalypsodeegree_impl.model.feature.FeaturePath;
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

  private static final String TITLE_PATTERN_REIB_CONST = "<runoff>"; //$NON-NLS-1$

  private static final String TITLE_PATTERN_WATERLEVEL = "<calcname>"; //$NON-NLS-1$

  private static final String LSFILE_PATTERN_WATERLEVEL = "Längsschnitt.gml";//$NON-NLS-1$ 

  private static final String LSFILE_PATTERN_REIB_CONST = "lengthSection_<runoff>.gml";//$NON-NLS-1$ 

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
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL modellGmlURL = (URL) inputProvider.getInputForID( INPUT_MODELL_GML );
    final String calcXPath = (String) inputProvider.getInputForID( INPUT_CALC_PATH );
    final String epsThinning = (String) inputProvider.getInputForID( INPUT_EPS_THINNING );
    URL ovwMapURL = null;
    if( inputProvider.hasID( INPUT_OVW_MAP_GENERAL ) )
      ovwMapURL = (URL) inputProvider.getInputForID( INPUT_OVW_MAP_GENERAL );
    if( inputProvider.hasID( INPUT_OVW_MAP_SPECIAL ) )
      ovwMapURL = (URL) inputProvider.getInputForID( INPUT_OVW_MAP_SPECIAL );

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

      // load results + copy to result folder + unzip templates
      monitor.setProgress( 80 );
      monitor.setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.13" ) ); //$NON-NLS-1$
      pwSimuLog.println( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.14" ) ); //$NON-NLS-1$

      // alle Modi
      final File ctrlFile = new File( dathDir, "Kontroll.log" ); //$NON-NLS-1$
      if( ctrlFile.exists() )
      {
        resultEater.addResult( "ControlFile", ctrlFile ); //$NON-NLS-1$

        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.15" ) ); //$NON-NLS-1$
      }
      else
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.16" ) ); //$NON-NLS-1$

      final File beiwerteFile = new File( dathDir, "Beiwerte.aus" ); //$NON-NLS-1$
      if( beiwerteFile.exists() )
      {
        resultEater.addResult( "BeiwerteAus", beiwerteFile ); //$NON-NLS-1$
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.17" ) ); //$NON-NLS-1$
      }
      else
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.18" ) ); //$NON-NLS-1$

      final File lambdaFile = new File( dathDir, "lambda_i.txt" ); //$NON-NLS-1$
      if( lambdaFile.exists() )
      {
        resultEater.addResult( "LambdaI", lambdaFile ); //$NON-NLS-1$
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.19" ) ); //$NON-NLS-1$
      }
      log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.20" ) ); //$NON-NLS-1$

      if( log.checkCanceled() )
        return;

      final MODE calcMode = calculation.getCalcMode();
      if( log.checkCanceled() )
        return;

      final TuhhReach reach = calculation.getReach();

      switch( calcMode )
      {
        case WATERLEVEL:
        {
          final LengthSectionParser lsProcessor = new LengthSectionParser( calculation, new File( dathDir, "laengsschnitt.txt" ), resultEater, tmpDir, epsThinning, TITLE_PATTERN_WATERLEVEL, LSFILE_PATTERN_WATERLEVEL ); //$NON-NLS-1$
          final IStatus lsResult = lsProcessor.process( log );
          if( !lsResult.isOK() )
            log.log( false, lsResult.toString() );
          // TODO: check this error handling
          if( lsResult.getSeverity() == IStatus.ERROR )
          {
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.21" ) ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.22" ) ); //$NON-NLS-1$
            monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.22" ) ); //$NON-NLS-1$
            return;
          }

          if( log.checkCanceled() )
            return;

          final LengthSectionProcessor[] processedLengthSections = lsProcessor.getProcessedLengthSections();
          if( processedLengthSections.length < 1 )
          {
            log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ), new Object[0] ); //$NON-NLS-1$
            monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ) ); //$NON-NLS-1$
            return;
          }
          else if( processedLengthSections.length > 1 )
          {
            log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ), new Object[0] ); //$NON-NLS-1$
            monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ) ); //$NON-NLS-1$
            return;
          }
          final LengthSectionProcessor processedLS = processedLengthSections[0];

          final File gmlFile = processedLS.getGmlFile();
          if( gmlFile.exists() )
          {
            resultEater.addResult( "LengthSectionGml", gmlFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.25" ) ); //$NON-NLS-1$
          }

          final File diagFile = processedLS.getDiagFile();
          if( diagFile != null && diagFile.exists() )
          {
            resultEater.addResult( "LengthSectionDiag", diagFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.26" ) ); //$NON-NLS-1$
          }

          final File tableFile = processedLS.getTableFile();
          if( tableFile != null && tableFile.exists() )
          {
            resultEater.addResult( "LengthSectionTab", tableFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.27" ) ); //$NON-NLS-1$
          }

          final File breaklineFile = processedLS.getBreaklineFile();
          if( breaklineFile.exists() )
          {
            resultEater.addResult( "Bruchkanten", breaklineFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.28" ) ); //$NON-NLS-1$
          }

          final File tinFile = processedLS.getTinFile();
          if( tinFile.exists() )
          {
            resultEater.addResult( "WspTin", tinFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.29" ) ); //$NON-NLS-1$
          }

          final File tinSldFile = processedLS.getTinSldFile();
          if( tinSldFile.exists() )
            resultEater.addResult( "WspTinSld", tinSldFile ); //$NON-NLS-1$

          final File boundaryFile = processedLS.getBoundaryFile();
          if( boundaryFile.exists() )
          {
            resultEater.addResult( "Modellgrenzen", boundaryFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.30" ) ); //$NON-NLS-1$
          }

          final File waterlevelFile = processedLS.getWaterlevelFile();
          if( waterlevelFile.exists() )
          {
            resultEater.addResult( "Ueberschwemmungslinie", waterlevelFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.31" ) ); //$NON-NLS-1$
          }

          if( log.checkCanceled() )
            return;

          //
          // overview map (generated by token replacement) either from OVW_MAP_SPECIAL or OVW_MAP_GENERAL
          //
          // TODO: maybe move into LengthSectionProcessor?
          if( ovwMapURL != null )
          {
            final String mapContent = FileUtilities.toString( ovwMapURL, "UTF-8" );
            final FeaturePath ftPath = reach.getWorkspace().getFeaturepathForFeature( reach );
            final String newMapContent = mapContent.replaceAll( "%FID%", ftPath.toString() ); //$NON-NLS-1$
            final File mapFile = new File( tmpDir, "map.gmt" ); //$NON-NLS-1$
            FileUtils.writeStringToFile( mapFile, newMapContent, "UTF-8" );
            resultEater.addResult( "OvwMap", mapFile ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.32" ) ); //$NON-NLS-1$
          }

          if( log.checkCanceled() )
            return;

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" ); //$NON-NLS-1$
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] ); //$NON-NLS-1$

            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.0" ) ); //$NON-NLS-1$
          }

          break;
        }

        case BF_UNIFORM:
        {
          // bankfull uniform
          // *.qb2 = Q als Treppenfunktion, we don't fetch it
          // *.qb1 = bankfull-lengthsection
          final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" ); //$NON-NLS-1$
          final File[] bfLenSecFile = dathDir.listFiles( qb1Filter );
          if( bfLenSecFile.length > 0 )
          {
            // assumption: max. one QB1
            resultEater.addResult( "bfLengthSection", bfLenSecFile[0] ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.1" ) ); //$NON-NLS-1$
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" ); //$NON-NLS-1$
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.2" ) ); //$NON-NLS-1$
          }

          break;
        }

        case BF_NON_UNIFORM:
        {
          // bankfull nonuniform

          // TODO: also parse QLang-File?

          // TODO :check everything below if it still makes sense?

          // *.qb2 = Q als Treppenfunktion, we don't fetch it
          // *.qb1 = bankfull-lengthsection
          final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" ); //$NON-NLS-1$
          final File[] bfLenSecFile = dathDir.listFiles( qb1Filter );
          if( bfLenSecFile.length > 0 )
          {
            // assumption: max. one QB1
            resultEater.addResult( "bfLengthSection", bfLenSecFile[0] ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.3" ) ); //$NON-NLS-1$
          }

          /* break */
          // FALL THROUGH
        }

        case REIB_KONST:
        {
          /* Process length sections */
          /* Parse Q_LangSchnitt.txt into several length-sections */
          final File lsOutDir = new File( tmpDir, "LengthSections" ); //$NON-NLS-1$
          lsOutDir.mkdirs();
          resultEater.addResult( "resultListsNonUni", lsOutDir ); //$NON-NLS-1$

          final LengthSectionParser lsProcessor = new LengthSectionParser( calculation, new File( dathDir, "Q_LangSchnitt.txt" ), resultEater, lsOutDir, epsThinning, TITLE_PATTERN_REIB_CONST, LSFILE_PATTERN_REIB_CONST ); //$NON-NLS-1$
          final IStatus lsResult = lsProcessor.process( log );
          if( !lsResult.isOK() )
            log.log( false, lsResult.toString() );
          // TODO: check this error handling
          if( lsResult.getSeverity() == IStatus.ERROR )
          {
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.4" ) ); //$NON-NLS-1$
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
            monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
            return;
          }

          log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.6" ) ); //$NON-NLS-1$
          if( log.checkCanceled() )
            return;

          if( log.checkCanceled() )
            return;

          /* Calculate and fetch Polynomes */
          final File polynomeTmpDir = new File( tmpDir, "polynome" ); //$NON-NLS-1$
          PolynomeHelper.processPolynomes( polynomeTmpDir, dathDir, log, 0, resultEater, calculation );

          break;
        }
      }
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
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALCJOB_SPEC );
  }
}
