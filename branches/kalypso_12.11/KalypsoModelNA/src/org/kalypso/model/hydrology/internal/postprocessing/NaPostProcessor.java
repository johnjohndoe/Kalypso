/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.NaResultDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatistics;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class NaPostProcessor
{
  private static final String VERSION_PATTERN = ".*\\*+[ \\t]+VERS\\.[ \\t]+(\\d+\\.\\d+)\\.\\d+[ \\t\\w\\.:]+\\*+.*"; //$NON-NLS-1$

  private static final String STRING_RESULT_SUCCESSFUL_1 = "berechnung wurde ohne fehler beendet"; //$NON-NLS-1$

  private static final String FILENAME_OUTPUT_RES = "output.res"; //$NON-NLS-1$

  private final Logger m_logger;

  private final GMLWorkspace m_modelWorkspace;

  private final NAModellControl m_naControl;

  private final HydroHash m_hydroHash;

  private final IDManager m_idManager;

  private ENACoreResultsFormat m_coreResultsFormat;

  private IStatusCollector m_errorLog;

  public NaPostProcessor( final IDManager idManager, final Logger logger, final GMLWorkspace modelWorkspace, final NAModellControl naControl, final HydroHash hydroHash )
  {
    m_idManager = idManager;
    m_logger = logger;
    m_modelWorkspace = modelWorkspace;
    m_naControl = naControl;
    m_hydroHash = hydroHash;
    m_errorLog = null;
  }

  // FIXME: we need (much) better error handling! and error recovery...
  public void process( final NaAsciiDirs asciiDirs, final NaSimulationDirs simDirs ) throws Exception
  {
    final NaResultDirs currentResultDirs = simDirs.currentResultDirs;
    translateErrorGml( asciiDirs, currentResultDirs );

    copyNaExeLogs( asciiDirs, currentResultDirs );

    try
    {
      checkSuccessAndResultsFormat( asciiDirs );

      final NAStatistics statistics = loadTSResults( asciiDirs.outWeNatDir, simDirs.currentResultDir );
      statistics.writeStatistics( simDirs.currentResultDir, currentResultDirs.reportDir );

      copyStatisticResultFile( asciiDirs, currentResultDirs );

      final Date[] initialDates = m_naControl.getInitialDatesToBeWritten();
      final LzsimReader lzsimManager = new LzsimReader( initialDates, currentResultDirs.anfangswertDir );
      lzsimManager.readInitialValues( m_idManager, m_hydroHash, asciiDirs.lzsimDir, m_logger );
    }
    catch( final SimulationException e )
    {
      m_errorLog.add( IStatus.ERROR, Messages.getString("NaPostProcessor.2"), e ); //$NON-NLS-1$
    }
  }

  private void copyNaExeLogs( final NaAsciiDirs asciiDirs, final NaResultDirs currentResultDirs )
  {
    ZipOutputStream zos = null;
    try
    {
      zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( currentResultDirs.exe_logs_zip ) ) );
      // REMARK: We rename the files in the zip, else the windoes explorer will show an empty zip by default (unknown
      // extensions)
      ZipUtilities.writeZipEntry( zos, asciiDirs.output_res, "output.txt" ); //$NON-NLS-1$
      ZipUtilities.writeZipEntry( zos, asciiDirs.output_err, "error.txt" ); //$NON-NLS-1$
      zos.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "NaPostProcessor.4" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      m_logger.severe( msg );
    }
    finally
    {
      IOUtils.closeQuietly( zos );
    }
  }

  /**
   * Translates the id inside the KalypsoNA log to KalypsoHydrology id's.
   */
  private void translateErrorGml( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
  {
    final NaFortranLogTranslater logTranslater = new NaFortranLogTranslater( asciiDirs.asciiDir, m_idManager, m_logger );

    final File resultFile = new File( resultDirs.logDir, "error.gml" ); //$NON-NLS-1$
    resultFile.getParentFile().mkdirs();

    logTranslater.translate( resultFile );

    final IStatusCollector errorLog = logTranslater.getErrorLog();
    m_errorLog = errorLog;
  }

  /**
   * Checks if the calculation was successful or not; if yes, checks for the results file version. Starting from NA core
   * 2.2 dates for block time series are written as YYYYMMDD, older versions are using YYMMDD format.
   */
  private void checkSuccessAndResultsFormat( final NaAsciiDirs asciiDirs ) throws SimulationException
  {
    final List<String> logContent = readOutputRes( asciiDirs.startDir );
    if( logContent == null || logContent.size() == 0 )
      throw new SimulationException( Messages.getString( "NaPostProcessor.0" ) ); //$NON-NLS-1$

    checkLogForSuccess( logContent );

    m_coreResultsFormat = findCoreResultsFormat( logContent );
  }

  private ENACoreResultsFormat findCoreResultsFormat( final List<String> logContent )
  {
    final Pattern versionPattern = Pattern.compile( VERSION_PATTERN );
    for( int i = 0; i < logContent.size(); i++ )
    {
      final String line = logContent.get( i );
      final Matcher matcher = versionPattern.matcher( line );
      if( matcher.matches() )
      {
        final String[] strings = matcher.group( 1 ).split( "\\." ); //$NON-NLS-1$
        if( Integer.parseInt( strings[0] ) > 2 )
          return ENACoreResultsFormat.FMT_2_2_AND_NEWER;

        if( Integer.parseInt( strings[0] ) == 2 )
        {
          if( Integer.parseInt( strings[1] ) >= 2 )
            return ENACoreResultsFormat.FMT_2_2_AND_NEWER;
          else
            return ENACoreResultsFormat.FMT_2_1_AND_OLDER;
        }

        return ENACoreResultsFormat.FMT_2_1_AND_OLDER;
      }
    }

    return ENACoreResultsFormat.FMT_2_2_AND_NEWER;
  }

  private void checkLogForSuccess( final List<String> logContent ) throws SimulationException
  {
    for( int i = logContent.size() - 1; i >= 0; i-- )
    {
      final String line = logContent.get( i ).toLowerCase();
      if( line.contains( STRING_RESULT_SUCCESSFUL_1 ) )
        return;
    }

    throw new SimulationException( Messages.getString( "NaPostProcessor.1" ) ); //$NON-NLS-1$
  }

  private List<String> readOutputRes( final File startDir )
  {
    try
    {
      return FileUtils.readLines( new File( startDir, FILENAME_OUTPUT_RES ), (Charset) null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /** kopiere statistische Ergebnis-Dateien */
  // FIXME: why copy? bilanz does not belong to ascii and should be directly written to result dirs
  private void copyStatisticResultFile( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
  {
    resultDirs.reportDir.mkdirs();

    final String[] wildcards = new String[] { "*bil*" }; //$NON-NLS-1$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    final File outWeNatDir = asciiDirs.outWeNatDir;
    final File[] bilFiles = outWeNatDir.listFiles( filter );
    for( final File bilFile : bilFiles )
    {
      try
      {
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220", bilFile.getName() ) ); //$NON-NLS-1$

        FileUtils.copyFile( bilFile, resultDirs.bilanceFile );
      }
      catch( final IOException e )
      {
        final String inputPath = outWeNatDir.getName() + bilFile.getName();
        e.printStackTrace();

        final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224", inputPath, e.getLocalizedMessage() ); //$NON-NLS-1$
        m_logger.severe( msg );
      }
    }
  }

  private NAStatistics loadTSResults( final File outWeNatDir, final File resultDir ) throws SensorException
  {
    final ResultTimeseriesLoader resultProcessor = new ResultTimeseriesLoader( outWeNatDir, resultDir, m_modelWorkspace, m_idManager, getCoreResultsFormat(), m_logger );
    resultProcessor.processResults();
    return resultProcessor.getStatistics();
  }

  public ENACoreResultsFormat getCoreResultsFormat( )
  {
    return m_coreResultsFormat;
  }

  public IStatusCollector getErrorLog( )
  {
    return m_errorLog;
  }
}