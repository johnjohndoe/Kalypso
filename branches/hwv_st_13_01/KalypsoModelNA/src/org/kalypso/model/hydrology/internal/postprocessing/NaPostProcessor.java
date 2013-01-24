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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.java.util.zip.GZipUtils;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.NaResultDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatistics;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class NaPostProcessor
{
  private static final String STRING_RESULT_SUCCESSFUL_1 = "berechnung wurde ohne fehler beendet"; //$NON-NLS-1$

  private static final String STRING_RESULT_SUCCESSFUL_2 = "Berechnung wurde ohne Fehler beendet!";

  private static final String FILENAME_OUTPUT_RES = "output.res"; //$NON-NLS-1$

  private final Logger m_logger;

  private final GMLWorkspace m_modelWorkspace;

  private boolean m_isSucceeded;

  private final NAModellControl m_naControl;

  private final HydroHash m_hydroHash;

  private final IDManager m_idManager;

  public NaPostProcessor( final IDManager idManager, final Logger logger, final GMLWorkspace modelWorkspace, final NAModellControl naControl, final HydroHash hydroHash )
  {
    m_idManager = idManager;
    m_logger = logger;
    m_modelWorkspace = modelWorkspace;
    m_naControl = naControl;
    m_hydroHash = hydroHash;
  }

  // FIXME: we need (much) better error handling! and error recovery...
  public void process( final NaAsciiDirs asciiDirs, final NaSimulationDirs simDirs ) throws Exception
  {
    final NaResultDirs currentResultDirs = simDirs.currentResultDirs;
    translateErrorGml( asciiDirs, currentResultDirs );

    copyNaExeLogs( asciiDirs, currentResultDirs );

    m_isSucceeded = checkSuccess( asciiDirs );
    if( !m_isSucceeded )
      return;

    final NAStatistics statistics = loadTSResults( asciiDirs.outWeNatDir, simDirs.currentResultDir );
    statistics.writeStatistics( simDirs.currentResultDir, currentResultDirs.reportDir );

    copyStatisticResultFile( asciiDirs, currentResultDirs );

    final Date[] initialDates = m_naControl.getInitialDatesToBeWritten();
    final LzsimReader lzsimManager = new LzsimReader( initialDates, currentResultDirs.anfangswertDir );
    lzsimManager.readInitialValues( m_idManager, m_hydroHash, asciiDirs.lzsimDir, m_logger );
  }

  private void copyNaExeLogs( final NaAsciiDirs asciiDirs, final NaResultDirs currentResultDirs )
  {
    try
    {
      GZipUtils.gzip( asciiDirs.output_res, currentResultDirs.logOutputRes );
      GZipUtils.gzip( asciiDirs.output_err, currentResultDirs.logOutputErr );
      GZipUtils.gzip( asciiDirs.exe_log, currentResultDirs.logExeLog );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to copy Kalypso-NA log files.", e.getLocalizedMessage() );
      m_logger.severe( msg );
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
  }

  private boolean checkSuccess( final NaAsciiDirs asciiDirs )
  {
    final String logContent = readOutputRes( asciiDirs.startDir );
    if( logContent == null )
      return false;

    if( logContent.contains( STRING_RESULT_SUCCESSFUL_1 ) )
      return true;
    if( logContent.contains( STRING_RESULT_SUCCESSFUL_2 ) )
      return true;

    return false;
  }

  private String readOutputRes( final File startDir )
  {
    try
    {
      return FileUtils.readFileToString( new File( startDir, FILENAME_OUTPUT_RES ), null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public boolean isSucceeded( )
  {
    return m_isSucceeded;
  }

  /** kopiere statistische Ergebnis-Dateien */
  // FIXME: why copy? bilanz does not belong to ascii and should be directly written to result dirs
  private void copyStatisticResultFile( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
  {
    resultDirs.bilanzDir.mkdirs();

    final String[] wildcards = new String[] { "*bil*" }; //$NON-NLS-1$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    final File outWeNatDir = asciiDirs.outWeNatDir;
    final File[] bilFiles = outWeNatDir.listFiles( filter );
    for( final File bilFile : bilFiles )
    {
      try
      {
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220", bilFile.getName() ) );
        final File resultFile = new File( resultDirs.bilanzDir, "Bilanz.txt" ); //$NON-NLS-1$ 
        FileUtils.copyFile( bilFile, resultFile );
      }
      catch( final IOException e )
      {
        final String inputPath = outWeNatDir.getName() + bilFile.getName();
        e.printStackTrace();

        final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224", inputPath, e.getLocalizedMessage() );
        m_logger.severe( msg );
      }
    }
  }

  private NAStatistics loadTSResults( final File outWeNatDir, final File resultDir ) throws SensorException
  {
    final ResultTimeseriesLoader resultProcessor = new ResultTimeseriesLoader( outWeNatDir, resultDir, m_modelWorkspace, m_idManager, m_logger );
    resultProcessor.processResults();
    return resultProcessor.getStatistics();
  }
}
