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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.results.processing.IResultLSFile;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * @author Gernot Belger
 */
public class WspmTuhhPostProcessor
{
  private static final String TITLE_PATTERN_REIB_CONST = "<runoff>"; //$NON-NLS-1$

  private static final String TITLE_PATTERN_WATERLEVEL = "<calcname>"; //$NON-NLS-1$

  private static final String LSFILE_PATTERN_WATERLEVEL = IWspmTuhhConstants.FILE_LAENGSSCHNITT_GML;

  private static final String LSFILE_PATTERN_REIB_CONST = "lengthSection_<runoff>.gml";//$NON-NLS-1$ 

  private final LogHelper m_log;

  private final File m_dathDir;

  private final ISimulationResultEater m_resultEater;

  private final TuhhCalculation m_calculation;

  private final String m_epsThinning;

  private final File m_tmpDir;

  private final URL m_ovwMapURL;

  private final File m_profDir;

  public WspmTuhhPostProcessor( final TuhhCalculation calculation, final File tmpDir, final File dathDir, final File profDir, final String epsThinning, final URL ovwMapURL, final ISimulationResultEater resultEater, final LogHelper log )
  {
    m_calculation = calculation;
    m_tmpDir = tmpDir;
    m_dathDir = dathDir;
    m_profDir = profDir;
    m_epsThinning = epsThinning;
    m_ovwMapURL = ovwMapURL;
    m_resultEater = resultEater;
    m_log = log;
  }

  private void addResult( final IResultLSFile resultFile ) throws SimulationException
  {
    final String resultID = resultFile.getResultID();
    final File outputFile = resultFile.getResultFile();
    addResult( resultID, outputFile, resultFile.getTitle() );
  }

  private void addResult( final String id, final File file, final String title ) throws SimulationException
  {
    final String message = String.format( "- %s", title ); //$NON-NLS-1$
    final String error = String.format( Messages.getString( "WspmTuhhPostProcessor.1" ), title ); //$NON-NLS-1$

    // FIXME: what about optional files?
    if( file != null && file.exists() )
    {
      m_resultEater.addResult( id, file ); //$NON-NLS-1$
      m_log.log( false, message );
    }
    else
      m_log.log( false, error );
  }

  public void run( final ISimulationMonitor monitor ) throws Exception
  {
    // load results + copy to result folder + unzip templates
    monitor.setProgress( 80 );
    monitor.setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.13" ) ); //$NON-NLS-1$
    m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.14" ) ); //$NON-NLS-1$

    // alle Modi
    final File ctrlFile = new File( m_dathDir, "Kontroll.log" ); //$NON-NLS-1$
    addResult( "ControlFile", ctrlFile, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.15" ) ); //$NON-NLS-1$ //$NON-NLS-2$ 

    final File beiwerteFile = new File( m_dathDir, "Beiwerte.aus" ); //$NON-NLS-1$
    addResult( "BeiwerteAus", beiwerteFile, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.17" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    final File lambdaFile = new File( m_dathDir, "lambda_i.txt" ); //$NON-NLS-1$
    addResult( "LambdaI", lambdaFile, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.19" ) ); //$NON-NLS-1$ //$NON-NLS-2$    

    // *.tab (-> fixed name "Ergebnis.list")
    final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" ); //$NON-NLS-1$
    final File[] ergListFile = m_dathDir.listFiles( ergListFilter );
    // assumption: max. one TAB-file
    if( ergListFile.length > 0 )
      addResult( "resultList", ergListFile[0], Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$ 

    if( m_log.checkCanceled() )
      return;

    processCalculationMode( monitor );
  }

  private void processCalculationMode( final ISimulationMonitor monitor ) throws Exception
  {
    final MODE calcMode = m_calculation.getCalcMode();
    if( m_log.checkCanceled() )
      return;

    switch( calcMode )
    {
      case WATERLEVEL:
        processWaterlevel();
        return;

      case BF_UNIFORM:
        processBfUniform();
        return;

      case BF_NON_UNIFORM:
        processBfNonUniform( monitor );
        return;

      case REIB_KONST:
        processReibKonst( monitor );
        return;
    }
  }

  private void processWaterlevel( ) throws SimulationException, IOException
  {
    final File inputLSfile = new File( m_dathDir, "laengsschnitt.txt" ); //$NON-NLS-1$
    final LengthSectionParser lsProcessor = new LengthSectionParser( m_calculation, inputLSfile, m_resultEater, m_tmpDir, m_epsThinning, TITLE_PATTERN_WATERLEVEL, LSFILE_PATTERN_WATERLEVEL, m_ovwMapURL ); //$NON-NLS-1$
    final IStatus lsResult = lsProcessor.process( m_log );
    if( !lsResult.isOK() )
      m_log.log( false, lsResult.toString() );

    if( lsResult.matches( IStatus.ERROR ) )
    {
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.21" ) ); //$NON-NLS-1$
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.22" ) ); //$NON-NLS-1$
      // REMARK just return, no exception, else we get a ugly trace in the console.
      return;
    }

    if( m_log.checkCanceled() )
      return;

    final ResultLengthSection[] processedLengthSections = lsProcessor.getProcessedLengthSections();
    if( processedLengthSections.length < 1 )
    {
      m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ), new Object[0] ); //$NON-NLS-1$
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ) ); //$NON-NLS-1$
    }
    else if( processedLengthSections.length > 1 )
    {
      m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ), new Object[0] ); //$NON-NLS-1$
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ) ); //$NON-NLS-1$
    }

    final ResultLengthSection processedLS = processedLengthSections[0];
    final IResultLSFile[] resultFiles = processedLS.getResultFiles();
    for( final IResultLSFile resultLSFile : resultFiles )
    {
      if( m_log.checkCanceled() )
        return;

      addResult( resultLSFile );
    }
  }

  private void processBfUniform( ) throws SimulationException
  {
    // bankfull uniform
    // *.qb2 = Q als Treppenfunktion, we don't fetch it
    // *.qb1 = bankfull-lengthsection
    final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" ); //$NON-NLS-1$
    final File[] bfLenSecFile = m_dathDir.listFiles( qb1Filter );
    // assumption: max. one QB1
    if( bfLenSecFile.length > 0 )
      addResult( "bfLengthSection", bfLenSecFile[0], Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /** bankfull nonuniform */
  private void processBfNonUniform( final ISimulationMonitor monitor ) throws Exception
  {
    // TODO: also parse QLang-File?

    // TODO :check everything below if it still makes sense?

    // *.qb2 = Q als Treppenfunktion, we don't fetch it
    // *.qb1 = bankfull-lengthsection
    final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" ); //$NON-NLS-1$
    final File[] bfLenSecFile = m_dathDir.listFiles( qb1Filter );
    // assumption: max. one QB1
    if( bfLenSecFile.length > 0 )
      addResult( "bfLengthSection", bfLenSecFile[0], Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    processMultipleRunoffs( monitor, false );
  }

  private void processReibKonst( final ISimulationMonitor monitor ) throws Exception
  {
    processMultipleRunoffs( monitor, true );
  }

  private File processMultipleRunoffs( final ISimulationMonitor monitor, final boolean processPolynoms ) throws SimulationException, Exception
  {
    /* Process length sections */
    /* Parse Q_LangSchnitt.txt into several length-sections */
    final File lsOutDir = new File( m_tmpDir, "LengthSections" ); //$NON-NLS-1$
    lsOutDir.mkdirs();
    m_resultEater.addResult( "resultListsNonUni", lsOutDir ); //$NON-NLS-1$

    final LengthSectionParser lsProcessor = new LengthSectionParser( m_calculation, new File( m_dathDir, "Q_LangSchnitt.txt" ), m_resultEater, lsOutDir, m_epsThinning, TITLE_PATTERN_REIB_CONST, LSFILE_PATTERN_REIB_CONST, null ); //$NON-NLS-1$
    final IStatus lsResult = lsProcessor.process( m_log );
    if( !lsResult.isOK() )
      m_log.log( false, lsResult.toString() );
    // TODO: check this error handling
    if( lsResult.getSeverity() == IStatus.ERROR )
    {
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.4" ) ); //$NON-NLS-1$
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
      return null;
    }

    m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.6" ) ); //$NON-NLS-1$
    if( m_log.checkCanceled() )
      return null;

    if( m_log.checkCanceled() )
      return null;

    m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.13" ) ); //$NON-NLS-1$

    final MultipleRunoffReader runoffReader = new MultipleRunoffReader( m_tmpDir, m_profDir, m_calculation, m_log );
    runoffReader.init();
    runoffReader.readKM();

    if( processPolynoms )
    {
      final File polynomeTmpDir = new File( m_tmpDir, "polynome" ); //$NON-NLS-1$
      final PolynomeProcessor processor = new PolynomeProcessor( polynomeTmpDir, m_dathDir, m_calculation, m_log );
      final File resultDir = processor.processPolynomes();
      runoffReader.readPolynomeResults( polynomeTmpDir, resultDir, m_log, m_resultEater );
    }

    runoffReader.createSumComponents();
    runoffReader.createResult( m_resultEater );

    return lsOutDir;
  }
}
