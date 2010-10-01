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
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author Gernot Belger
 */
public class WspmTuhhPostProcessor
{
  private static final String TITLE_PATTERN_REIB_CONST = "<runoff>"; //$NON-NLS-1$

  private static final String TITLE_PATTERN_WATERLEVEL = "<calcname>"; //$NON-NLS-1$

  private static final String LSFILE_PATTERN_WATERLEVEL = "L‰ngsschnitt.gml";//$NON-NLS-1$ 

  private static final String LSFILE_PATTERN_REIB_CONST = "lengthSection_<runoff>.gml";//$NON-NLS-1$ 

  private final LogHelper m_log;

  private final File m_dathDir;

  private final ISimulationResultEater m_resultEater;

  private final TuhhCalculation m_calculation;

  private final String m_epsThinning;

  private final File m_tmpDir;

  private final URL m_ovwMapURL;

  public WspmTuhhPostProcessor( final TuhhCalculation calculation, final File tmpDir, final File dathDir, final String epsThinning, final URL ovwMapURL, final ISimulationResultEater resultEater, final LogHelper log )
  {
    m_calculation = calculation;
    m_tmpDir = tmpDir;
    m_dathDir = dathDir;
    m_epsThinning = epsThinning;
    m_ovwMapURL = ovwMapURL;
    m_resultEater = resultEater;
    m_log = log;
  }

  private void addResult( final String id, final File file, final String messageKey, final String errorKey ) throws SimulationException
  {
    if( file != null && file.exists() )
    {
      m_resultEater.addResult( id, file ); //$NON-NLS-1$
      if( messageKey != null )
        m_log.log( false, Messages.getString( messageKey ) );
    }
    else
    {
      if( errorKey != null )
        m_log.log( false, Messages.getString( errorKey ) );
    }
  }

  public void run( final ISimulationMonitor monitor ) throws Exception
  {
    // load results + copy to result folder + unzip templates
    monitor.setProgress( 80 );
    monitor.setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.13" ) ); //$NON-NLS-1$
    m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.14" ) ); //$NON-NLS-1$

    // alle Modi
    final File ctrlFile = new File( m_dathDir, "Kontroll.log" ); //$NON-NLS-1$
    addResult( "ControlFile", ctrlFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.15", "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.16" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File beiwerteFile = new File( m_dathDir, "Beiwerte.aus" ); //$NON-NLS-1$
    addResult( "BeiwerteAus", beiwerteFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.17", "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.18" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File lambdaFile = new File( m_dathDir, "lambda_i.txt" ); //$NON-NLS-1$
    addResult( "LambdaI", lambdaFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.19", "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.20" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$    

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
        processWaterlevel( monitor );
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

  private void processWaterlevel( final ISimulationMonitor monitor ) throws Exception
  {
    final LengthSectionParser lsProcessor = new LengthSectionParser( m_calculation, new File( m_dathDir, "laengsschnitt.txt" ), m_resultEater, m_tmpDir, m_epsThinning, TITLE_PATTERN_WATERLEVEL, LSFILE_PATTERN_WATERLEVEL ); //$NON-NLS-1$
    final IStatus lsResult = lsProcessor.process( m_log );
    if( !lsResult.isOK() )
      m_log.log( false, lsResult.toString() );
    // TODO: check this error handling
    if( lsResult.getSeverity() == IStatus.ERROR )
    {
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.21" ) ); //$NON-NLS-1$
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.22" ) ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.22" ) ); //$NON-NLS-1$
      return;
    }

    if( m_log.checkCanceled() )
      return;

    final LengthSectionProcessor[] processedLengthSections = lsProcessor.getProcessedLengthSections();
    if( processedLengthSections.length < 1 )
    {
      m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ), new Object[0] ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.23" ) ); //$NON-NLS-1$
      return;
    }
    else if( processedLengthSections.length > 1 )
    {
      m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ), new Object[0] ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.24" ) ); //$NON-NLS-1$
      return;
    }
    final LengthSectionProcessor processedLS = processedLengthSections[0];

    final File gmlFile = processedLS.getGmlFile();
    addResult( "LengthSectionGml", gmlFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.25" , null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File diagFile = processedLS.getDiagFile();
    addResult( "LengthSectionDiag", diagFile , "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.26", null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File tableFile = processedLS.getTableFile();
    addResult( "LengthSectionTab", tableFile , "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.27", null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File breaklineFile = processedLS.getBreaklineFile();
    addResult( "Bruchkanten", breaklineFile , "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.28", null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File tinFile = processedLS.getTinFile();
    addResult( "WspTin", tinFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.29", null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File tinSldFile = processedLS.getTinSldFile();
    addResult( "WspTinSld", tinSldFile, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File boundaryFile = processedLS.getBoundaryFile();
    addResult( "Modellgrenzen", boundaryFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.30", null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File waterlevelFile = processedLS.getWaterlevelFile();
    addResult( "Ueberschwemmungslinie", waterlevelFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.31", null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( m_log.checkCanceled() )
      return;

    //
    // overview map (generated by token replacement) either from OVW_MAP_SPECIAL or OVW_MAP_GENERAL
    //
    // TODO: maybe move into LengthSectionProcessor?
    if( m_ovwMapURL != null )
    {
      final TuhhReach reach = m_calculation.getReach();
      final String mapContent = FileUtilities.toString( m_ovwMapURL, "UTF-8" ); //$NON-NLS-1$
      final FeaturePath ftPath = reach.getWorkspace().getFeaturepathForFeature( reach );
      final String newMapContent = mapContent.replaceAll( "%FID%", ftPath.toString() ); //$NON-NLS-1$
      final File mapFile = new File( m_tmpDir, "map.gmt" ); //$NON-NLS-1$
      FileUtils.writeStringToFile( mapFile, newMapContent, "UTF-8" ); //$NON-NLS-1$

      addResult( "OvwMap", mapFile, "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.32", null );//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    if( m_log.checkCanceled() )
      return;

    // *.tab (-> fixed name "Ergebnis.list")
    final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" ); //$NON-NLS-1$
    final File[] ergListFile = m_dathDir.listFiles( ergListFilter );
    if( ergListFile.length > 0 )
    {
      // assumption: max. one TAB-file
      addResult( "resultList", ergListFile[0], "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.0", null );//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
      addResult( "bfLengthSection", bfLenSecFile[0] , "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.1",null  ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // *.tab (-> fixed name "Ergebnis.list")
    final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" ); //$NON-NLS-1$
    final File[] ergListFile = m_dathDir.listFiles( ergListFilter );
    // assumption: max. one TAB-file
    if( ergListFile.length > 0 )
      addResult( "resultList", ergListFile[0], "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.2", null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
      addResult( "bfLengthSection", bfLenSecFile[0], "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.3", null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    processMultipleRunoffs( monitor );
  }

  private void processReibKonst( final ISimulationMonitor monitor ) throws Exception
  {
    processMultipleRunoffs( monitor );
  }

  private void processMultipleRunoffs( final ISimulationMonitor monitor ) throws SimulationException, Exception
  {
    /* Process length sections */
    /* Parse Q_LangSchnitt.txt into several length-sections */
    final File lsOutDir = new File( m_tmpDir, "LengthSections" ); //$NON-NLS-1$
    lsOutDir.mkdirs();
    m_resultEater.addResult( "resultListsNonUni", lsOutDir ); //$NON-NLS-1$

    final LengthSectionParser lsProcessor = new LengthSectionParser( m_calculation, new File( m_dathDir, "Q_LangSchnitt.txt" ), m_resultEater, lsOutDir, m_epsThinning, TITLE_PATTERN_REIB_CONST, LSFILE_PATTERN_REIB_CONST ); //$NON-NLS-1$
    final IStatus lsResult = lsProcessor.process( m_log );
    if( !lsResult.isOK() )
      m_log.log( false, lsResult.toString() );
    // TODO: check this error handling
    if( lsResult.getSeverity() == IStatus.ERROR )
    {
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.4" ) ); //$NON-NLS-1$
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.5" ) ); //$NON-NLS-1$
      return;
    }

    m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.6" ) ); //$NON-NLS-1$
    if( m_log.checkCanceled() )
      return;

    if( m_log.checkCanceled() )
      return;

    /* Calculate and fetch Polynomes */
    final File polynomeTmpDir = new File( m_tmpDir, "polynome" ); //$NON-NLS-1$
    PolynomeHelper.processPolynomes( polynomeTmpDir, m_dathDir, m_log, 0, m_resultEater, m_calculation );
  }
}
