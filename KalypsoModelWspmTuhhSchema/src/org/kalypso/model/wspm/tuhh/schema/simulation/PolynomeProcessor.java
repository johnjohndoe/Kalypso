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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.Locale;
import java.util.SortedMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties.TripleMode;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * Helper class to start the processing of the polynomes.
 * 
 * @author Gernot Belger
 */
public class PolynomeProcessor
{
  private static final int TIMEOUT = 0;

  public static final String POLYNOME_1D_EXE_FORMAT = "Polynome1d%s.exe";//$NON-NLS-1$ 

  public static final String POLYNOME_1D_EXE_PATTERN = "Polynome1d(.*).exe";//$NON-NLS-1$ 

  // TODO: Deciding which approach to take, energy level or water stage

// private static final String WEIR_FILE_NAME = "HOW_QWehr_HUW.txt";
  static final String WEIR_FILE_NAME = "EOW_QWehr_EUW.txt"; //$NON-NLS-1$

// private static final String BRIDGE_FILE_NAME = "HOW_QBruecke_HUW.txt";
  static final String BRIDGE_FILE_NAME = "EOW_QBruecke_EUW.txt"; //$NON-NLS-1$

  private static final String QLANG_FILE_NAME = "Q_LangSchnitt.txt"; //$NON-NLS-1$

  private final File m_tmpDir;

  private final File m_dathDir;

  private final TuhhCalculation m_calculation;

  private final LogHelper m_log;

  public PolynomeProcessor( final File tmpDir, final File dathDir, final TuhhCalculation calculation, final LogHelper log )
  {
    m_tmpDir = tmpDir;
    m_dathDir = dathDir;
    m_calculation = calculation;
    m_log = log;
  }

  public File processPolynomes( ) throws SimulationException
  {
    final ISimulationMonitor monitor = m_log.getMonitor();

    m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.3" ) ); //$NON-NLS-1$

    m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.4" ) ); //$NON-NLS-1$

    final File eingangDir = preparePolynomes();
    final File resultDir = new File( m_tmpDir, "02Ausgang" ); //$NON-NLS-1$
    /* Need to create result-dir, else the calculation does not work */
    resultDir.mkdirs();
    if( eingangDir == null )
      return null;

    if( monitor.isCanceled() )
      return null;

    m_log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.5" ) ); //$NON-NLS-1$
    prepareSteuerpoly();

    if( monitor.isCanceled() )
      return null;

    final File logFile = new File( m_tmpDir, "Polynome1d.log" ); //$NON-NLS-1$
    final File errFile = new File( m_tmpDir, "Polynome1d.err" ); //$NON-NLS-1$

    OutputStream logStream = null;
    OutputStream errStream = null;
    try
    {
      logStream = new BufferedOutputStream( new FileOutputStream( logFile ) );
      errStream = new BufferedOutputStream( new FileOutputStream( errFile ) );

      /* Start the polynome1d process */
      final File exeFile = WspmTuhhCalcJob.getExecuteable( m_calculation, m_tmpDir, POLYNOME_1D_EXE_FORMAT, POLYNOME_1D_EXE_PATTERN, monitor );
      if( exeFile == null )
        throw new SimulationException( "Couldn't resolve Polynome1d.exe" ); //$NON-NLS-1$

      final String cmdLine = "cmd.exe /C \"" + exeFile.getAbsolutePath() + "\""; //$NON-NLS-1$ //$NON-NLS-2$
      ProcessHelper.startProcess( cmdLine, null, m_tmpDir, monitor, TIMEOUT, logStream, errStream, null );

      logStream.close();
      errStream.close();

      /* The weir and brigde files are not processed by the polynome.exe, just copy it to the result-folder */
      FileUtils.copyFileToDirectory( new File( eingangDir, WEIR_FILE_NAME ), resultDir );
      FileUtils.copyFileToDirectory( new File( eingangDir, BRIDGE_FILE_NAME ), resultDir );
    }
    catch( final IOException e )
    {
      m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.8" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.9" ) ); //$NON-NLS-1$
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.10" ) + e.getLocalizedMessage(), e ); //$NON-NLS-1$
    }
    catch( final ProcessTimeoutException e )
    {
      m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.11" ) ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.11" ) ); //$NON-NLS-1$
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( logStream );
      IOUtils.closeQuietly( errStream );
    }

    if( m_log.checkCanceled() )
      return null;

    return resultDir;
  }

  /**
   * Prepares the input files for the polynome process
   * 
   * @param tmpDir
   *          any tmp dir, must be empty before start, may be deleted after end
   * @param dathDir
   *          Directory containing the laengsschnitt.txt and the beiwerte.aus files.
   * @return The polynom input dir (01Eingang), if preparation was succesful, else <code>null</code>.
   */
  private File preparePolynomes( )
  {
    /* The files needed from the 1D-calculation */
    final File lsQFile = new File( m_dathDir, QLANG_FILE_NAME );
    final File weirFile = new File( m_dathDir, WEIR_FILE_NAME );
    final File bridgeFile = new File( m_dathDir, BRIDGE_FILE_NAME );

    final File[] dathFiles = new File[] { lsQFile, weirFile, bridgeFile };

    /* Check input data */
    for( final File file : dathFiles )
    {
      if( !file.exists() )
      {
        m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.0" ), file ); //$NON-NLS-1$
        return null;
      }
    }

    /* Copy input data to exe dir */
    try
    {
      final File eingangDir = new File( m_tmpDir, "01Eingang" ); //$NON-NLS-1$

      for( final File file : dathFiles )
        FileUtils.copyFileToDirectory( file, eingangDir );

      return eingangDir;
    }
    catch( final IOException e )
    {
      m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.2" ) ); //$NON-NLS-1$
      return null;
    }
  }

  private void prepareSteuerpoly( ) throws SimulationException
  {
    final File steuerFile = new File( m_tmpDir, "steuerpoly.ini" ); //$NON-NLS-1$

    try
    {
      final double startStation = m_calculation.getStartStation().doubleValue();
      final double endStation = m_calculation.getStartStation().doubleValue();

      /* Polynomial Parameters */
      final PolynomeProperties pps = m_calculation.getPolynomeProperties();
      final int polynomialDeegree = pps.getDeegree();
      final boolean ignoreOutlier = pps.getIgnoreOutlier();
      final boolean isTripleForAll = pps.getTripleForAll();
      final BigDecimal alphaLimit = pps.getAlphaLimit();
      final TripleMode tripleMode = pps.getTripleMode();
      final boolean autoSlopeDetection = tripleMode == TripleMode.slopeChange;
      final BigDecimal runoffSlope = pps.getRunoffSlope();
      final BigDecimal areaSlope = pps.getAreaSlope();
      final BigDecimal alphaSlope = pps.getAlphaSlope();
      final BigDecimal weightSplinePoint = pps.getWeightSplinePoint();

      // Programming Language C (PRC) locale in order to format with decimal '.'
      final Formatter formatter = new Formatter( steuerFile, Charset.defaultCharset().name(), Locale.PRC );

      formatter.format( "Steuerdatei fuer die Polynomfunktionen je Profil%n" ); //$NON-NLS-1$
      formatter.format( "-------------------------------------------------%n" ); //$NON-NLS-1$
      formatter.format( "02 L‰ngsschnitt(Pfad) 01Eingang\\%s%n", QLANG_FILE_NAME ); //$NON-NLS-1$
      formatter.format( "03 PolyGrad(2,3,4) %d%n", polynomialDeegree ); //$NON-NLS-1$
      formatter.format( "04 DreiTeil(J/N) %1s%n", isTripleForAll ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "05 PolyReduce(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "06 ProfIntervall(J/N) N%n" ); //$NON-NLS-1$
      formatter.format( "07 StartProf(0000.0000) %.4f%n", startStation ); //$NON-NLS-1$
      formatter.format( "08 EndProf(0000.0000) %.4f%n", endStation ); //$NON-NLS-1$
      formatter.format( "09 AusgabeJeFunktion(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "10 AusgabeWspWerte(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "11 AusgabeKontrolle(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "12 AusgabeFile(Pfad) 02Ausgang\\%n" ); //$NON-NLS-1$
      formatter.format( "13 Ausreisser(J/N) %1s%n", ignoreOutlier ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "14 Impulsstrom(00.0000) %.4f%n", alphaLimit ); //$NON-NLS-1$
      formatter.format( "15 AutoSteigung(J/N) %1s%n", autoSlopeDetection ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "16 Q_Steigung(00.0000) %.4f%n", runoffSlope ); //$NON-NLS-1$
      formatter.format( "17 A_Steigung(00.0000) %.4f%n", areaSlope ); //$NON-NLS-1$
      formatter.format( "18 Alp_Steigung(00.0000) %.4f%n", alphaSlope ); //$NON-NLS-1$
      formatter.format( "19 WichtungSplinePkt(0000.00) %.2f%n", weightSplinePoint ); //$NON-NLS-1$

      formatter.close();

      FormatterUtils.checkIoException( formatter );
    }
    catch( final IOException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.16" ), e ); //$NON-NLS-1$
    }
    finally
    {
    }
  }

  // FIXME does not belong here
  public static <S> S forStationAdjacent( final SortedMap<BigDecimal, S> stationIndex, final BigDecimal station, final boolean upstream )
  {
    final BigDecimal pred = NumberUtils.decrement( station );
    final BigDecimal succ = NumberUtils.increment( station );

    if( upstream )
    {
      final SortedMap<BigDecimal, ? extends Object> successors = stationIndex.tailMap( succ );
      if( !successors.isEmpty() )
        return stationIndex.get( successors.firstKey() );
    }
    else
    {

      final SortedMap<BigDecimal, ? extends Object> predecessors = stationIndex.headMap( pred );
      if( !predecessors.isEmpty() )
        return stationIndex.get( predecessors.lastKey() );
    }

    return null;
  }
}
