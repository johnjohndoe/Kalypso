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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Date;
import java.util.Formatter;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.wspwin.calc.TuhhCalcEnergylossWriter;
import org.kalypso.model.wspm.tuhh.core.wspwin.calc.TuhhCalcZustandWriter;
import org.kalypso.wspwin.core.WspWinProject;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author thuel2
 */
public final class WspWinExporter
{
  private WspWinExporter( )
  {
  }

  /**
   * Schreibt eine Berechnung für den 1D Tuhh-Rechenkern in das angegebene Verzeichnis
   *
   * @param context
   *          Context to resolve links inside the gml structure.
   */
  public static void writeForTuhhKernel( final TuhhCalculation calculation, final File dir ) throws IOException, CoreException
  {
    dir.mkdirs();
    final WspWinProject wspWinProject = new WspWinProject( dir );

    final File profDir = wspWinProject.getProfDir();
    final File batFile = new File( dir, "kalypso-1D.ini" ); //$NON-NLS-1$
    final File zustFile = new File( profDir, "zustand.001" ); //$NON-NLS-1$
    final File qwtFile = new File( profDir, "qwert.001" ); //$NON-NLS-1$
    final File psiFile = new File( profDir, "zustand.psi" ); //$NON-NLS-1$
    profDir.mkdirs();
    // TODO: what is the purpose of the psi file? energy loss? -> implement it
    // final File psiFile = new File( profDir, "zustand.psi" ); //$NON-NLS-1$

    final TuhhReach reach = calculation.getReach();
    if( reach == null )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.11" ) ); //$NON-NLS-1$

    final WspmWaterBody reachWater = reach.getWaterBody();

    final TuhhReachProfileSegment[] profileSegments = reach.getReachProfileSegments();
    if( profileSegments.length == 0 )
      throw new IllegalArgumentException( Messages.getString( "WspWinExporter.1" ) ); //$NON-NLS-1$

    final TuhhStationRange stationRange = new TuhhStationRange( calculation );

    write1DTuhhSteuerparameter( calculation, batFile, zustFile, qwtFile,psiFile, stationRange );

    write1DTuhhEnergyloss( calculation, stationRange, psiFile );

    write1DTuhhZustand( calculation, stationRange, zustFile );
    if( calculation.getCalcMode() == MODE.WATERLEVEL )
    {
      final IRunOffEvent runOffEvent = calculation.getRunOffEvent();
      if( runOffEvent == null )
        throw new IllegalArgumentException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.12" ) ); //$NON-NLS-1$

      final WspmWaterBody runoffWater = runOffEvent.getOwner();
      if( runoffWater != reachWater )
      {
        final String error = String.format( Messages.getString( "WspWinExporter.2" ), runOffEvent.getName(), runoffWater.getName(), reach.getName(), reachWater.getName() ); //$NON-NLS-1$
        throw new IllegalArgumentException( error );
      }

      write1DTuhhRunOff( runOffEvent, qwtFile, stationRange );
    }
  }

  private static void write1DTuhhRunOff( final IRunOffEvent runOffEvent, final File qwtFile, final TuhhStationRange stationRange ) throws IOException, CoreException
  {
    final SortedMap<BigDecimal, BigDecimal> values = runOffEvent.getDischargeTable();

    if( values.isEmpty() )
    {
      final String message = String.format( Messages.getString( "WspWinExporter.3" ), FeatureHelper.getAnnotationValue( runOffEvent, IAnnotation.ANNO_LABEL ) ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    PrintWriter pw = null;
    try
    {
      qwtFile.getParentFile().mkdirs();

      pw = new PrintWriter( new BufferedWriter( new FileWriter( qwtFile ) ) );

      final int exportSign = stationRange.getExportSign();
      final String runoffName = runOffEvent.getName();
      final String cleanRunoffName = cleanupRunoffName( runoffName );
      pw.print( cleanRunoffName );
      pw.print( " " ); //$NON-NLS-1$
      pw.println( values.size() );

      // write it sorted into the file
      for( final Map.Entry<BigDecimal, BigDecimal> entry : values.entrySet() )
      {
        final BigDecimal station = entry.getKey();
        final BigDecimal runOff = entry.getValue();

        pw.print( station.doubleValue() * exportSign );
        pw.print( " " ); //$NON-NLS-1$
        pw.print( runOff.doubleValue() );
        pw.println();
      }
    }
    finally
    {
      IOUtils.closeQuietly( pw );
    }
  }

  private static String cleanupRunoffName( final String runoffName )
  {
    return runoffName.replaceAll( "( |,|\\.)", "_" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private static void write1DTuhhSteuerparameter( final TuhhCalculation calculation, final File batFile, final File zustFile, final File qwtFile, final File psiFile,final TuhhStationRange stationRange ) throws IOException
  {
    final MODE calcMode = calculation.getCalcMode();

    Formatter pw = null;
    try
    {
      batFile.getParentFile().mkdirs();

      pw = new Formatter( batFile );

      pw.format( "# %s%n", calculation.getName() ); //$NON-NLS-1$
      pw.format( "# %s%n", DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.SHORT ).format( new Date() ) ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "PROJEKTPFAD=%s%n", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      pw.format( "STRANGDATEI=%s%n", zustFile.getName() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte:%n" ); //$NON-NLS-1$
      pw.format( "# WATERLEVEL%n" ); //$NON-NLS-1$
      pw.format( "# BF_UNIFORM%n" ); //$NON-NLS-1$
      pw.format( "# BF_NON_UNIFORM%n" ); //$NON-NLS-1$
      pw.format( "# REIB_KONST%n" ); //$NON-NLS-1$

      pw.format( "BERECHNUNGSMODUS=%s%n", calcMode.name() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte:%n" ); //$NON-NLS-1$
      pw.format( "# DARCY_WEISBACH_OHNE_FORMEINFLUSS%n" ); //$NON-NLS-1$
      pw.format( "# DARCY_WEISBACH_MIT_FORMEINFLUSS%n" ); //$NON-NLS-1$
      pw.format( "# MANNING_STRICKLER%n" ); //$NON-NLS-1$
      pw.format( "FLIESSGESETZ=%s%n", calculation.getFliessgesetz().name() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$

      pw.format( Locale.US, "ANFANGSSTATION=%s%n", stationRange.getExportFrom() ); //$NON-NLS-1$
      pw.format( Locale.US, "ENDSTATION=%s%n", stationRange.getExportTo() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte%n" ); //$NON-NLS-1$
      pw.format( "# CRITICAL_WATER_DEPTH%n" ); //$NON-NLS-1$
      pw.format( "# UNIFORM_BOTTOM_SLOPE%n" ); //$NON-NLS-1$
      pw.format( "# WATERLEVEL%n" ); //$NON-NLS-1$
      pw.format( "ART_RANDBEDINGUNG=%s%n", calculation.getStartKind().name() ); //$NON-NLS-1$
      final Double startWaterlevel = calculation.getStartWaterlevel();
      if( startWaterlevel != null )
      {
        pw.format( Locale.US, "ANFANGSWASSERSPIEGEL=%s%n", startWaterlevel ); //$NON-NLS-1$
      }
      final BigDecimal startSlope = calculation.getStartSlope();
      if( startSlope != null )
      {
        pw.format( "GEFAELLE=%s%n", startSlope ); //$NON-NLS-1$
      }

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte%n" ); //$NON-NLS-1$
      pw.format( "# NON%n" ); //$NON-NLS-1$
      pw.format( "# DVWK%n" ); //$NON-NLS-1$
      pw.format( "# BJOERNSEN%n" ); //$NON-NLS-1$
      pw.format( "# DFG%n" ); //$NON-NLS-1$
      pw.format( "VERZOEGERUNGSVERLUST=%s%n", calculation.getVerzoegerungsverlust().name() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte%n" ); //$NON-NLS-1$
      pw.format( "# SIMPLE%n" ); //$NON-NLS-1$
      pw.format( "# EXACT%n" ); //$NON-NLS-1$
      pw.format( "ITERATIONSART=%s%n", calculation.getIterationType().name() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte%n" ); //$NON-NLS-1$
      pw.format( "# TRAPEZ_FORMULA%n" ); //$NON-NLS-1$
      pw.format( "# GEOMETRIC_FORMULA%n" ); //$NON-NLS-1$
      pw.format( "REIBUNGSVERLUST=%s%n", calculation.getReibungsverlust().name() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "# mögliche Werte: true / false%n" ); //$NON-NLS-1$
      pw.format( "MIT_BRUECKEN=%b%n", calculation.isCalcBridges() ); //$NON-NLS-1$
      pw.format( "MIT_WEHREN=%b%n", calculation.isCalcBarrages() ); //$NON-NLS-1$
      pw.format( "USE_EXTREM_ROUGH=%b%n", calculation.isUseExtremeRoughness() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "ABFLUSSEREIGNIS=%s%n", qwtFile.getName() ); //$NON-NLS-1$

      pw.format( "%n" ); //$NON-NLS-1$
      pw.format( "EINZELVERLUSTE=%s%n", psiFile.getName() ); //$NON-NLS-1$ //$NON-NLS-2$

      pw.format( "%n" ); //$NON-NLS-1$
      final Double minQ = calculation.getMinQ();
      if( minQ != null )
      {
        pw.format( Locale.US, "MIN_Q=%s%n", minQ ); //$NON-NLS-1$
      }
      final Double maxQ = calculation.getMaxQ();
      if( maxQ != null )
      {
        pw.format( Locale.US, "MAX_Q=%s%n", maxQ ); //$NON-NLS-1$
      }
      final Double qstep = calculation.getQStep();
      if( qstep != null )
      {
        pw.format( Locale.US, "DELTA_Q=%s%n", qstep ); //$NON-NLS-1$
      }

      pw.format( "%n" ); //$NON-NLS-1$
      // Einheit des Durchflusses wird standardmäßig festgelegt
      pw.format( "# mögliche Werte%n" ); //$NON-NLS-1$
      pw.format( "# QM_S%n" ); //$NON-NLS-1$
      pw.format( "# L_S%n" ); //$NON-NLS-1$
      pw.format( "DURCHFLUSS_EINHEIT=QM_S%n" ); //$NON-NLS-1$

      FormatterUtils.checkIoException( pw );
    }
    finally
    {
      if( pw != null )
      {
        pw.close();
      }
    }

  }

  private static void write1DTuhhEnergyloss( final TuhhCalculation calculation, final TuhhStationRange stationRange, final File psiFile ) throws IOException
  {
    final TuhhCalcEnergylossWriter energylossWriter = new TuhhCalcEnergylossWriter( calculation.getReach(),stationRange );
    energylossWriter.write( psiFile );
  }

  private static void write1DTuhhZustand( final TuhhCalculation calculation, final TuhhStationRange stationRange, final File zustFile ) throws IOException
  {
    final String roughnessType = getRoughnessForFG( calculation.getFliessgesetz() );

    final boolean preferingRoughnessClasses = calculation.isPreferingRoughnessClasses();
    final boolean preferingVegetationClasses = calculation.isPreferingVegetationClasses();

    final TuhhCalcZustandWriter zustandWriter = new TuhhCalcZustandWriter( calculation.getReach(), stationRange, roughnessType, preferingRoughnessClasses, preferingVegetationClasses );

    zustandWriter.write( zustFile );
  }

  private static String getRoughnessForFG( final FLIESSGESETZ fg )
  {
    if( FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS.equals( fg ) )
      return IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS;
    else if( FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS.equals( fg ) )
      return IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS;
    else if( FLIESSGESETZ.MANNING_STRICKLER.equals( fg ) )
      return IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST;
    else
      return ""; //$NON-NLS-1$
  }
}