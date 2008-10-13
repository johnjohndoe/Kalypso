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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.Formatter;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.WspWinHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
public class WspWinExporter
{

  public WspWinExporter( )
  {
    // will not be instantiated
  }

  public static IStatus exportWspmProject( final Iterator<IResource> modelGml, final File wspwinDir, final SubProgressMonitor monitor )
  {
    monitor.beginTask( "WspWin Projekt exportieren", 1000 );

    monitor.subTask( " - Initialisiere KALYPSO..." );

    // modelGml holds all selected resources (very general)
    while( modelGml.hasNext() )
    {

      try
      {
        final IResource resource = modelGml.next();
        if( resource instanceof IFile )
        {
          // TODO: ensure that resource is a GML file?
          // read gml workspace
          final URL modelGmlUrl = ResourceUtilities.createURL( resource );

          final GMLWorkspace gmlWrkSpce = GmlSerializer.createGMLWorkspace( modelGmlUrl, null );
          final Feature rootFeat = gmlWrkSpce.getRootFeature();

          // featType holen
          final IFeatureType featureType = rootFeat.getFeatureType();
          final QName featureName = featureType.getQName();

          // process only WspmProject features
          if( QNameUtilities.equals( featureName, IWspmConstants.NS_WSPMPROJ, "WspmProject" ) )
          {
            // TODO: sicherstellen, dass es sich um ein TU-HH-Modell handelt?

            // load (initialize) WspmProject
            monitor.subTask( " - Modell " + resource.getName() + " wird geladen..." );
            final Feature modelRootFeature = gmlWrkSpce.getRootFeature();
            final TuhhWspmProject wspmProject = new TuhhWspmProject( modelRootFeature );

            // create unique wspwinProjectDir
            File wspwinProjDir = new File( wspwinDir, wspmProject.getName() );
            int ii = 0;

            while( wspwinProjDir.exists() )
            {
              ii++;
              wspwinProjDir = new File( wspwinDir, wspmProject.getName() + "_" + ii );
            }
            wspwinProjDir.mkdirs();

            // write data into wspwinDir projectDir
            monitor.subTask( " - Daten werden konvertiert..." );

            // CalculationTuhh
            final TuhhCalculation[] tuhhCalcs = wspmProject.getCalculations();
            for( final TuhhCalculation calculation : tuhhCalcs )
            {
              final File dir = new File( wspwinProjDir, calculation.getFeature().getId() );
              writeForTuhhKernel( calculation, dir );
            }
          }
        }
        else
          // we don't process folders
          continue;
      }
      catch( final Throwable t )
      {
        t.printStackTrace();
        return StatusUtilities.statusFromThrowable( t );
      }
      finally
      {
        // clean up
        monitor.done();
      }
    }
    return Status.OK_STATUS;
  }

  /**
   * Schreibt eine Berechnung für den 1D Tuhh-Rechenkern in das angegebene Verzeichnis
   * 
   * @param context
   *          Context to resolve links inside the gml structure.
   */
  public static void writeForTuhhKernel( final TuhhCalculation calculation, final File dir ) throws IOException
  {
    dir.mkdirs();

    final File profDir = WspWinHelper.getProfDir( dir );
    final File batFile = new File( dir, "kalypso-1D.ini" );
    final File zustFile = new File( profDir, "zustand.001" );
    final File qwtFile = new File( profDir, "qwert.001" );
    final File psiFile = new File( profDir, "zustand.psi" );

    final TuhhReach reach = calculation.getReach();
    if( reach == null )
      throw new IllegalArgumentException( "Gewässerstrang nicht festgelegt." );

    final boolean isDirectionUpstreams = reach.getWaterBody().isDirectionUpstreams();

    write1DTuhhSteuerparameter( calculation, batFile, zustFile, qwtFile );
    write1DTuhhZustand( calculation, isDirectionUpstreams, zustFile, psiFile );
    if( calculation.getCalcMode() == MODE.WATERLEVEL )
    {
      final IObservation<TupleResult> runOffEvent = calculation.getRunOffEvent();
      if( runOffEvent == null )
      {
        throw new IllegalArgumentException( "Kein Abflussereignis angegeben. Abbruch der Berechnung." );
      }
      else
        write1DTuhhRunOff( runOffEvent, isDirectionUpstreams, qwtFile );
    }
  }

  private static void write1DTuhhRunOff( final IObservation<TupleResult> runOffEvent, final boolean isDirectionUpstreams, final File qwtFile ) throws IOException
  {
    final TupleResult result = runOffEvent.getResult();

    int stationComp = -1;
    int abflussComp = -1;
    final IComponent[] components = result.getComponents();
    for( int i = 0; i < components.length; i++ )
    {
      final IComponent comp = components[i];
      // TODO: get component via phenomenon
      if( comp.getName().startsWith( "Abfluss" ) )
        abflussComp = i;
      if( comp.getName().startsWith( "Station" ) )
        stationComp = i;
    }

    final Comparator<BigDecimal> comp = new Comparator<BigDecimal>()
    {
      public int compare( final BigDecimal o1, final BigDecimal o2 )
      {
        if( isDirectionUpstreams )
          return o1.compareTo( o2 );
        else
          return o2.compareTo( o1 );
      }
    };

    final Map<BigDecimal, BigDecimal> values = new TreeMap<BigDecimal, BigDecimal>( comp );
    for( final IRecord record : result )
    {
      final BigDecimal station = (BigDecimal) record.getValue( stationComp );
      final BigDecimal runOff = (BigDecimal) record.getValue( abflussComp );
      values.put( station, runOff );
    }

    PrintWriter pw = null;
    try
    {
      qwtFile.getParentFile().mkdirs();

      pw = new PrintWriter( new BufferedWriter( new FileWriter( qwtFile ) ) );

      pw.print( runOffEvent.getName().replaceAll( " ", "_" ) );
      pw.print( " " );
      pw.println( result.size() );

      // write it sorted into the flle
      for( final Map.Entry<BigDecimal, BigDecimal> entry : values.entrySet() )
      {
        final BigDecimal station = entry.getKey();
        final BigDecimal runOff = entry.getValue();

        pw.print( Double.toString( station.doubleValue() ) );
        pw.print( " " );
        pw.print( Double.toString( runOff.doubleValue() ) );
        pw.println();
      }
    }
    finally
    {
      IOUtils.closeQuietly( pw );
    }
  }

  private static void write1DTuhhSteuerparameter( final TuhhCalculation calculation, final File batFile, final File zustFile, final File qwtFile ) throws IOException
  {
    final MODE calcMode = calculation.getCalcMode();

    Formatter pw = null;
    try
    {
      batFile.getParentFile().mkdirs();

      pw = new Formatter( batFile );

      pw.format( "# %s%n", calculation.getName() );
      pw.format( "# %s%n", SimpleDateFormat.getDateTimeInstance( SimpleDateFormat.SHORT, SimpleDateFormat.SHORT ).format( new Date() ) );

      pw.format( "%n" );
      pw.format( "PROJEKTPFAD=%s%n", zustFile.getParentFile().getParent() );
      pw.format( "STRANGDATEI=%s%n", zustFile.getName() );

      pw.format( "%n" );
      pw.format( "# mögliche Werte:%n" );
      pw.format( "# WATERLEVEL%n" );
      pw.format( "# BF_UNIFORM%n" );
      pw.format( "# BF_NON_UNIFORM%n" );
      pw.format( "# REIB_KONST%n" );

      pw.format( "BERECHNUNGSMODUS=%s%n", calcMode.name() );

      // TODO: passt das zum RK?
      pw.format( "%n" );
      pw.format( "# mögliche Werte:%n" );
      pw.format( "# DARCY_WEISBACH_OHNE_FORMEINFLUSS%n" );
      pw.format( "# DARCY_WEISBACH_MIT_FORMEINFLUSS%n" );
      pw.format( "# MANNING_STRICKLER%n" );
      pw.format( "FLIESSGESETZ=%s%n", calculation.getFliessgesetz().name() );

      pw.format( "%n" );
      pw.format( "ANFANGSSTATION=%s%n", Double.toString( calculation.getStartStation().doubleValue() ) );
      pw.format( "ENDSTATION=%s%n", Double.toString( calculation.getEndStation().doubleValue() ) );

      pw.format( "%n" );
      pw.format( "# mögliche Werte%n" );
      pw.format( "# CRITICAL_WATER_DEPTH%n" );
      pw.format( "# UNIFORM_BOTTOM_SLOPE%n" );
      pw.format( "# WATERLEVEL%n" );
      pw.format( "ART_RANDBEDINGUNG=%s%n", calculation.getStartKind().name() );
      final Double startWaterlevel = calculation.getStartWaterlevel();
      if( startWaterlevel != null )
        pw.format( "ANFANGSWASSERSPIEGEL=%s%n", Double.toString( startWaterlevel ) );
      final BigDecimal startSlope = calculation.getStartSlope();
      if( startSlope != null )
        pw.format( "GEFAELLE=%s%n", startSlope );

      pw.format( "%n" );
      pw.format( "# mögliche Werte%n" );
      pw.format( "# NON%n" );
      pw.format( "# DVWK%n" );
      pw.format( "# BJOERNSEN%n" );
      pw.format( "# DFG%n" );
      pw.format( "VERZOEGERUNGSVERLUST=%s%n", calculation.getVerzoegerungsverlust().name() );

      pw.format( "%n" );
      pw.format( "# mögliche Werte%n" );
      pw.format( "# SIMPLE%n" );
      pw.format( "# EXACT%n" );
      pw.format( "ITERATIONSART=%s%n", calculation.getIterationType().name() );

      pw.format( "%n" );
      pw.format( "# mögliche Werte%n" );
      pw.format( "# TRAPEZ_FORMULA%n" );
      pw.format( "# GEOMETRIC_FORMULA%n" );
      pw.format( "REIBUNGSVERLUST=%s%n", calculation.getReibungsverlust().name() );

      pw.format( "%n" );
      pw.format( "# mögliche Werte: true / false%n" );
      pw.format( "MIT_BRUECKEN=%b%n", calculation.isCalcBridges() );
      pw.format( "MIT_WEHREN=%b%n", calculation.isCalcBarrages() );
      pw.format( "USE_EXTREM_ROUGH=%b%n", calculation.isUseExtremeRoughness() );

      pw.format( "%n" );
      pw.format( "ABFLUSSEREIGNIS=%s%n", qwtFile.getName() );

      pw.format( "%n" );
      pw.format( "EINZELVERLUSTE=%s%n", "TODO" );

      pw.format( "%n" );
      final Double minQ = calculation.getMinQ();
      if( minQ != null )
        pw.format( "MIN_Q=%s%n", Double.toString( minQ ) );
      final Double maxQ = calculation.getMaxQ();
      if( maxQ != null )
        pw.format( "MAX_Q=%s%n", Double.toString( maxQ ) );
      final Double qstep = calculation.getQStep();
      if( qstep != null )
        pw.format( "DELTA_Q=%s%n", Double.toString( qstep ) );

      pw.format( "%n" );
      // Einheit des Durchflusses wird standardmäßig festgelegt
      pw.format( "# mögliche Werte%n" );
      pw.format( "# QM_S%n" );
      pw.format( "# L_S%n" );
      pw.format( "DURCHFLUSS_EINHEIT=QM_S%n" );

      FormatterUtils.checkIoException( pw );
    }
    finally
    {
      if( pw != null )
        pw.close();
    }

  }

  private static void write1DTuhhZustand( final TuhhCalculation calculation, final boolean isDirectionUpstreams, final File zustFile, final File psiFile ) throws IOException
  {
    final TuhhReach reach = calculation.getReach();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();

    final TuhhStationRange stationRange = new TuhhStationRange( calculation, isDirectionUpstreams );
    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( isDirectionUpstreams );

    Arrays.sort( segments, stationComparator );

    PrintWriter zustWriter = null;
    PrintWriter psiWriter = null;
    PrintWriter prfWriter = null;
    try
    {
      zustFile.getParentFile().mkdirs();
      zustWriter = new PrintWriter( new BufferedWriter( new FileWriter( zustFile ) ) );

      psiFile.getParentFile().mkdirs();
      psiWriter = new PrintWriter( new BufferedWriter( new FileWriter( psiFile ) ) );

      int fileCount = 0;
      for( final TuhhReachProfileSegment segment : segments )
      {
        final BigDecimal station = segment.getStation();

        if( stationRange.isOutside( station ) )
          continue;

        final IProfileFeature profileMember = segment.getProfileMember();

        final String prfName = "Profil_" + fileCount++ + ".prf";

        zustWriter.print( prfName );
        zustWriter.print( " " );
        // TODO mindestens 4, besser 5 Nachkommastellen?
        zustWriter.println( station );

        final IProfil profil = ProfileFeatureFactory.toProfile( profileMember );
        profil.setStation( station.doubleValue() );

        final File outPrfFile = new File( zustFile.getParentFile(), prfName );
        prfWriter = new PrintWriter( outPrfFile );
        final IProfilSink ps = new PrfSink();
        ps.write( profil, prfWriter );
      }

      zustWriter.close();
      psiWriter.close();
      prfWriter.close();
    }
    finally
    {
      IOUtils.closeQuietly( zustWriter );
      IOUtils.closeQuietly( psiWriter );
      IOUtils.closeQuietly( prfWriter );
    }
  }
}
