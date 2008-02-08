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
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
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
   * Schreibt eine Berechnung f�r den 1D Tuhh-Rechenkern in das angegebene Verzeichnis
   * 
   * @param context
   *            Context to resolve links inside the gml structure.
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
      throw new IllegalArgumentException( "Gew�sserstrang nicht festgelegt." );

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

    IComponent stationComp = null;
    IComponent abflussComp = null;
    final IComponent[] components = result.getComponents();
    for( final IComponent comp : components )
    {
      // TODO: get component via phenomenon
      if( comp.getName().startsWith( "Abfluss" ) )
        abflussComp = comp;
      if( comp.getName().startsWith( "Station" ) )
        stationComp = comp;
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

    PrintWriter pw = null;
    try
    {
      batFile.getParentFile().mkdirs();
      pw = new PrintWriter( new BufferedWriter( new FileWriter( batFile ) ) );

      pw.println( "# " + calculation.getName() );
      pw.println( "# " + SimpleDateFormat.getDateTimeInstance( SimpleDateFormat.SHORT, SimpleDateFormat.SHORT ).format( new Date() ) );

      pw.println();
      pw.println( "PROJEKTPFAD=" + zustFile.getParentFile().getParent() );
      pw.println( "STRANGDATEI=" + zustFile.getName() );

      // TODO: passt das zum RK?
      pw.println();
      pw.println( "# m�gliche Werte:" );
      pw.println( "# WATERLEVEL" );
      pw.println( "# BF_UNIFORM" );
      pw.println( "# BF_NON_UNIFORM" );
      pw.println( "# REIB_KONST" );
      pw.println( "BERECHNUNGSMODUS=" + calcMode.name() );

      // TODO: passt das zum RK?
      pw.println();
      pw.println( "# m�gliche Werte:" );
      pw.println( "# DARCY_WEISBACH_OHNE_FORMEINFLUSS" );
      pw.println( "# DARCY_WEISBACH_MIT_FORMEINFLUSS" );
      pw.println( "# MANNING_STRICKLER" );
      pw.println( "FLIESSGESETZ=" + calculation.getFliessgesetz().name() );

      pw.println();
      pw.println( "ANFANGSSTATION=" + Double.toString( calculation.getStartStation().doubleValue() ) );
      pw.println( "ENDSTATION=" + Double.toString( calculation.getEndStation().doubleValue() ) );

      pw.println();
      pw.println( "# m�gliche Werte" );
      pw.println( "# CRITICAL_WATER_DEPTH" );
      pw.println( "# UNIFORM_BOTTOM_SLOPE" );
      pw.println( "# WATERLEVEL" );
      pw.println( "ART_RANDBEDINGUNG=" + calculation.getStartKind().name() );
      final Double startWaterlevel = calculation.getStartWaterlevel();
      if( startWaterlevel != null )
        pw.println( "ANFANGSWASSERSPIEGEL=" + Double.toString( startWaterlevel ) );
      final Double startSlope = calculation.getStartSlope();
      if( startSlope != null )
        pw.println( "GEFAELLE=" + Double.toString( startSlope ) );

      pw.println();
      pw.println( "# m�gliche Werte" );
      pw.println( "# NON" );
      pw.println( "# DVWK" );
      pw.println( "# BJOERNSEN" );
      pw.println( "# DFG" );
      pw.println( "VERZOEGERUNGSVERLUST=" + calculation.getVerzoegerungsverlust().name() );

      pw.println();
      pw.println( "# m�gliche Werte" );
      pw.println( "# SIMPLE" );
      pw.println( "# EXACT" );
      pw.println( "ITERATIONSART=" + calculation.getIterationType().name() );

      pw.println();
      pw.println( "# m�gliche Werte" );
      pw.println( "# TRAPEZ_FORMULA" );
      pw.println( "# GEOMETRIC_FORMULA" );
      pw.println( "REIBUNGSVERLUST=" + calculation.getReibungsverlust().name() );

      pw.println();
      pw.println( "# m�gliche Werte: true / false" );
      pw.println( "MIT_BRUECKEN=" + calculation.isCalcBridges().toString() );
      pw.println( "MIT_WEHREN=" + calculation.isCalcBarrages().toString() );

      pw.println();
      pw.println( "ABFLUSSEREIGNIS=" + qwtFile.getName() );

      pw.println();
      pw.println( "EINZELVERLUSTE=" + "TODO" );

      pw.println();
      final Double minQ = calculation.getMinQ();
      if( minQ != null )
        pw.println( "MIN_Q=" + Double.toString( minQ ) );
      final Double maxQ = calculation.getMaxQ();
      if( maxQ != null )
        pw.println( "MAX_Q=" + Double.toString( maxQ ) );
      final Double qstep = calculation.getQStep();
      if( qstep != null )
        pw.println( "DELTA_Q=" + Double.toString( qstep ) );

      pw.println();
      // Einheit des Durchflusses wird standardm��ig festgelegt
      pw.println( "# m�gliche Werte" );
      pw.println( "# QM_S" );
      pw.println( "# L_S" );
      pw.println( "DURCHFLUSS_EINHEIT=QM_S" );

      pw.close();
    }
    finally
    {
      IOUtils.closeQuietly( pw );
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

        final WspmProfile profileMember = segment.getProfileMember();

        final String prfName = "Profil_" + fileCount++ + ".prf";

        zustWriter.print( prfName );
        zustWriter.print( " " );
        // TODO mindestens 4, besser 5 Nachkommastellen?
        zustWriter.println( station );

        final IProfil profil = ProfileFeatureFactory.toProfile( profileMember.getFeature() );
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
