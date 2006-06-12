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
import org.kalypso.model.wspm.core.gml.ReachSegmentStationComparator;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReachProfileSegment;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.WspWinHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import serializer.prf.ProfilesSerializer;

/**
 * @author thuel2
 */
public class WspWinExporter
{

  public WspWinExporter( )
  {
    // will not be instantiated
  }

  public static IStatus exportWspmProject( final Iterator modelGml, final File wspwinDir, final SubProgressMonitor monitor )
  {
    monitor.beginTask( "WspWin Projekt exportieren", 1000 );

    monitor.subTask( " - Initialisiere KALYPSO..." );

    // modelGml holds all selected resources (very general)
    while( modelGml.hasNext() )
    {

      try
      {
        final IResource resource = (IResource) modelGml.next();
        if( resource instanceof IFile )
        {
          // TODO: ensure that resource is a GML file?
          // read gml workspace
          final URL modelGmlUrl = ResourceUtilities.createURL( resource );

          final GMLWorkspace gmlWrkSpce = GmlSerializer.createGMLWorkspace( modelGmlUrl );
          Feature rootFeat = gmlWrkSpce.getRootFeature();

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
      catch( final Exception e1 )
      {
        e1.printStackTrace();
        return StatusUtilities.statusFromThrowable( e1 );
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
  public static void writeForTuhhKernel( final TuhhCalculation calculation, final File dir ) throws IOException, ProfilDataException
  {
    dir.mkdirs();

    final File profDir = WspWinHelper.getProfDir( dir );
    final File batFile = new File( dir, "kalypso-1D.ini" );
    final File zustFile = new File( profDir, "zustand.001" );
    final File qwtFile = new File( profDir, "qwert.001" );
    final File psiFile = new File( profDir, "zustand.psi" );

    final boolean isDirectionUpstreams = calculation.getReach().getWaterBody().isDirectionUpstreams();

    write1DTuhhSteuerparameter( calculation, batFile, zustFile, qwtFile );
    write1DTuhhZustand( calculation.getReach(), isDirectionUpstreams, zustFile, psiFile );
    write1DTuhhRunOff( calculation.getRunOffEvent(), isDirectionUpstreams, qwtFile );
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
      if( comp.getName().equals( "Abfluss" ) )
        abflussComp = comp;
      if( comp.getName().equals( "Station" ) )
        stationComp = comp;
    }

    final Comparator<Double> comp = new Comparator<Double>()
    {
      public int compare( final Double o1, final Double o2 )
      {
        if( isDirectionUpstreams )
          return o1.compareTo( o2 );
        else
          return o2.compareTo( o1 );
      }
    };

    final Map<Double, Double> values = new TreeMap<Double, Double>( comp );
    for( final IRecord record : result )
    {
      final Double station = (Double) record.getValue( stationComp );
      final Double runOff = (Double) record.getValue( abflussComp );
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
      for( final Map.Entry<Double, Double> entry : values.entrySet() )
      {
        final Double station = entry.getKey();
        final Double runOff = entry.getValue();

        pw.print( Double.toString( station ) );
        pw.print( " " );
        pw.print( Double.toString( runOff ) );
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
      pw.println( "# mögliche Werte:" );
      pw.println( "# WATERLEVEL" );
      pw.println( "# BF_UNIFORM" );
      pw.println( "# BF_NON_UNIFORM" );
      pw.println( "BERECHNUNGSMODUS=" + calculation.getCalcMode().name() );

      // TODO: passt das zum RK?
      pw.println();
      pw.println( "# mögliche Werte:" );
      pw.println( "# DARCY_WEISBACH_OHNE_FORMEINFLUSS" );
      pw.println( "# DARCY_WEISBACH_MIT_FORMEINFLUSS" );
      pw.println( "# MANNING_STRICKLER" );
      pw.println( "FLIESSGESETZ=" + calculation.getFliessgesetz().name() );

      pw.println();
      pw.println( "ANFANGSSTATION=" + Double.toString( calculation.getStartStation().doubleValue() ) );
      pw.println( "ENDSTATION=" + Double.toString( calculation.getEndStation().doubleValue() ) );

      pw.println();
      pw.println( "# mögliche Werte" );
      pw.println( "# CRITICAL_WATER_DEPTH" );
      pw.println( "# UNIFORM_BOTTOM_SLOPE" );
      pw.println( "# WATERLEVEL" );
      pw.println( "ART_RANDBEDINGUNG=" + calculation.getStartKind().name() );
      pw.println( "ANFANGSWASSERSPIEGEL=" + Double.toString( calculation.getStartWaterlevel() ) );
      pw.println( "GEFAELLE=" + Double.toString( calculation.getStartSlope() ) );

      pw.println();
      pw.println( "# mögliche Werte" );
      pw.println( "# DVWK" );
      pw.println( "# BJOERNSEN" );
      pw.println( "# DFG" );
      pw.println( "VERZOEGERUNGSVERLUST=" + calculation.getVerzoegerungsverlust().name() );

      pw.println();
      pw.println( "# mögliche Werte" );
      pw.println( "# SIMPLE" );
      pw.println( "# EXACT" );
      pw.println( "ITERATIONSART=" + calculation.getIterationType().name() );

      pw.println();
      pw.println( "# mögliche Werte" );
      pw.println( "# TRAPEZ_FORMULA" );
      pw.println( "# GEOMETRIC_FORMULA" );
      pw.println( "REIBUNGSVERLUST=" + calculation.getReibungsverlust().name() );

      pw.println();
      pw.println( "# mögliche Werte: true / false" );
      pw.println( "MIT_BRUECKEN=" + calculation.isCalcBridges().toString() );
      pw.println( "MIT_WEHREN=" + calculation.isCalcBarrages().toString() );

      pw.println();
      pw.println( "ABFLUSSEREIGNIS=" + qwtFile.getName() );

      pw.println();
      pw.println( "EINZELVERLUSTE=" + "TODO" );

      pw.println();
      pw.println( "MIN_Q=" + Double.toString( calculation.getMinQ() ) );
      pw.println( "MAX_Q=" + Double.toString( calculation.getMaxQ() ) );
      pw.println( "DELTA_Q=" + Double.toString( calculation.getQStep() ) );

      pw.println();
      // Einheit des Durchflusses wird standardmäßig festgelegt
      pw.println( "# mögliche Werte" );
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

  private static void write1DTuhhZustand( final TuhhReach reach, final boolean isDirectionUpstreams, final File zustFile, final File psiFile ) throws IOException, ProfilDataException
  {
    final WspmReachProfileSegment[] segments = reach.getReachProfileSegments();

    Arrays.sort( segments, new ReachSegmentStationComparator( isDirectionUpstreams ) );

    PrintWriter zustWriter = null;
    PrintWriter psiWriter = null;
    try
    {
      zustFile.getParentFile().mkdirs();
      zustWriter = new PrintWriter( new BufferedWriter( new FileWriter( zustFile ) ) );

      psiFile.getParentFile().mkdirs();
      psiWriter = new PrintWriter( new BufferedWriter( new FileWriter( psiFile ) ) );

      int fileCount = 0;
      for( final WspmReachProfileSegment segment : segments )
      {
        final BigDecimal station = segment.getStation();
        final WspmProfile profileMember = segment.getProfileMember();

        // final String href = profileMember.getHref();

        final String prfName = "Profil_" + fileCount++ + ".prf";

        zustWriter.print( prfName );
        zustWriter.print( " " );
        // TODO mindestens 4, besser 5 Nachkommastellen?
        zustWriter.println( station.doubleValue() );

        final IProfil profil = ProfileFeatureFactory.toProfile( profileMember.getFeature() );
        profil.setStation( station.doubleValue() );

        final File outPrfFile = new File( zustFile.getParentFile(), prfName );
        ProfilesSerializer.store( profil, outPrfFile );
      }

      zustWriter.close();
      psiWriter.close();
    }
    finally
    {
      IOUtils.closeQuietly( zustWriter );
      IOUtils.closeQuietly( psiWriter );
    }
  }
}
