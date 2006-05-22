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
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Arrays;
import java.util.Iterator;

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
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypso.ui.model.wspm.abstraction.ReachSegmentStationComparator;
import org.kalypso.ui.model.wspm.abstraction.TuhhCalculation;
import org.kalypso.ui.model.wspm.abstraction.TuhhReach;
import org.kalypso.ui.model.wspm.abstraction.TuhhReachProfileSegment;
import org.kalypso.ui.model.wspm.abstraction.TuhhWspmProject;
import org.kalypso.ui.model.wspm.abstraction.WspmProfileReference;
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

            // reachTuhh

            //

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

  // TODO Unterscheide wspwin bzw. rechenkern via flag: schreibt Berechnungsparameter unterschiedlich
  /**
   * Schreibt eine Berechnung für den 1D Tuhh-Rechenkern in das angegebene Verzeichnis
   */
  private static void writeForTuhhKernel( final TuhhCalculation calculation, final File dir ) throws IOException
  {
    dir.mkdirs();

    final File profDir = WspWinImporter.getProfDir( dir );
    final File batFile = new File( profDir, "bat.001" );
    final File zustFile = new File( profDir, "zustand.001" );
    final File qwtFile = new File( profDir, "qwert.001" );

    // write1DTuhhSteuerparameter( calculation, batFile );
    write1DTuhhZustand( calculation.getReach(), zustFile );
    // write1DTuhhZustand( calculation.getRunOffEvent(), qwtFile );
  }

  private static void write1DTuhhZustand( final TuhhReach reach, final File zustFile ) throws IOException
  {
    final boolean isDirectionUpstreams = reach.getWaterBody().isDirectionUpstreams();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();

    Arrays.sort( segments, new ReachSegmentStationComparator( isDirectionUpstreams ) );

    PrintWriter writer = null;
    try
    {
      zustFile.getParentFile().mkdirs();
      writer = new PrintWriter( new BufferedWriter( new FileWriter( zustFile ) ) );

      for( final TuhhReachProfileSegment segment : segments )
      {
        final BigDecimal station = segment.getStation();
        final WspmProfileReference profileMember = segment.getProfileMember();
        final String href = profileMember.getHref();

        writer.print( href );
        writer.print( " " );
        writer.println( station.doubleValue() );
      }

      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

  }
}
