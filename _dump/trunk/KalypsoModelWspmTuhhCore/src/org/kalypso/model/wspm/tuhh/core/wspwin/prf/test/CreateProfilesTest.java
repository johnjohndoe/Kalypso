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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf.test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;

import junit.framework.TestCase;

/**
 * @author kimwerner
 */
public class CreateProfilesTest extends TestCase
{
  final String dir = "c:\\PrfTest\\Prof\\";

  // TODO: This test depends on external data, so it is commented out and always fails
  // Please put the data into the sources so that the test runs everywhere
  // If this test is not intended for automatic testing, please write a main() method instead
  public void testRunTest( ) throws Exception
  {
    fail( "This test depends on external data, so it fails!" );
    //
    // final ArrayList<IProfil> p = new ArrayList<IProfil>();
    //
    // p.add( createMinProf() );
    // p.add( createWspWinProf() );
    // p.add( createBruecke() );
    // p.add( createWehr() );
    // p.add( createBewuchs() );
    // p.add( createGeoCoord() );
    // p.add( createDurchlass( BUILDING_TYP.EI ) );
    // p.add( createDurchlass( BUILDING_TYP.KREIS ) );
    // p.add( createDurchlass( BUILDING_TYP.MAUL ) );
    // p.add( createDurchlass( BUILDING_TYP.TRAPEZ ) );
    // writePrf( p );
    // writeStr( p );

  }

//  public IProfil createWspWinProf( )
//  {
//    final IProfil p = createMinProf();
//    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
//
//    p.addDevider( p.getPoints().get( 0 ), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( p.getPoints().get( 4 ), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( p.getPoints().get( 1 ), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    p.addDevider( p.getPoints().get( 3 ), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//
//    return p;
//  }
//
//  public IProfil createBruecke( ) throws IllegalProfileOperationException
//  {
//    final IProfil p = createMinProf();
//    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
//    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) );
//    final IProfilPoint pf = p.getPoints().getFirst();
//    final IProfilPoint pl = p.getPoints().getLast();
//    final IProfilPoint pm = ProfilUtil.getPointAfter( p, pf );
//
//    p.addDevider( pf, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( pl, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( pm, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    p.addDevider( ProfilUtil.getPointBefore( p, pl ), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    final List<IProfilPoint> oList = ProfilUtil.getInnerPoints( p, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    final List<IProfilPoint> uList = ProfilUtil.getInnerPoints( p, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    for( IProfilPoint pkt : oList )
//    {
//      pkt.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, pf.getValueFor( POINT_PROPERTY.HOEHE ) );
//
//    }
//    for( IProfilPoint pkt : uList )
//    {
//      pkt.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, pm.getValueFor( POINT_PROPERTY.HOEHE ) );
//
//    }
//    return p;
//  }
//
//  public IProfil createDurchlass( String bt ) throws IllegalProfileOperationException
//  {
//    final IProfil p = createWspWinProf();
//    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
//    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( bt ) );
//    return p;
//  }
//
//  public IProfil createBewuchs( )
//  {
//    final IProfil p = createWspWinProf();
//    p.addPointProperty( POINT_PROPERTY.BEWUCHS_AX );
//    return p;
//  }
//
//  public IProfil createGeoCoord( )
//  {
//    final IProfil p = createWspWinProf();
//    p.addPointProperty( POINT_PROPERTY.HOCHWERT );
//    return p;
//  }
//
//  public IProfil createWehr( ) throws IllegalProfileOperationException
//  {
//    final IProfil p = createMinProf();
//    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
//    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( IWspmTuhhConstants.BUILDING_TYP_WEHR ) );
//    final IProfilPoint pf = p.getPoints().getFirst();
//    final IProfilPoint pl = p.getPoints().getLast();
//    final IProfilPoint pm = ProfilUtil.getPointAfter( p, pf );
//
//    p.addDevider( pf, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( pl, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
//    p.addDevider( pm, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    p.addDevider( ProfilUtil.getPointBefore( p, pl ), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//    final List<IProfilPoint> oList = ProfilUtil.getInnerPoints( p, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
//
//    for( IProfilPoint pkt : oList )
//    {
//      pkt.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, pm.getValueFor( POINT_PROPERTY.HOEHE ) );
//    }
//    return p;
//  }
//
//  public void writePrf( ArrayList<IProfil> profs ) throws FileNotFoundException
//  {
//
//    final IProfilSink ps = new PrfSink();
//    Integer nr = 0;
//    for( IProfil p : profs )
//    {
//      p.setStation( nr );
//      final File outPrfFile = new File( dir + String.format( Locale.US, "%04d", nr++ ) + ".prf" );
//      final PrintWriter prfWriter = new PrintWriter( outPrfFile );
//      ps.write( p, prfWriter );
//    }
//  }
//
//  public void writeStr( ArrayList<IProfil> profs ) throws IOException
//  {
//    final FileWriter f = new FileWriter( dir + "ProfilTest.str" );
//    final Integer cnt = profs.size();
//
//    f.write( String.format( Locale.US, "%5d", cnt ) + String.format( Locale.US, "%5d", cnt - 1 ) + "      Test       Prf" );
//    for( Integer stat = 0; stat < cnt; stat++ )
//    {
//
//      f.write( "\nTest" + String.format( Locale.US, "%10d", stat ) + " 0         0   Ist        " + String.format( Locale.US, "%04d", stat ) + ".prf" );
//    }
//
//    for( Integer stat = 1; stat < cnt; stat++ )
//    {
//      f.write( "\n" + String.format( Locale.US, "%.6f", new Double( stat - 1 ) ) + String.format( Locale.US, "% .6f", new Double( stat ) ) + " 1.000 1.000 1.000 "
//          + String.format( Locale.US, "%04d", stat - 1 ) + ".prf " + String.format( Locale.US, "%04d", stat ) + ".prf" );
//    }
//
//    f.flush();
//    f.close();
//  }
//
//  public IProfil createMinProf( )
//  {
//    final IProfil p = ProfilFactory.createProfil( "org.kalypso.model.wspm.tuhh.profiletype" );
//    p.addPoint( 0, 100 );
//    p.addPoint( 15, 70 );
//    p.addPoint( 35, 40 );
//    p.addPoint( 50, 20 );
//    p.addPoint( 65, 40 );
//    p.addPoint( 85, 70 );
//    p.addPoint( 100, 100 );
//    return p;
//  }
  //kann man eventuell im WSPWINEXPORTER benutzen
//  private static void writeWspWinFiles( final TuhhCalculation calculation, final boolean isDirectionUpstreams, final File strFile, final File cfgFile ) throws IOException
//  {
//    final TuhhReach reach = calculation.getReach();
//    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
//    final String[] wspWinPrfNames = new String[segments.length];
//    final Double[] wspWinPrfStations = new Double[segments.length];
//
//    final TuhhStationRange stationRange = new TuhhStationRange( calculation, isDirectionUpstreams );
//    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( isDirectionUpstreams );
//
//    Arrays.sort( segments, stationComparator );
//
//    PrintWriter cfgWriter = null;
//    PrintWriter strWriter = null;
//    try
//    {
//      cfgFile.getParentFile().mkdirs();
//      cfgWriter = new PrintWriter( new BufferedWriter( new FileWriter( cfgFile ) ) );
//
//      strFile.getParentFile().mkdirs();
//      strWriter = new PrintWriter( new BufferedWriter( new FileWriter( strFile ) ) );
//
//      strWriter.println( String.format( Locale.US, "%5d", wspWinPrfNames.length ) + " " + String.format( Locale.US, "%5d", wspWinPrfNames.length - 1 ) + "     WspWin    Export" );
//
//      int fileCount = 0;
//      for( final TuhhReachProfileSegment segment : segments )
//      {
//        final BigDecimal station = segment.getStation();
//        if( stationRange.isOutside( station ) )
//          continue;
//
//        wspWinPrfNames[fileCount] = "Prf_" + fileCount + ".prf";
//        wspWinPrfStations[fileCount++] = station.doubleValue();
//      }
//      for( int i = 0; i < wspWinPrfStations.length; i++ )
//      {
//        strWriter.println( "WspWin      " + String.format( Locale.US, "%.4f", wspWinPrfStations[i] ) + "  0         0   Export    " + wspWinPrfNames[i] );
//      }
//      strWriter.println( "" );
//      for( int i = 1; i < wspWinPrfStations.length; i++ )
//      {
//        final String dist = String.format( Locale.US, "%.4f", wspWinPrfStations[i] - wspWinPrfStations[i - 1] );
//        strWriter.println( String.format( Locale.US, "%.6f", wspWinPrfStations[i] ) + " " + String.format( Locale.US, "%.6f", wspWinPrfStations[i] ) + " " + dist + " " + dist + " " + dist + " "
//            + wspWinPrfNames[i - 1] + " " + wspWinPrfNames[i] );
//      }
//      cfgWriter.println( "   19    1   20 b" );
//      cfgWriter.println( "WspWin         Export         11.11.1111       " + String.format( Locale.US, "%.6f", wspWinPrfStations[0] ) + "       "
//          + String.format( Locale.US, "%.6f", wspWinPrfStations[wspWinPrfStations.length - 1] ) + "   WspWin.str" );
//      cfgWriter.close();
//      strWriter.close();
//    }
//    finally
//    {
//      IOUtils.closeQuietly( cfgWriter );
//      IOUtils.closeQuietly( strWriter );
//    }
//  }
  
}
