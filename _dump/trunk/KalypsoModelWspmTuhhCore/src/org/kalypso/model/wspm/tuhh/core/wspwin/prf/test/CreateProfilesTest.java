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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import junit.framework.TestCase;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;

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
//    final ArrayList<IProfil> p = new ArrayList<IProfil>();
//
//    p.add( createMinProf() );
//    p.add( createWspWinProf() );
//    p.add( createBruecke() );
//    p.add( createWehr() );
//    p.add( createBewuchs() );
//    p.add( createGeoCoord() );
//    p.add( createDurchlass( BUILDING_TYP.EI ) );
//    p.add( createDurchlass( BUILDING_TYP.KREIS ) );
//    p.add( createDurchlass( BUILDING_TYP.MAUL ) );
//    p.add( createDurchlass( BUILDING_TYP.TRAPEZ ) );
//    writePrf( p );
//    writeStr( p );

  }

  public IProfil createWspWinProf( )
  {
    final IProfil p = createMinProf();
    p.addPointProperty( POINT_PROPERTY.RAUHEIT );

    p.addDevider( p.getPoints().get( 0 ), IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( p.getPoints().get( 4 ), IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( p.getPoints().get( 1 ), IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    p.addDevider( p.getPoints().get( 3 ), IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );

    return p;
  }

  public IProfil createBruecke( ) throws ProfilDataException
  {
    final IProfil p = createMinProf();
    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( IProfilConstants.BUILDING_TYP_BRUECKE ) );
    final IProfilPoint pf = p.getPoints().getFirst();
    final IProfilPoint pl = p.getPoints().getLast();
    final IProfilPoint pm = ProfilUtil.getPointAfter( p, pf );

    p.addDevider( pf, IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( pl, IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( pm, IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    p.addDevider( ProfilUtil.getPointBefore( p, pl ), IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    final List<IProfilPoint> oList = ProfilUtil.getInnerPoints( p, IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    final List<IProfilPoint> uList = ProfilUtil.getInnerPoints( p, IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    for( IProfilPoint pkt : oList )
    {
      pkt.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, pf.getValueFor( POINT_PROPERTY.HOEHE ) );

    }
    for( IProfilPoint pkt : uList )
    {
      pkt.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, pm.getValueFor( POINT_PROPERTY.HOEHE ) );

    }
    return p;
  }

  public IProfil createDurchlass( String bt ) throws ProfilDataException
  {
    final IProfil p = createWspWinProf();
    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( bt ) );
    return p;
  }

  public IProfil createBewuchs( )
  {
    final IProfil p = createWspWinProf();
    p.addPointProperty( POINT_PROPERTY.BEWUCHS_AX  );
    return p;
  }
  public IProfil createGeoCoord( )
  {
    final IProfil p = createWspWinProf();
    p.addPointProperty( POINT_PROPERTY.HOCHWERT  );
    return p;
  }

  public IProfil createWehr( ) throws ProfilDataException
  {
    final IProfil p = createMinProf();
    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
    p.setBuilding( ProfilBuildingFactory.createProfilBuilding( IProfilConstants.BUILDING_TYP_WEHR ) );
    final IProfilPoint pf = p.getPoints().getFirst();
    final IProfilPoint pl = p.getPoints().getLast();
    final IProfilPoint pm = ProfilUtil.getPointAfter( p, pf );

    p.addDevider( pf, IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( pl, IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( pm, IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    p.addDevider( ProfilUtil.getPointBefore( p, pl ), IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    final List<IProfilPoint> oList = ProfilUtil.getInnerPoints( p, IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );

    for( IProfilPoint pkt : oList )
    {
      pkt.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, pm.getValueFor( POINT_PROPERTY.HOEHE ) );
    }
    return p;
  }

  public void writePrf( ArrayList<IProfil> profs ) throws FileNotFoundException
  {

    final IProfilSink ps = new PrfSink();
    Integer nr = 0;
    for( IProfil p : profs )
    {
      p.setStation( nr );
      final File outPrfFile = new File( dir + String.format( Locale.US, "%04d", nr++ ) + ".prf" );
      final PrintWriter prfWriter = new PrintWriter( outPrfFile );
      ps.write( p, prfWriter );
    }
  }

  public void writeStr( ArrayList<IProfil> profs ) throws IOException
  {
    final FileWriter f = new FileWriter( dir + "ProfilTest.str" );
    final Integer cnt = profs.size();

    f.write( String.format( Locale.US, "%5d", cnt ) + String.format( Locale.US, "%5d", cnt - 1 ) + "      Test       Prf" );
    for( Integer stat = 0; stat < cnt; stat++ )
    {

      f.write( "\nTest" + String.format( Locale.US, "%10d", stat ) + " 0         0   Ist        " + String.format( Locale.US, "%04d", stat ) + ".prf" );
    }

    for( Integer stat = 1; stat < cnt; stat++ )
    {
      f.write( "\n" + String.format( Locale.US, "%.6f", new Double( stat - 1 ) ) + String.format( Locale.US, "% .6f", new Double( stat ) ) + " 1.000 1.000 1.000 "
          + String.format( Locale.US, "%04d", stat - 1 ) + ".prf " + String.format( Locale.US, "%04d", stat ) + ".prf" );
    }

    f.flush();
    f.close();
  }

  public IProfil createMinProf( )
  {
    final IProfil p = ProfilFactory.createProfil();
    p.addPoint( 0, 100 );
    p.addPoint( 15, 70 );
    p.addPoint( 35, 40 );
    p.addPoint( 50, 20 );
    p.addPoint( 65, 40 );
    p.addPoint( 85, 70 );
    p.addPoint( 100, 100 );
    return p;
  }
}
