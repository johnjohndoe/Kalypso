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
package org.kalypso.model.wspm.core.profil.test;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingEdit;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.core.profil.changes.DeviderAdd;
import org.kalypso.model.wspm.core.profil.changes.DeviderEdit;
import org.kalypso.model.wspm.core.profil.changes.DeviderMove;
import org.kalypso.model.wspm.core.profil.changes.DeviderRemove;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfilPropertyEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;

/**
 * @author kimwerner
 */
public class ProfilChangesTest extends TestCase
{
  public void testRunTest( ) throws Exception
  {
    final IProfil p = CreateTestProfil();
    setGetDeleteBruecke( p );
    addMoveDeleteBordvoll( p );
    setGetDeleteWehr(p);
  }

  @SuppressWarnings( { "boxing", "unchecked" })
  public static IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = ProfilFactory.createProfil( "org.kalypso.model.wspm.tuhh.profiletype" );
    
    // erzeuge Punkte
    final IProfilChange[] changes = new IProfilChange[1];
    final ProfilChangeHint hint = new ProfilChangeHint();

    final IProfilPoint firstPkt = p.addPoint( 0, 50.0 );
    final IProfilPoint lastPkt = p.addPoint( 100.0, 50.0 );

    final IProfilPoint p3 = ProfilUtil.splitSegment( firstPkt, lastPkt );
    changes[0] = new PointPropertyEdit( p3, POINT_PROPERTY.HOEHE, 0.0 );
    changes[0].doChange( hint );

    changes[0] = new PointAdd( p, firstPkt, p3 );
    changes[0].doChange( hint );

    final IProfilPoint p5 = ProfilUtil.splitSegment( p3, lastPkt );
    changes[0] = new PointAdd( p, p3, p5 );
    changes[0].doChange( hint );

    final IProfilPoint p4 = ProfilUtil.splitSegment( p3, p5 );
    changes[0] = new PointAdd( p, p3, p4 );
    changes[0].doChange( hint );

    final IProfilPoint p1 = ProfilUtil.splitSegment( firstPkt, p3 );
    changes[0] = new PointAdd( p, firstPkt, p1 );
    changes[0].doChange( hint );

    final IProfilPoint p2 = ProfilUtil.splitSegment( p1, p3 );
    changes[0] = new PointAdd( p, p1, p2 );
    changes[0].doChange( hint );

    // erzeuge Trenner
    changes[0] = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE, p1 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE, p5 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_TRENNFLAECHE, p2 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_TRENNFLAECHE, p4 );
    changes[0].doChange( hint );

    // TrennerTest
    final IProfilPoint dpL = p.getDevider(IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE )[0].getPoint();
    final IProfilPoint dpR = p.getDevider( IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE )[1].getPoint();
    final IProfilPoint tpL = p.getDevider( IProfilConstants.DEVIDER_TYP_TRENNFLAECHE )[0].getPoint();
    final IProfilPoint tpR = p.getDevider(IProfilConstants.DEVIDER_TYP_TRENNFLAECHE )[1].getPoint();
    assertEquals( "Durchströmter Bereich links:", p1, dpL );
    assertEquals( "Durchströmter Bereich rechts", p5, dpR );
    assertEquals( "Trennfläche links:", p2, tpL );
    assertEquals( "Trennfläche rechts:", p4, tpR );

    // erzeuge Rauheiten
    changes[0] = new PointPropertyAdd( p, POINT_PROPERTY.RAUHEIT, 1.2345 );
    changes[0].doChange( hint );
    changes[0] = new ProfilPropertyEdit( p, IProfilConstants.RAUHEIT_TYP, IProfilConstants.RAUHEIT_TYP_KS );
    changes[0].doChange( hint );

    // RauheitTest
    assertEquals( "Rauheit TrennflächenPkt links:", 1.2345, tpL.getValueFor( POINT_PROPERTY.RAUHEIT ) );
    assertEquals( "RauheitTyp:", IProfilConstants.RAUHEIT_TYP_KS, p.getProperty( IProfilConstants.RAUHEIT_TYP ) );

    // erzeuge Kommentar
    final ArrayList<String> stringList = new ArrayList<String>();
    stringList.add( "" );
    stringList.add( "Einzeiliger Kommentar" );
    stringList.add( "dritte Zeile" );
    changes[0] = new ProfilPropertyEdit( p, IProfilConstants.PROFIL_PROPERTY_KOMMENTAR, stringList );
    changes[0].doChange( hint );

    // KommentarTest
    ArrayList<String> c = (ArrayList<String>) p.getProperty( IProfilConstants.PROFIL_PROPERTY_KOMMENTAR );
    assertEquals( "Zeile1:", "", c.get( 0 ) );
    assertEquals( "Zeile2:", "Einzeiliger Kommentar", c.get( 1 ) );
    assertEquals( "Zeile3:", "dritte Zeile", c.get( 2 ) );
    
    return p;
  }

  public void addMoveDeleteBordvoll( final IProfil p ) throws Exception
  {
    final LinkedList<IProfilPoint> pktlst = p.getPoints();
    final IProfilPoint firstPkt = pktlst.getFirst();
    final IProfilPoint lastPkt = pktlst.getLast();
    final IProfilChange change1 = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_BORDVOLL, firstPkt );
    change1.doChange( null );
    final IProfilChange change2 = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_BORDVOLL, lastPkt );
    change2.doChange( null );

    final IProfilDevider[] deviders = p.getDevider( IProfilConstants.DEVIDER_TYP_BORDVOLL );
    final List<IProfilPoint> points = ProfilUtil.getInnerPoints( p, IProfilConstants.DEVIDER_TYP_TRENNFLAECHE );

    final IProfilChange change3 = new DeviderMove( deviders[0], points.get( 1 ) );
    change3.doChange( null );
    final IProfilChange change4 = new DeviderMove( deviders[1], points.get( 1 ) );
    change4.doChange( null );

    assertEquals( "Bordvollpunkte:", deviders[0].getPoint(), deviders[1].getPoint() );

    final IProfilChange change5 = new DeviderRemove( p, deviders[0] );
    change5.doChange( null );
    final IProfilChange change6 = new DeviderRemove( p, deviders[1] );
    change6.doChange( null );

    assertEquals( "Anzahl Bordvollpunkte:", null, p.getDevider(IProfilConstants.DEVIDER_TYP_BORDVOLL ) );

  }

  @SuppressWarnings("boxing")
  public void setGetDeleteBruecke( final IProfil p ) throws Exception
  {
    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( IProfilConstants.BUILDING_TYP_BRUECKE );
    IProfilChange change = new BuildingSet( p, building );
    IProfilChange undoChange = change.doChange( null );

    assertEquals( "neue Brücke:", IProfilConstants.BUILDING_TYP_BRUECKE , p.getBuilding().getTyp() );
    final IProfilPoint firstPkt = p.getPoints().getFirst();
    change = new PointPropertyEdit( firstPkt, POINT_PROPERTY.UNTERKANTEBRUECKE, 1000.23456 );
    change.doChange( null );
    change = new BuildingEdit( building, BUILDING_PROPERTY.FORMBEIWERT, 0.5 );
    change.doChange( null );
    assertEquals( "Pfeiler Formbeiwert:", 0.5, p.getBuilding().getValueFor( BUILDING_PROPERTY.FORMBEIWERT ) );
    assertEquals( "Hoehe Unterkante: ", 1000.23456, firstPkt.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE ) );

    undoChange.doChange( null );
    assertEquals( "kein Gebäude:", null, p.getBuilding() );

    try
    {
      firstPkt.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE );
    }
    catch( ProfilDataException e )
    {
      assertEquals( "Exception erwartet: ", ProfilDataException.class, e.getClass() );
    }

  }
  public void setGetDeleteWehr( final IProfil p ) throws Exception
  {
    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( IProfilConstants.BUILDING_TYP_WEHR);
    IProfilChange change = new BuildingSet( p, building );
    IProfilChange undoChange = change.doChange( null );

    assertEquals( "neues Wehr:", IProfilConstants.BUILDING_TYP_WEHR, p.getBuilding().getTyp() );
    final IProfilPoint firstPkt = p.getDevider(IProfilConstants.DEVIDER_TYP_TRENNFLAECHE)[0].getPoint();
    final IProfilPoint midPkt = ProfilUtil.getPointAfter(p,firstPkt);
    change = new PointPropertyEdit( midPkt, POINT_PROPERTY.OBERKANTEWEHR, 1000.23456 );
    change.doChange( null );
    change = new DeviderAdd( p, IProfilConstants.DEVIDER_TYP_WEHR ,midPkt);
    change.doChange( null );
    change = new DeviderEdit( p.getDevider(IProfilConstants.DEVIDER_TYP_WEHR)[0] ,DEVIDER_PROPERTY.BEIWERT,0.123);
    change.doChange( null );
    assertEquals( "Überfallbeiwert:", 0.123,  p.getDevider(IProfilConstants.DEVIDER_TYP_WEHR)[0].getValueFor(DEVIDER_PROPERTY.BEIWERT) );
    assertEquals( "Hoehe Wehrkante: ", 1000.23456, midPkt.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) );

    undoChange.doChange( null );
    assertEquals( "kein Gebäude:", null, p.getBuilding() );

    try
    {
      firstPkt.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE );
    }
    catch( ProfilDataException e )
    {
      assertEquals( "Exception erwartet: ", ProfilDataException.class, e.getClass() );
    }

  }

}
