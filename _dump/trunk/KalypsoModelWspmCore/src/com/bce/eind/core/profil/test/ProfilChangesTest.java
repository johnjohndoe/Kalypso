package com.bce.eind.core.profil.test;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;

import com.bce.eind.ProfilFactory;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilBuildingFactory;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfil.PROFIL_PROPERTY;
import com.bce.eind.core.profil.IProfil.RAUHEIT_TYP;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.changes.BuildingEdit;
import com.bce.eind.core.profil.changes.BuildingSet;
import com.bce.eind.core.profil.changes.DeviderAdd;
import com.bce.eind.core.profil.changes.DeviderMove;
import com.bce.eind.core.profil.changes.DeviderRemove;
import com.bce.eind.core.profil.changes.PointAdd;
import com.bce.eind.core.profil.changes.PointPropertyAdd;
import com.bce.eind.core.profil.changes.PointPropertyEdit;
import com.bce.eind.core.profil.changes.ProfilChangeHint;
import com.bce.eind.core.profil.changes.ProfilPropertyEdit;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner
 */
public class ProfilChangesTest extends TestCase
{
  public void testRunTest( ) throws Exception
  {
    final IProfil p = CreateTestProfil();
    setGetDeleteBuilding( p );
    addMoveDeleteBordvoll( p );
  }

  @SuppressWarnings("boxing")
  public static IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = ProfilFactory.createProfil();
    final ArrayList<String> metaData = new ArrayList<String>();
    for(int i = 0;i < 13;i++)
    {
      metaData.add("");
    }
    IProfilChange change = new ProfilPropertyEdit(p,PROFIL_PROPERTY.METASTRINGS,metaData);
    change.doChange(null);
    final IProfilChange[] changes = new IProfilChange[1];
    final ProfilChangeHint hint = new ProfilChangeHint();

    changes[0] = new PointAdd( p, null, null );
    changes[0].doChange( hint );
    final IProfilPoint firstPkt = p.getPoints().getFirst();
    changes[0] = new PointPropertyEdit( firstPkt, POINT_PROPERTY.HOEHE, 50.0 );
    changes[0].doChange( hint );
    changes[0] = new PointAdd( p, firstPkt, null );
    changes[0].doChange( hint );
    final IProfilPoint lastPkt = p.getPoints().getLast();
    changes[0] = new PointPropertyEdit( lastPkt, POINT_PROPERTY.BREITE, 100.0 );
    changes[0].doChange( hint );
    changes[0] = new PointPropertyEdit( lastPkt, POINT_PROPERTY.HOEHE, 50.0 );
    changes[0].doChange( hint );

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

 

    changes[0] = new DeviderAdd( p, DEVIDER_TYP.DURCHSTROEMTE, p1 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, DEVIDER_TYP.DURCHSTROEMTE, p5 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, DEVIDER_TYP.TRENNFLAECHE, p2 );
    changes[0].doChange( hint );
    changes[0] = new DeviderAdd( p, DEVIDER_TYP.TRENNFLAECHE, p4 );
    changes[0].doChange( hint );

    final IProfilPoint dpL = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[0].getPoint();
    final IProfilPoint dpR = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[1].getPoint();
    final IProfilPoint tpL = p.getDevider( DEVIDER_TYP.TRENNFLAECHE )[0].getPoint();
    final IProfilPoint tpR = p.getDevider( DEVIDER_TYP.TRENNFLAECHE )[1].getPoint();

    assertEquals( "Durchströmter Bereich links:", p1, dpL );
    assertEquals( "Durchströmter Bereich rechts", p5, dpR );
    assertEquals( "Trennfläche links:", p2, tpL );
    assertEquals( "Trennfläche rechts:", p4, tpR );

    changes[0] = new PointPropertyAdd( p, POINT_PROPERTY.RAUHEIT, 1.2345 );
    changes[0].doChange( hint );
    changes[0] = new ProfilPropertyEdit( p, PROFIL_PROPERTY.RAUHEIT_TYP, RAUHEIT_TYP.ks );
    changes[0].doChange( hint );

    assertEquals( "Rauheit TrennflächenPkt links:", 1.2345, tpL.getValueFor( POINT_PROPERTY.RAUHEIT ) );
    assertEquals( "RauheitTyp:", RAUHEIT_TYP.ks, p.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP ) );

    return p;
  }

 
  public void addMoveDeleteBordvoll( final IProfil p ) throws Exception
  {
    final LinkedList<IProfilPoint> pktlst = p.getPoints();
    final IProfilPoint firstPkt = pktlst.getFirst();
    final IProfilPoint lastPkt = pktlst.getLast();
    final IProfilChange change1 = new DeviderAdd( p, DEVIDER_TYP.BORDVOLL, firstPkt );
    change1.doChange( null );
    final IProfilChange change2 = new DeviderAdd( p, DEVIDER_TYP.BORDVOLL, lastPkt );
    change2.doChange( null );

    final IProfilDevider[] deviders = p.getDevider( DEVIDER_TYP.BORDVOLL );
    final List<IProfilPoint> points = ProfilUtil.getInnerPoints( p, DEVIDER_TYP.TRENNFLAECHE );

    final IProfilChange change3 = new DeviderMove( deviders[0], points.get( 1 ) );
    change3.doChange( null );
    final IProfilChange change4 = new DeviderMove( deviders[1], points.get( 1 ) );
    change4.doChange( null );

    assertEquals( "Bordvollpunkte:", deviders[0].getPoint(), deviders[1].getPoint() );

    final IProfilChange change5 = new DeviderRemove( p, deviders[0] );
    change5.doChange( null );
    final IProfilChange change6 = new DeviderRemove( p, deviders[1] );
    change6.doChange( null );

    assertEquals( "Anzahl Bordvollpunkte:", null, p.getDevider( DEVIDER_TYP.BORDVOLL ) );

  }

  @SuppressWarnings("boxing")
  public void setGetDeleteBuilding( final IProfil p ) throws Exception
  {
    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( BUILDING_TYP.BRUECKE );
    IProfilChange change = new BuildingSet (p,building);
    IProfilChange undoChange = change.doChange(null);

    assertEquals( "neues Gebäude:", BUILDING_TYP.BRUECKE, p.getBuilding().getTyp() );
    final IProfilPoint firstPkt = p.getPoints().getFirst();
    change = new PointPropertyEdit(firstPkt,POINT_PROPERTY.UNTERKANTEBRUECKE ,1000.23456 );
    change.doChange(null);
    change = new BuildingEdit(building,BUILDING_PROPERTY.FORMBEIWERT, 0.5 );
    change.doChange(null);
    assertEquals( "Pfeiler Formbeiwert:", 0.5, p.getBuilding().getValueFor( BUILDING_PROPERTY.FORMBEIWERT ) );
    assertEquals( "Hoehe Unterkante: ", 1000.23456, firstPkt.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE ) );
    
    undoChange.doChange(null);
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
