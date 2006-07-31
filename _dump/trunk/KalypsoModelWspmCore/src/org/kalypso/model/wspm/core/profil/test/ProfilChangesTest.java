package org.kalypso.model.wspm.core.profil.test;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingEdit;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.core.profil.changes.DeviderAdd;
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
    setGetDeleteBuilding( p );
    addMoveDeleteBordvoll( p );
  }

  @SuppressWarnings("boxing")
  public static IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = null;// ProfilFactory.createProfil();
    final ArrayList<String> metaData = new ArrayList<String>();
    for( int i = 0; i < 13; i++ )
    {
      metaData.add( "" );
    }
    IProfilChange change = new ProfilPropertyEdit( p, "PROFIL_PROPERTY.METASTRINGS", metaData );
    change.doChange( null );
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
    final ArrayList<String> stringList = new ArrayList<String>();
    stringList.add( "" );
    stringList.add( "Einzeiliger Kommentar" );
    stringList.add( "dritte Zeile" );
    changes[0] = new ProfilPropertyEdit( p, PROFIL_PROPERTY.KOMMENTAR, stringList );
    changes[0].doChange( hint );
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
    IProfilChange change = new BuildingSet( p, building );
    IProfilChange undoChange = change.doChange( null );

    assertEquals( "neues Gebäude:", BUILDING_TYP.BRUECKE, p.getBuilding().getTyp() );
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
}
