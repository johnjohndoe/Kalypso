package com.bce.eind.core.profil.impl.test;

import java.util.LinkedList;

import junit.framework.TestCase;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilBuildingFactory;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfil.PROFIL_PROPERTY;
import com.bce.eind.core.profil.IProfil.RAUHEIT_TYP;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

/**
 * @author kimwerner
 */
public class ProfilDataTest extends TestCase
{
  public void testRunTest( ) throws Exception
  {
    final IProfil p = CreateTestProfil();
    setGetMoveDevider( p );
    setGetBuilding( p );
    addMoveDeleteBordvoll( p );
  }

  public IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = new PlainProfil();
    final IProfilPoint p1 = p.addPoint( 100.0001, -100.0001 );
    final IProfilPoint p4 = p.addPoint( 200.0002, -200.0002 );
    final IProfilPoint p2 = p.insertPoint( p1 );
    final IProfilPoint p3 = p.insertPoint( p2 );

    assertEquals( "Anzahl Koordinaten:", 4, p.getPoints().size() );
    assertEquals( "punkt1:", p1, p.findPoint( 100.0001, 0 ) );
    assertEquals( "punkt4:", p4, p.findPoint( 200.0002, 0 ) );

    assertEquals( "punkt3:", p3, p.findPoint( 175.0002, 0.0001 ) );
    assertEquals( "punkt2:", p2, p.findPoint( 150.00015, 0.0001 ) );

    p.addDevider( p1, DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( p2, DEVIDER_TYP.TRENNFLAECHE );
    p.addDevider( p3, DEVIDER_TYP.TRENNFLAECHE );
    p.addDevider( p4, DEVIDER_TYP.DURCHSTROEMTE );

    final IProfilPoint dpL = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[0].getPoint();
    final IProfilPoint dpR = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[1].getPoint();
    final IProfilPoint tpL = p.getDevider( DEVIDER_TYP.TRENNFLAECHE )[0].getPoint();
    final IProfilPoint tpR = p.getDevider( DEVIDER_TYP.TRENNFLAECHE )[1].getPoint();

    assertEquals( "Durchströmter Bereich links:", p1, dpL );
    assertEquals( "Durchströmter Bereich rechts", p4, dpR );
    assertEquals( "Trennfläche links:", p2, tpL );
    assertEquals( "Trennfläche rechts:", p3, tpR );

    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
    p.setProperty( PROFIL_PROPERTY.RAUHEIT_TYP, RAUHEIT_TYP.ks );
    p2.setValueFor( POINT_PROPERTY.RAUHEIT, 1.2345 );

    assertEquals( "Rauheit TrennflächenPkt links:", 1.2345, tpL.getValueFor( POINT_PROPERTY.RAUHEIT ) );
    assertEquals( "RauheitTyp:", RAUHEIT_TYP.ks, p.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP ) );

    return p;
  }

  public void setGetMoveDevider( final IProfil p ) throws Exception
  {
    final IProfilDevider[] deviderTF = p.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    final IProfilDevider rightTF = deviderTF[1];
    final IProfilDevider leftTF = deviderTF[0];
    rightTF.setValueFor(DEVIDER_PROPERTY.BOESCHUNG, false );
    assertEquals( "Trennfläche rechts Typ:", false, rightTF
        .getValueFor( DEVIDER_PROPERTY.BOESCHUNG ) );

    leftTF.setValueFor(DEVIDER_PROPERTY.BOESCHUNG,true );
    assertEquals( "Trennfläche rechts Typ:", DEVIDER_PROPERTY.BOESCHUNG, leftTF
        .getValueFor( DEVIDER_PROPERTY.BOESCHUNG) );

    final IProfilPoint newPkt = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[0].getPoint();
    p.moveDevider( p.getDevider( DEVIDER_TYP.TRENNFLAECHE )[0], newPkt );
    final IProfilPoint aktPkt = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[0].getPoint();
    assertEquals( "neu Durchstroemte links:", newPkt, aktPkt );
  }

  public void addMoveDeleteBordvoll( final IProfil p ) throws Exception
  {
    final LinkedList<IProfilPoint> pktlst = p.getPoints();
    final IProfilPoint pkt = p.insertPoint( pktlst.getFirst() );
    final IProfilDevider deviderBVL = p.addDevider( pkt, DEVIDER_TYP.BORDVOLL );
    final IProfilDevider deviderBVR = p.addDevider( pkt, DEVIDER_TYP.BORDVOLL );
    p.moveDevider( deviderBVR, pktlst.getLast() );
    final IProfilPoint aktPkt = deviderBVR.getPoint();
    assertEquals( "Bordvoll links:", pkt, deviderBVL.getPoint() );
    assertEquals( "Bordvoll rechts:", aktPkt, deviderBVR.getPoint() );
    p.removeDevider( deviderBVL );
    final IProfilDevider[] deviders = p.getDevider( DEVIDER_TYP.BORDVOLL );
    assertEquals( "Anzahl Bordvollpunkte:", 1, deviders.length );
    assertEquals( "Bordvoll links:", pktlst.getLast(), deviders[0].getPoint() );

  }

  public void setGetBuilding( final IProfil p ) throws Exception
  {
    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( BUILDING_TYP.BRUECKE );
    p.setBuilding( building );
    assertEquals( "neues Gebäude:", BUILDING_TYP.BRUECKE, p.getBuilding().getTyp() );
    final IProfilPoint firstPkt = p.getPoints().getFirst();
    firstPkt.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, 1000.65432 );
    firstPkt.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, 1000.23456 );
    p.getBuilding().setValue( BUILDING_PROPERTY.PFEILERFORM, 0.5 );
    p.getBuilding().setValue(BUILDING_PROPERTY.RAUHEIT, 5.5 );
    assertEquals( "Pfeiler Formbeiwert:", 0.5, p.getBuilding().getValueFor(
        BUILDING_PROPERTY.PFEILERFORM ) );
    assertEquals( "Hoehe Unterkante: ", 1000.23456, firstPkt
        .getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE ) );
    p.removeBuilding();
    assertEquals( "kein Gebäude:", BUILDING_TYP.NONE, p.getBuilding().getTyp() );

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
