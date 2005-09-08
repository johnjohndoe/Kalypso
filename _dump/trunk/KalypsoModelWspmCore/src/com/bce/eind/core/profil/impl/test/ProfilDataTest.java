package com.bce.eind.core.profil.impl.test;

import java.util.LinkedList;

import junit.framework.TestCase;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilConstants.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilConstants.PROFIL_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.RAUHEIT_PROPERTY;
import com.bce.eind.core.profil.impl.Profil;

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
    addMoveDeleteBordvoll(p);
  }

  public IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = new Profil();
    final IProfilPoint p1 = p.addPoint( 100.0001, -100.0001 );
    final IProfilPoint p4 = p.addPoint( 200.0002, -200.0002 );
    final IProfilPoint p2 = p.insertPoint( p1 );
    final IProfilPoint p3 = p.insertPoint( p2 );

    assertEquals( "Anzahl Koordinaten:", 4, p.getPoints().size());
    assertEquals( "punkt1:", p1, p.findPoint( 100.0001,0) );
    assertEquals( "punkt4:", p4, p.findPoint(  200.0002,0) );

    assertEquals( "punkt3:", p3, p.findPoint(  175.0002,0.0001) );
    assertEquals( "punkt2:", p2, p.findPoint(  150.00015,0.0001 ) );


    p.addDevider( p1,DEVIDER_TYP.DURCHSTROEMTE );
    p.addDevider( p2, DEVIDER_TYP.FLIESSZONE );
    p.addDevider( p3, DEVIDER_TYP.FLIESSZONE );
    p.addDevider( p4, DEVIDER_TYP.DURCHSTROEMTE );

    final IProfilPoint dpL = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[0].getPoint();
    final IProfilPoint dpR = p.getDevider( DEVIDER_TYP.DURCHSTROEMTE )[1].getPoint();
    final IProfilPoint tpL = p.getDevider( DEVIDER_TYP.FLIESSZONE)[0].getPoint();
    final IProfilPoint tpR = p.getDevider( DEVIDER_TYP.FLIESSZONE)[1].getPoint();

    assertEquals( "Durchströmter Bereich links:", p1, dpL );
    assertEquals( "Durchströmter Bereich rechts", p4, dpR );
    assertEquals( "Trennfläche links:", p2, tpL );
    assertEquals( "Trennfläche rechts:", p3, tpR );

    p.addPointProperty(PointProperty.RAUHEIT);
    p.setProperty( PROFIL_PROPERTY.RAUHEIT_TYP,RAUHEIT_PROPERTY.ks );
    p.setValueFor( p2, PointProperty.RAUHEIT, 1.2345 );

    assertEquals( "Rauheit TrennflächenPkt links:", 1.2345, tpL
        .getValueFor( PointProperty.RAUHEIT ) );
    assertEquals( "RauheitTyp:", RAUHEIT_PROPERTY.ks, p.getProperty(PROFIL_PROPERTY.RAUHEIT_TYP));
   
    return p;
  }

  public void setGetMoveDevider( final IProfil p ) throws Exception
  {
final IProfilDevider[] deviderTF = p.getDevider(DEVIDER_TYP.FLIESSZONE);
final IProfilDevider rightTF = deviderTF[1];
final IProfilDevider leftTF = deviderTF[0];
rightTF.setValueFor(DEVIDER_PROPERTY.class,DEVIDER_PROPERTY.SOHLE);
    assertEquals( "Trennfläche rechts Typ:", DEVIDER_PROPERTY.SOHLE, rightTF.getValueFor(DEVIDER_PROPERTY.class ) );

    leftTF.setValueFor(DEVIDER_PROPERTY.class,DEVIDER_PROPERTY.BOESCHUNG);
    assertEquals( "Trennfläche rechts Typ:", DEVIDER_PROPERTY.BOESCHUNG, leftTF.getValueFor(DEVIDER_PROPERTY.class ) );

    final IProfilPoint newPkt = p.getDevider(DEVIDER_TYP.DURCHSTROEMTE)[0].getPoint(); 
    p.moveDevider( p.getDevider(DEVIDER_TYP.FLIESSZONE)[0],newPkt );
    final IProfilPoint aktPkt = p.getDevider(DEVIDER_TYP.DURCHSTROEMTE)[0].getPoint();
    assertEquals( "neu Durchstroemte links:", newPkt, aktPkt );
  }

  public void addMoveDeleteBordvoll( final IProfil p ) throws Exception
  {
    final LinkedList<IProfilPoint> pktlst = p.getPoints();
    final IProfilPoint pkt = p.insertPoint(pktlst.getFirst());
    final IProfilDevider deviderBVL = p.addDevider(pkt,DEVIDER_TYP.BORDVOLL);
    final IProfilDevider deviderBVR = p.addDevider(pkt,DEVIDER_TYP.BORDVOLL);
    p.moveDevider(deviderBVR,pktlst.getLast());
    final IProfilPoint aktPkt = deviderBVR.getPoint();
    assertEquals( "Bordvoll links:", pkt, deviderBVL.getPoint());
    assertEquals( "Bordvoll rechts:", aktPkt, deviderBVR.getPoint());
    p.removeDevider(deviderBVL);
    final IProfilDevider[] deviders = p.getDevider(DEVIDER_TYP.BORDVOLL);
    assertEquals( "Anzahl Bordvollpunkte:", 1, deviders.length);
    assertEquals( "Bordvoll links:", pktlst.getLast(), deviders[0].getPoint());
    
  }

  public void setGetBuilding( final IProfil p ) throws Exception
  {
    p.setBuilding( BUILDING_TYP.BRUECKE );
    assertEquals( "neues Gebäude:", BUILDING_TYP.BRUECKE, p.getBuilding()
        .getBuildingTyp() );
    final IProfilPoint firstPkt = p.getPoints().getFirst();
    p.setValueFor( firstPkt, PointProperty.OBERKANTEBRUECKE, 1000.65432 );
    p.setValueFor( firstPkt, PointProperty.UNTERKANTEBRUECKE, 1000.23456 );
    p.getBuilding().setValue( BUILDING_PROPERTY.PFEILERFORM, 0.5 );
    p.getBuilding().setValue( BUILDING_PROPERTY.RAUHEIT, 5.5 );
    assertEquals( "Pfeiler Formbeiwert:", 0.5, p.getBuilding().getValue(
        BUILDING_PROPERTY.PFEILERFORM ) );
    assertEquals( "Hoehe Unterkante: ", 1000.23456, firstPkt
        .getValueFor( PointProperty.UNTERKANTEBRUECKE ) );
    p.removeBuilding();
    assertEquals( "kein Gebäude:", BUILDING_TYP.NONE, p.getBuilding()
        .getBuildingTyp() );

    try
    {
      firstPkt.getValueFor( PointProperty.UNTERKANTEBRUECKE );
    }
    catch( ProfilDataException e )
    {
      assertEquals( "Exception erwartet: ", ProfilDataException.class, e.getClass() );
    }

  }
}
