/*
 * Created on 02.03.2005
 */
package com.bce.eind.core.profil.impl.test;

import junit.framework.TestCase;

import com.bce.eind.core.profil.DeviderKey;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilPointProperty;
import com.bce.eind.core.profil.IProfil.RAUHEITEN_TYP;
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
  }

  public IProfil CreateTestProfil( ) throws Exception
  {
    final IProfil p = new Profil();
    final IProfilPoint p1 = p.addPoint( 100.0001, -100.0001 );
    final IProfilPoint p4 = p.addPoint( 200.0002, -200.0002 );
    final IProfilPoint p2 = p.insertPoint( p1 );
    final IProfilPoint p3 = p.insertPoint( p2 );
    
    assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() );
    assertEquals( "punkt1:", true, p1.isPosition( 100.0001, -100.0001 ) );
    assertEquals( "punkt2:", true, p2.isPosition( 150.00015, -150.00015 ) );
    assertEquals( "punkt3:", true, p3.isPosition( 175.0002, -175.0002 ) );
    assertEquals( "punkt4:", true, p4.isPosition( 200.0002, -200.0002 ) );

    p.setDevider( p1, p4, ProfilPointProperty.DURCHSTROEMTE );
    p.setDevider( p2, p3, ProfilPointProperty.TRENNFLAECHE );
 
    final IProfilPoint dpL = p.getDevider(DeviderKey.DURCHSTROEMTE_L );
    final IProfilPoint dpR = p.getDevider(DeviderKey.DURCHSTROEMTE_R );
    final IProfilPoint tpL = p.getDevider(DeviderKey.TRENNFLAECHE_L );
    final IProfilPoint tpR = p.getDevider(DeviderKey.TRENNFLAECHE_R );
    
    assertEquals("Durchströmter Bereich links:",p1,dpL);
    assertEquals("Durchströmter Bereich rechts",p4,dpR);
    assertEquals("Trennfläche links:",p2,tpL);
    assertEquals("Trennfläche rechts:",p3,tpR);
    
    p.setRauheitTyp(RAUHEITEN_TYP.RAUHEIT_KS);
    p.setValueFor(p2,ProfilPointProperty.RAUHEIT, 1.2345 );
    
    assertEquals("Rauheit TrennflächenPkt links:",1.2345,tpL.getValueFor(ProfilPointProperty.RAUHEIT));
    assertEquals("RauheitTyp:",RAUHEITEN_TYP.RAUHEIT_KS,p.getRauheitTyp());
    return p;
  }

  
  public void setGetMoveDevider( final IProfil p ) throws Exception
  {

    p.setDeviderTyp( DeviderKey.TRENNFLAECHE_R, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG );
    assertEquals( "Trennfläche rechts Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG, p
        .getDeviderTyp( DeviderKey.TRENNFLAECHE_R ) );

    p.setDeviderTyp( DeviderKey.TRENNFLAECHE_L, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE );
    assertEquals( "Trennfläche links Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE, p
        .getDeviderTyp( DeviderKey.TRENNFLAECHE_L ) );
    
    final IProfilPoint newPkt = p.insertPoint(p.getDevider(DeviderKey.DURCHSTROEMTE_L) );

    p.moveDevider( DeviderKey.DURCHSTROEMTE_L,newPkt );
    final IProfilPoint aktPkt = p.getDevider(DeviderKey.DURCHSTROEMTE_L) ;
    assertEquals( "neu Durchstroemte links:", newPkt,aktPkt);
  }
  
}
