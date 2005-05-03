/*
 * Created on 02.03.2005
 */
package com.bce.eind.core.profil.impl.test;

import junit.framework.TestCase;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;
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

    p.setDevider( p1, p4, POINT_PROPERTY.DURCHSTROEMTE );
    p.setDevider( p2, p3, POINT_PROPERTY.TRENNFLAECHE );
 
    final IProfilPoint dpL = p.getDevider(IProfil.DURCHSTROEMTE_L );
    final IProfilPoint dpR = p.getDevider(IProfil.DURCHSTROEMTE_R );
    final IProfilPoint tpL = p.getDevider(IProfil.TRENNFLAECHE_L );
    final IProfilPoint tpR = p.getDevider(IProfil.TRENNFLAECHE_R );
    
    assertEquals("Durchstr�mter Bereich links:",p1,dpL);
    assertEquals("Durchstr�mter Bereich rechts",p4,dpR);
    assertEquals("Trennfl�che links:",p2,tpL);
    assertEquals("Trennfl�che rechts:",p3,tpR);
    
    p.setRauheitTyp(RAUHEITEN_TYP.RAUHEIT_KS);
    p.setValueFor(p2,POINT_PROPERTY.RAUHEIT, 1.2345 );
    
    assertEquals("Rauheit Trennfl�chenPkt links:",1.2345,tpL.getValueFor(POINT_PROPERTY.RAUHEIT));
    assertEquals("RauheitTyp:",RAUHEITEN_TYP.RAUHEIT_KS,p.getRauheitTyp());
    return p;
  }

  
  public void setGetMoveDevider( final IProfil p ) throws Exception
  {

    p.setDeviderTyp( IProfil.TRENNFLAECHE_R, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG );
    assertEquals( "Trennfl�che rechts Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG, p
        .getDeviderTyp( IProfil.TRENNFLAECHE_R ) );

    p.setDeviderTyp( IProfil.TRENNFLAECHE_L, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE );
    assertEquals( "Trennfl�che links Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE, p
        .getDeviderTyp( IProfil.TRENNFLAECHE_L ) );
    
    final IProfilPoint newPkt = p.insertPoint(p.getDevider(IProfil.TRENNFLAECHE_L) );

    p.moveDevider( IProfil.DURCHSTROEMTE_R,newPkt );
    assertEquals( "neu Durchstroemte links:", newPkt,p.getDevider(IProfil.TRENNFLAECHE_L) );
  }
  /*
   * public void testSplitSegment( ) throws Exception { final IProfil p = testAddInsertPoint( );
   * assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() ); final IProfilPoint startPoint =
   * p.getPoint( 0 ); final IProfilPoint nextPoint = p.getNextPoint( startPoint ); final
   * IProfilPoint middlePoint = ProfilUtil.splitSegment( startPoint, nextPoint ); assertEquals(
   * "Interpolierter Punkt(Breite):", -194.34, middlePoint.getValueFor( POINT_PROPERTY.BREITE ) );
   * assertEquals( "Interpolierter Punkt(H�he):", 66.95, middlePoint.getValueFor(
   * POINT_PROPERTY.HOEHE ) ); } public void testClonePoint( ) throws Exception { final IProfil p =
   * testAddInsertPoint( ); assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() ); final
   * IProfilPoint startPoint = p.getPoint( 3 ); final IProfilPoint clonePoint =
   * startPoint.clonePoint(); for( final Iterator<POINT_PROPERTY> tdkIt =
   * p.getProfilPointProperties(true).iterator(); tdkIt.hasNext(); ) { final POINT_PROPERTY tdk =
   * tdkIt.next(); assertEquals( tdk.toString(), startPoint.getValueFor( tdk ),
   * clonePoint.getValueFor( tdk ) ); } } public void testTableListener( ) throws Exception { class
   * TestListener implements ProfilListener { public void onPointChanged( final IProfilPoint point,
   * IProfilPointProperty pointProperty ) { // TODO Auto-generated method stub } public void
   * onMetaDataChanged( METADATA metadata, String value ) { // TODO Auto-generated method stub }
   * public void onProfilDataChanged( IProfilPointProperty pointProperty, Object value ) { // TODO
   * Auto-generated method stub } } final IProfil p = testAddInsertPoint( ); assertEquals( "Anzahl
   * Koordinaten:", 4, p.getPointsCount() ); final TestListener testListener = new TestListener();
   * p.addProfilListener( testListener ); final IProfilPoint point = p.getPoint( 3 ); p.setValueFor(
   * point, POINT_PROPERTY.RAUHEIT, 0.07 ); p.setProfilMetaData(METADATA.STATUS,new String("Na so
   * was")); p.setRauheitTyp(IProfil.RAUHEITEN_TYP.RAUHEIT_KST); p.removeProfilListener(
   * testListener ); }
   */
}
