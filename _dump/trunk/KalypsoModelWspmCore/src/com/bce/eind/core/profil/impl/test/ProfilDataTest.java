/*
 * Created on 02.03.2005
 */
package com.bce.eind.core.profil.impl.test;

import java.util.Iterator;

import junit.framework.TestCase;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.ProfilListener;
import com.bce.eind.core.profil.IProfil.METADATA;
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.Profil;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner
 */
public class ProfilDataTest extends TestCase
{
    
  public IProfil testAddInsertPoint( ) throws Exception
  {
    final IProfil p = new Profil();
    final IProfilPoint p1 = p.addPoint(100.0001, -100.0001 );
    final IProfilPoint p4 = p.addPoint(200.0002, -200.0002 );
    final IProfilPoint p2 = p.insertPoint(p1);
    final IProfilPoint p3 = p.insertPoint(p2);
    
    assertEquals( "punkt1:",true , p1.isEqualPosition(100.0001,-100.0001 ));
    assertEquals( "punkt2:",true , p2.isEqualPosition(150.00015,-150.00015 ));
    assertEquals( "punkt3:",true , p3.isEqualPosition(175.0002,-175.0002 ));
    assertEquals( "punkt4:",true , p4.isEqualPosition(200.0002,-200.0002 ));

    
    
    
    p.setDevider(p1,p4,POINT_PROPERTY.DURCHSTROEMTE);
    p.setDevider(p2,p3,POINT_PROPERTY.TRENNFLAECHE);
    p.setDeviderTyp(IProfil.TRENNFLAECHE_L,IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG);


    return p;
  }
 /* final IProfilPoint pkt4 = p.getPoints().get( 3 );
    p.insertPoint( pkt4, 888.9876, -123.4567 );
    assertEquals( "Punkt eingefügt:", pointsCount+2, p.getPointsCount() );
    assertEquals( "HOEHE(-123.4567):", -123.4567, p.getPoints().get( 4 )
        .getValueFor( IProfil.HOEHE ) );

    p.removePoint( pkt4 );
    assertEquals( "Punkt gelöscht:", pointsCount+1, p.getPointsCount() );
    assertEquals( "Ersetzt Breite(888.9876):", 888.9876, p.getPoints().get( 3 ).getValueFor(
        IProfil.BREITE ) );

    p.insertPoint( null, 1234.5678, 8765.4321 );
    assertEquals( "Punkt vorne eingefügt:", pointsCount+2, p.getPointsCount() );
    assertEquals( "HOEHE(8765.4321):", 8765.4321, p.getPoints().get( 0 )
        .getValueFor( IProfil.HOEHE ) );

  }*/

  public void testSetGetMoveDevider( ) throws Exception
  {
    final IProfil p = testAddInsertPoint( );

    assertEquals( "Anzahl Koordinaten:",4, p.getPointsCount() );
  
    final IProfilPoint pktTrennL = p.getDevider( IProfil.TRENNFLAECHE_L );
    assertEquals( "Trennfläche links:",true , pktTrennL.isEqualPosition( 150.00015,-150.00015 ));
    final IProfilPoint pktTrennR = p.getDevider( IProfil.TRENNFLAECHE_R );
    assertEquals( "Trennfläche rechts:",true , pktTrennR.isEqualPosition(175.0002,-175.0002) );
    final IProfilPoint pktDurchL = p.getDevider( IProfil.DURCHSTROEMTE_L );
    assertEquals( "Durchstroemte links:", true , pktDurchL.isEqualPosition(100.0001,-100.0001) );
    final IProfilPoint pktDurchR = p.getDevider( IProfil.DURCHSTROEMTE_R );
    assertEquals( "Durchstroemte rechts:",true,pktDurchR.isEqualPosition( 200.0002,-200.0002 ) );

    p.setDeviderTyp( IProfil.TRENNFLAECHE_R, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG );
    assertEquals( "Trennfläche rechts Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_BOESCHUNG, p
        .getDeviderTyp( IProfil.TRENNFLAECHE_R ) );

    p.setDeviderTyp( IProfil.TRENNFLAECHE_L, IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE );
    assertEquals( "Trennfläche links Typ:", IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_SOHLE, p
        .getDeviderTyp( IProfil.TRENNFLAECHE_L ) );

    p.moveDevider( IProfil.DURCHSTROEMTE_R, pktTrennR );
    final IProfilPoint newPkt = p.getDevider( IProfil.DURCHSTROEMTE_R );
    assertEquals( "neu Durchstroemte rechts:", pktTrennR.getValueFor( POINT_PROPERTY.BREITE ), newPkt
        .getValueFor( POINT_PROPERTY.BREITE ) );
  }
  /*
  public void testSplitSegment( ) throws Exception
  {
    final IProfil p = testAddInsertPoint( );

    assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() );

    final IProfilPoint startPoint = p.getPoint( 0 );
    final IProfilPoint nextPoint = p.getNextPoint( startPoint );
    final IProfilPoint middlePoint = ProfilUtil.splitSegment( startPoint, nextPoint );
    assertEquals( "Interpolierter Punkt(Breite):", -194.34, middlePoint.getValueFor( POINT_PROPERTY.BREITE ) );
    assertEquals( "Interpolierter Punkt(Höhe):", 66.95, middlePoint.getValueFor( POINT_PROPERTY.HOEHE ) );
  }

  public void testClonePoint( ) throws Exception
  {
    final IProfil p = testAddInsertPoint( );

    assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() );
    final IProfilPoint startPoint = p.getPoint( 3 );
    final IProfilPoint clonePoint = startPoint.clonePoint();

    for( final Iterator<POINT_PROPERTY> tdkIt = p.getProfilPointProperties(true).iterator(); tdkIt.hasNext(); )
    {
      final POINT_PROPERTY tdk = tdkIt.next();
      assertEquals( tdk.toString(), startPoint.getValueFor( tdk ), clonePoint.getValueFor( tdk ) );
    }
  }

  public void testTableListener( ) throws Exception
  {
    class TestListener implements ProfilListener
    {
      public void onPointChanged( final IProfilPoint point, IProfilPointProperty pointProperty )
      {
        // TODO Auto-generated method stub
      }

      public void onMetaDataChanged( METADATA metadata, String value )
      {
        // TODO Auto-generated method stub
        
      }

      public void onProfilDataChanged( IProfilPointProperty pointProperty, Object value )
      {
        // TODO Auto-generated method stub
        
      }

 
    }
    final IProfil p = testAddInsertPoint( );

    assertEquals( "Anzahl Koordinaten:", 4, p.getPointsCount() );
    final TestListener testListener = new TestListener();
    p.addProfilListener( testListener );
    final IProfilPoint point = p.getPoint( 3 );
    p.setValueFor( point, POINT_PROPERTY.RAUHEIT, 0.07 );
    p.setProfilMetaData(METADATA.STATUS,new String("Na so was"));
    p.setRauheitTyp(IProfil.RAUHEITEN_TYP.RAUHEIT_KST);
    p.removeProfilListener( testListener );
  }*/
}
