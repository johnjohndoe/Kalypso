package com.bce.eind.core.profil;

/*
 * Created on 21.12.2004
 *
 */

import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.impl.devider.DeviderKey;
import com.bce.eind.core.profil.impl.points.ProfilPointProperty;



/**
 * @author kimwerner
 * 
 */
public interface IProfil
{
  public static final ProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true );

  public static final ProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true );

  public static final ProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true );

  public static final ProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true );

  public static final ProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchströmte", false, false );

  public static final ProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true, true );
  
  public static final ProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty( "Brückenunterkante", true, true );
  
  public static final ProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty( "Brückenoberkante", true, true );

  public static final ProfilPointProperty HOEHE = new ProfilPointProperty( "Höhe", false, true );

  public static final ProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false, true );

  public static final ProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true, true );

  public static final ProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefläche", false, false );

  public static final ProfilPointProperty BORDVOLL = new ProfilPointProperty ("Bordvollpunkt",true,true);
  
  public static final DeviderKey TRENNFLAECHE_L = new DeviderKey( TRENNFLAECHE, -1 );

  public static final DeviderKey TRENNFLAECHE_R = new DeviderKey( TRENNFLAECHE, 1 );

  public static final DeviderKey DURCHSTROEMTE_L = new DeviderKey( DURCHSTROEMTE, -1 );

  public static final DeviderKey DURCHSTROEMTE_R = new DeviderKey( DURCHSTROEMTE, 1 );
  
  public static final DeviderKey BORDVOLL_L = new DeviderKey( BORDVOLL, -1 );

  public static final DeviderKey BORDVOLL_R = new DeviderKey( BORDVOLL, 1 );

  
  public static enum TRENNFLAECHEN_TYP
  {
    UNDEFINED, TRENNFLAECHE_SOHLE, TRENNFLAECHE_BOESCHUNG
  };

  public static enum RAUHEITEN_TYP
  {
    RAUHEIT_UNDEFINED, RAUHEIT_KST, RAUHEIT_KS
  };
  
  public static enum PROFILDATA_TYP
  {
    RAUHEITTYP
  };
  public static enum METADATA
  {
    MEHRFELDBRUECKE,METASTRINGS,STATION,STATUS,VERZWEIGUNGSKENNUNG,WASSERSPIEGEL,KOMMENTAR
  };
  
  public static enum BUILDING_TYP
  {
    BLD_NONE, BLD_EI, BLD_KREIS, BLD_MAUL, BLD_TRAPEZ, BLD_BRUECKE
  };
  
 public static enum BUILDING_VALUES
 {
   CENTER_X, CENTER_Y, RADIUS
 };
  
  public void addProfilListener( final ProfilListener pl ); 
  
   public void removeProfilListener( final ProfilListener pl );
 
  public void moveDevider (final DeviderKey deviderKey,final IProfilPoint newPosition) throws ProfilDataException;
  
  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final ProfilPointProperty columnKey ) throws ProfilDataException;

  public void addProfilMetaData( final METADATA metaDataKey, final Object data );

  public IProfilPoint addPoint( final double breite, final double hoehe ) throws ProfilDataException;

  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException;
  
  /** TODO Kim: HW/RW müssen interpoliert werden. Am besten (interne) Eigenschaft für jede Spalte, ob interpoliert wird oder einfach übertragen */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException;
  
  public boolean addProfilPointProperty( final ProfilPointProperty columnKey );

  public IProfilPoint getDevider( final DeviderKey deviderKey );

  public Object getProfilMetaData(METADATA metaData);
  
  public IProfilPoint getPoint( final double breite, final double hoehe );

  public IProfilPoint getPoint( final int index );
  
  public IProfilPoint getNextPoint(IProfilPoint point) throws ProfilDataException;

  public List<IProfilPoint> getPoints( );

  public List<IProfilPoint> getPoints( final IProfilPoint startPoint );

  public LinkedList<IProfilPoint> getPointsAtPos( final double breite );

  public RAUHEITEN_TYP getRauheitTyp( );

  public int getPointsCount( );

  public LinkedList<ProfilPointProperty> getProfilPointProperties( );

  public boolean setValueFor( IProfilPoint point, ProfilPointProperty columnKey, double value ) throws ProfilDataException;

  public double[] getValuesFor( final ProfilPointProperty columnKey ) throws ProfilDataException;

  public boolean removeProfilPointProperty( final ProfilPointProperty columnKey );

  public boolean removeProfilMetaData( final METADATA metaData );

  public boolean removePoint( final IProfilPoint point );

  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException;

  public TRENNFLAECHEN_TYP getDeviderTyp( final DeviderKey deviderKey);

  public boolean setProfilMetaData( final METADATA metaDataKey, final Object data );

  public void setRauheitTyp( final RAUHEITEN_TYP r );
  
  public Object getExtendedPointData(ProfilPointProperty tableDataKey);
  
  public String getComment();
  
  public void setComment(final String comment);
  
  public void addCommentLine(final String line);
  
  public void setProfilData(ProfilPointProperty pointProperty,Object value);
  
  public void setProfilBuilding(IProfilBuilding profilBuilding);
  
  public IProfilBuilding removeProfilBuilding();
  
  public IProfilBuilding getProfilBuilding();

}