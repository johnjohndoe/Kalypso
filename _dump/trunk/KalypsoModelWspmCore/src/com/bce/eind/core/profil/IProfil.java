package com.bce.eind.core.profil;

/*
 * Created on 21.12.2004
 *
 */

import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.impl.devider.DeviderKey;

/**
 * @author kimwerner
 */
public interface IProfil
{
  public static enum BUILDING_TYP
  {
    BLD_BRUECKE, BLD_EI, BLD_KREIS, BLD_MAUL, BLD_NONE, BLD_TRAPEZ
  }

  public static enum BUILDING_VALUES
  {
    CENTER_X, CENTER_Y, RADIUS
  }

  public static enum METADATA
  {
    KOMMENTAR, MEHRFELDBRUECKE, METASTRINGS, STATION, STATUS, VERZWEIGUNGSKENNUNG, WASSERSPIEGEL
  }

  public static enum POINT_PROPERTY
  {
    BEWUCHS_AX, BEWUCHS_AY, BEWUCHS_DP, BORDVOLL, BREITE, DURCHSTROEMTE, HOCHWERT, HOEHE, OBERKANTEBRUECKE, RAUHEIT, RECHTSWERT, TRENNFLAECHE, UNTERKANTEBRUECKE
  }

  public static enum PROFILDATA_TYP
  {
    RAUHEIT
  }

  public static enum RAUHEITEN_TYP
  {
    RAUHEIT_KS, RAUHEIT_KST, RAUHEIT_UNDEFINED
  }

  public static enum TRENNFLAECHEN_TYP
  {
    TRENNFLAECHE_UNDEFINED, TRENNFLAECHE_BOESCHUNG, TRENNFLAECHE_SOHLE
  }

  public static final DeviderKey BORDVOLL_L = new DeviderKey( POINT_PROPERTY.BORDVOLL, -1 );

  public static final DeviderKey BORDVOLL_R = new DeviderKey( POINT_PROPERTY.BORDVOLL, 1 );

  public static final DeviderKey DURCHSTROEMTE_L = new DeviderKey( POINT_PROPERTY.DURCHSTROEMTE, -1 );

  public static final DeviderKey DURCHSTROEMTE_R = new DeviderKey( POINT_PROPERTY.DURCHSTROEMTE, 1 );

  public static final DeviderKey TRENNFLAECHE_L = new DeviderKey( POINT_PROPERTY.TRENNFLAECHE, -1 );

  public static final DeviderKey TRENNFLAECHE_R = new DeviderKey( POINT_PROPERTY.TRENNFLAECHE, 1 );

  public void addCommentLine( final String line );

  public IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException;

  public void addProfilListener( final ProfilListener pl );

  public void addProfilMetaData( final METADATA metaDataKey, final Object data );

  public boolean addProfilPointProperty( final POINT_PROPERTY pointProperty );

  public void addUnknownObject( final Object unknownData );

  public String getComment( );

  public IProfilPoint getDevider( final DeviderKey deviderKey );

  public TRENNFLAECHEN_TYP getDeviderTyp( final DeviderKey deviderKey );

  public IProfilPoint getNextPoint( IProfilPoint point ) throws ProfilDataException;

  public IProfilPoint getPoint( final double breite, final double hoehe );

  public IProfilPoint getPoint( final int index );

  public IProfilPointProperty getPointProperty( final POINT_PROPERTY pointProperty );

  public List<IProfilPoint> getPoints( );

  public List<IProfilPoint> getPoints( final IProfilPoint startPoint );

  public LinkedList<IProfilPoint> getPointsAtPos( final double breite );

  public int getPointsCount( );

  public IProfilPoint getPreviousPoint( IProfilPoint point ) throws ProfilDataException;

  public IProfilBuilding getProfilBuilding( );

  public Object getProfilMetaData( METADATA metaData );

  public LinkedList<POINT_PROPERTY> getProfilPointProperties( final boolean filterNonVisible );

  public RAUHEITEN_TYP getRauheitTyp( );

  public List getUnknownObjects( );

  public double[] getValuesFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException;

  public int indexOf( final IProfilPoint point );

  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException;

  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException;

  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException;

  public void moveDevider( final DeviderKey deviderKey, final IProfilPoint newPosition )
      throws ProfilDataException;

  public boolean removePoint( final IProfilPoint point );

  public IProfilBuilding removeProfilBuilding( );

  public void removeProfilListener( final ProfilListener pl );

  public boolean removeProfilMetaData( final METADATA metaData );

  public boolean removeProfilPointProperty( final POINT_PROPERTY pointProperty );

  public void setComment( final String comment );

  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final POINT_PROPERTY pointProperty ) throws ProfilDataException;

  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException;

  public void setProfilBuilding( final IProfilBuilding profilBuilding );

  public boolean setProfilMetaData( final METADATA metaDataKey, final Object data );

  public void setRauheitTyp( final RAUHEITEN_TYP r );

  public boolean setValueFor( IProfilPoint point, POINT_PROPERTY pointProperty, double value )
      throws ProfilDataException;

}