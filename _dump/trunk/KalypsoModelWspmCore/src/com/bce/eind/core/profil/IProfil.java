package com.bce.eind.core.profil;

/*
 * Created on 21.12.2004
 *
 */

import java.util.LinkedList;
import java.util.List;

/**
 * @author kimwerner
 */
public interface IProfil
{
  public static enum BUILDING_TYP
  {
    NONE, BRUECKE, EI, KREIS, MAUL, TRAPEZ
  }

  /*
   * public static enum BUILDING_VALUES {
   * BEZUGSPUNKT_X,BEZUGSPUNKT_Y,STEIGUNG,SOHLGEFAELLE,HOEHE,BREITE }
   */
  public static enum METADATA
  {
    KOMMENTAR, MEHRFELDBRUECKE, METASTRINGS, STATION, STATUS, VERZWEIGUNGSKENNUNG, WASSERSPIEGEL
  }

  public static enum PROFILDATA_TYP
  {
    RAUHEIT
  }

  public static enum RAUHEITEN_TYP
  {
    UNDEFINED, KS, KST
  }

  public static enum TRENNFLAECHEN_TYP
  {
    UNDEFINED, BOESCHUNG, SOHLE
  }

  public void addCommentLine( final String line );

  public IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException;

  public void addProfilListener( final ProfilListener pl );

  public void addProfilMetaData( final METADATA metaDataKey, final Object data );

  public boolean addProfilPointProperty( final ProfilPointProperty pointProperty );

  public void addUnknownObject( final Object unknownData );

  public String getComment( );

  public IProfilPoint getDevider( final DeviderKey deviderKey );

  public TRENNFLAECHEN_TYP getDeviderTyp( final DeviderKey deviderKey );

  public IProfilPoint getNextPoint( IProfilPoint point ) throws ProfilDataException;

  public IProfilPoint getPoint( final double breite, final double hoehe );

  public IProfilPoint getPoint( final int index );

  public List<IProfilPoint> getPoints( );

  public List<IProfilPoint> getPoints( final IProfilPoint startPoint );
  
  public List<IProfilPoint> getPoints( final IProfilPoint startPoint, final IProfilPoint endPoint );

  public LinkedList<IProfilPoint> getPointsAtPos( final double breite );

  public int getPointsCount( );

  public IProfilPoint getPreviousPoint( IProfilPoint point ) throws ProfilDataException;

  public IProfilBuilding getProfilBuilding( );

  public Object getProfilMetaData( METADATA metaData );

  public LinkedList<ProfilPointProperty> getProfilPointProperties( final boolean filterNonVisible );

  public RAUHEITEN_TYP getRauheitTyp( );

  public List<Object> getUnknownObjects( );

  public double[] getValuesFor( final ProfilPointProperty pointProperty )
      throws ProfilDataException;

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

  public boolean removeProfilPointProperty( final ProfilPointProperty pointProperty );

  public void setComment( final String comment );

  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final ProfilPointProperty pointProperty ) throws ProfilDataException;

  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException;

  public void setProfilBuilding( final IProfil.BUILDING_TYP buildingTyp );

  public boolean setProfilMetaData( final METADATA metaDataKey, final Object data );

  public void setRauheitTyp( final RAUHEITEN_TYP r );

  public boolean setValueFor( IProfilPoint point, ProfilPointProperty pointProperty, double value )
      throws ProfilDataException;
  public boolean editBuilding(final ProfilBuildingProperty buildingProperty,final double value) throws ProfilBuildingException;
  public void  setValuesFor(List<IProfilPoint> pointList, ProfilPointProperty pointProperty, double value )
  throws ProfilDataException;

}