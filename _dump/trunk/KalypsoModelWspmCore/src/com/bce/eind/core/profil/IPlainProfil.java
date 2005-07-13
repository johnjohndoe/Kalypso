package com.bce.eind.core.profil;

import java.util.LinkedList;
import java.util.List;

/**
 * @author kimwerner
 */
public interface IPlainProfil
{
  public static enum BUILDING_TYP
  {
    NONE, BRUECKE, EI, KREIS, MAUL, TRAPEZ;
  }

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
    UNDEFINED, ks, KST
  }

  public static enum TRENNFLAECHEN_TYP
  {
    UNDEFINED, BOESCHUNG, SOHLE
  }

  public IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException;

  public void addProfilMetaData( final METADATA metaDataKey, final Object data );

  public ProfilPointProperty[] addProfilPointProperty( final ProfilPointProperty pointProperty )
      throws ProfilDataException;

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

  public IProfilPoint getPreviousPoint( final IProfilPoint point ) throws ProfilDataException;

  public IProfilBuilding getProfilBuilding( );

  public Object getProfilMetaData( METADATA metaData );

  public LinkedList<ProfilPointProperty> getProfilPointProperties( final boolean filterNonVisible );

  public RAUHEITEN_TYP getRauheitTyp( );

  public List<Object> getUnknownObjects( );

  public double[] getValuesFor( final ProfilPointProperty pointProperty )
      throws ProfilDataException;

  public int indexOf( final IProfilPoint point );

  /**
   * Erzeugt einen neuen Punkt und fügt in in das Profil ein. Er wird genau in die Mitte des
   * angegebenen Segments gesetzt, seine Werte interpoliert oder fortgesetzt.
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException;

  /**
   * Fügt einen neuen Punkt ein. Wie {@link #insertPoint(IProfilPoint)}, nur Breite und Höhe sind
   * bereits vorgegeben
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException;

  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException;

  /**
   * @return true, falls die Daten geändert wurden; false, falls der devider eh schon auf diesem
   *         Punkt lag
   * @throws ProfilDataException
   */
  public boolean moveDevider( final DeviderKey deviderKey, final IProfilPoint newPosition )
      throws ProfilDataException;

  public void removePoint( final IProfilPoint point ) throws ProfilDataException;

  public IProfilBuilding removeProfilBuilding( );

  public void removeProfilMetaData( final METADATA metaData );

  public ProfilPointProperty[] removeProfilPointProperty( final ProfilPointProperty pointProperty );

  public void setComment( final String comment );

  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final ProfilPointProperty pointProperty ) throws ProfilDataException;

  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException;

  public void setProfilBuilding( final IPlainProfil.BUILDING_TYP buildingTyp )
      throws ProfilDataException;

  public void setProfilMetaData( final METADATA metaDataKey, final Object data );

  public void setRauheitTyp( final RAUHEITEN_TYP r );

  public void setValues( final ProfilChange[] changes ) throws ProfilDataException;

  public void editBuilding( final ProfilBuildingProperty buildingProperty, final double value )
      throws ProfilBuildingException;
  public IProfilPoint getPointNearBy( final double breite, final double delta ) throws ProfilDataException;
  public IProfilPoint getPointCloseTo( final double breite ) throws ProfilDataException;
}