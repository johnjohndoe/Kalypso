package com.bce.eind.core.profil;

import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public interface IPlainProfil
{
  public static enum RAUHEIT_TYP
  {
    ks, kst
  };
  public static enum PROFIL_PROPERTY
  {
    KOMMENTAR, MEHRFELDBRUECKE, METASTRINGS, STATION,RAUHEIT_TYP, STATUS, VERZWEIGUNGSKENNUNG, WASSERSPIEGEL
  }
  /**
   * @param point
   * @param devider
   * @return IProfilDevider
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider );

  /**
   * gibt Null zurück wenn das profil nicht geändert wurde
   */
  public IProfilPoint addPoint( final double breite, final double hoehe );

  /**
   * @param pointProperty
   * @return POINT_PROPERTY[] with all current pointproperties
   */
  public POINT_PROPERTY[] addPointProperty( final POINT_PROPERTY pointProperty );

  /**
   * sucht den nächsten Punkt bei breite ,findet aber auf jeden Fall den ersten Punkt in der Liste
   * als nächsten
   */
  public IProfilPoint findNearestPoint( final double breite );

  /**
   * sucht den nächsten Punkt dessen x-position näher als delta an breite ist, ansonsten Null
   */

  /**
   * @param breite
   * @param delta
   * @return
   */
  public IProfilPoint findPoint( final double breite, final double delta );
  
  /**
   * @param index
   * @param breite
   * @param delta
   * @return
   */
  public IProfilPoint findPoint(final int index,  final double breite, final double delta );

  /**
   * @return das aktuelle bauwerk oder Typ Building_typ NONE
   */
  public IProfilBuilding getBuilding( );

  /**
   * @param deviderTyp
   * @return IProfilDevider[] mit allen Trennern sortiert nach breite, oder null bei leerem array
   */
  public IProfilDevider[] getDevider( final DEVIDER_TYP deviderTyp );

  /**
   * @param deviderTypes
   * @return IProfilDevider[] mit allen Trennern sortiert nach breite, oder null bei leerem array
   */
  public IProfilDevider[] getDevider( final DEVIDER_TYP[] deviderTypes );

  /**
   * @param filterNonVisible
   * @return LinkedList<POINT_PROPERTY>
   */
  public LinkedList<POINT_PROPERTY> getPointProperties( final boolean filterNonVisible );

  /**
   * @return
   */
  public LinkedList<IProfilPoint> getPoints( );

  /**
   * @param key Schlüsselwert einer Hashmap see IPlainProfil.PROPERTY
   * @return Wert zu key oder null
   */
  public Object getProperty( Object key );

  /**
   * @param pointProperty
   * @return double[] with values of pointproperty sorted by breite
   * @throws ProfilDataException 
   */
  public double[] getValuesFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException;

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

  /**
   * Fügt einen bestehenden Punkt ein. return true wenn die Liste geändert wurde, sonst false.
   * @param thePointBefore
   * @param point
   * @return
   * @throws ProfilDataException
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException;

  /**
   * @return Den Punkt vor dem Verschieben
   * @throws ProfilDataException
   */
  public IProfilPoint moveDevider( final IProfilDevider devider, final IProfilPoint newPosition );

  /**
   * @return das entfernte Bauwerk
   * @throws ProfilDataException 
   */
  public IProfilBuilding removeBuilding( ) throws ProfilDataException;

  /**
   * @param devider
   * @return den entfernten Trenner
   */
  public IProfilDevider removeDevider( final IProfilDevider devider );

  /**
   * @param point to remove
    */
  public boolean removePoint( final IProfilPoint point );

  /**
   * @param pointProperty
   * @return alle übriggebliebenen Eigenschaften
   */
  public POINT_PROPERTY[] removePointProperty( final POINT_PROPERTY pointProperty );

  /**
   * @param key eine HashMap see IPlainProfil.PROPERTY
   * @return den zugehörigen wert
   */
  public Object removeProperty( final Object key );

  /**
   * ändert den Bauwerkstyp @see IPlainProfil.BUILDING_TYP
   * setzen der Eigenschaften @see IProfilBuilding.setValue
   * @param buildingTyp
   * @throws ProfilDataException
   */
  public void setBuilding( final BUILDING_TYP buildingTyp ) throws ProfilDataException;

  /**
   * @param key
   * @param value
   * @see PROFIL_PROPERTY
   */
  public void setProperty( final Object key, final Object value );

  public void setValueFor(IProfilPoint point,POINT_PROPERTY property, double value) throws ProfilDataException;
 
  public void setValueFor(IProfilDevider devider,Object property,Object value);
  
  public void setValueFor(final IProfilBuilding building,final BUILDING_PROPERTY property,final double value) throws ProfilBuildingException;
  
  
  public void setValues( final PointChange[] changes ) throws ProfilDataException;
  
  /**
   * @param pointList
   * @param pointProperty
   * @param value
   * @throws ProfilDataException
    */
  public void setValuesFor( final List<IProfilPoint> pointList, POINT_PROPERTY pointProperty,
      double value ) throws ProfilDataException;
  /**
   * @param pointProperty
   * @param value
   * @throws ProfilDataException
   * ändert alle Punkte des Profils @see setValuesFor( final List<IProfilPoint> pointList, POINT_PROPERTY pointProperty,
   *   double value )
   */
  public void setValuesFor( final POINT_PROPERTY pointProperty, final double value )
  throws ProfilDataException;
}