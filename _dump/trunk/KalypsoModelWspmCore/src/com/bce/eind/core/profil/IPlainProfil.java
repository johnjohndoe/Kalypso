package com.bce.eind.core.profil;

import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfilConstants.DEVIDER_PROPERTY;

/**
 * @author kimwerner
 */
public interface IPlainProfil
{
  public static enum BUILDING_TYP
  {
    BRUECKE, EI, KREIS, MAUL, NONE, TRAPEZ, WEHR;
  }

  public static enum DEVIDER_TYP
  {
    BORDVOLL, DURCHSTROEMTE, FLIESSZONE, WEHR, PROFIL;
  }

  public static enum PROPERTY
  {
    KOMMENTAR, MEHRFELDBRUECKE, METASTRINGS, RAUHEIT_TYP, STATION, STATUS, VERZWEIGUNGSKENNUNG, WASSERSPIEGEL
  }

  /**
   * @param point
   * @param devider
   * @return IProfilDevider
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider );

  /**
   * gibt Null zur�ck wenn das profil nicht ge�ndert wurde
   */
  public IProfilPoint addPoint( final double breite, final double hoehe );

  /**
   * @param pointProperty
   * @return PointProperty[] with all current pointproperties
   */
  public PointProperty[] addPointProperty( final PointProperty pointProperty );

  /**
   * sucht den n�chsten Punkt bei breite ,findet aber auf jeden Fall den ersten Punkt in der Liste
   * als n�chsten
   */
  public IProfilPoint findNearestPoint( final double breite );

  /**
   * sucht den n�chsten Punkt dessen x-position n�her als delta an breite ist, ansonsten Null
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
   * @return LinkedList<PointProperty>
   */
  public LinkedList<PointProperty> getPointProperties( final boolean filterNonVisible );

  /**
   * @return
   */
  public LinkedList<IProfilPoint> getPoints( );

  /**
   * @param key Schl�sselwert einer Hashmap see IPlainProfil.PROPERTY
   * @return Wert zu key oder null
   */
  public Object getProperty( Object key );

  /**
   * @param pointProperty
   * @return double[] with values of pointproperty sorted by breite
   * @throws ProfilDataException 
   */
  public double[] getValuesFor( final PointProperty pointProperty ) throws ProfilDataException;

  /**
   * Erzeugt einen neuen Punkt und f�gt in in das Profil ein. Er wird genau in die Mitte des
   * angegebenen Segments gesetzt, seine Werte interpoliert oder fortgesetzt.
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException;

  /**
   * F�gt einen neuen Punkt ein. Wie {@link #insertPoint(IProfilPoint)}, nur Breite und H�he sind
   * bereits vorgegeben
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException;

  /**
   * F�gt einen bestehenden Punkt ein. return true wenn die Liste ge�ndert wurde, sonst false.
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
   */
  public IProfilBuilding removeBuilding( );

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
   * @return alle �briggebliebenen Eigenschaften
   */
  public PointProperty[] removePointProperty( final PointProperty pointProperty );

  /**
   * @param key eine HashMap see IPlainProfil.PROPERTY
   * @return den zugeh�rigen wert
   */
  public Object removeProperty( final Object key );

  /**
   * �ndert den Bauwerkstyp @see IPlainProfil.BUILDING_TYP
   * setzen der Eigenschaften @see IProfilBuilding.setValue
   * @param buildingTyp
   * @throws ProfilDataException
   */
  public void setBuilding( final IPlainProfil.BUILDING_TYP buildingTyp ) throws ProfilDataException;

  /**
   * @param key
   * @param value
   * @see PROPERTY
   */
  public void setProperty( final Object key, final Object value );

  public void setValueFor(IProfilPoint point,PointProperty property, double value) throws ProfilDataException;
 
  public void setValueFor(IProfilDevider devider,DEVIDER_PROPERTY property);
  
  public void setValues( final PointChange[] changes ) throws ProfilDataException;
  
  /**
   * @param pointList
   * @param pointProperty
   * @param value
   * @throws ProfilDataException
    */
  public void setValuesFor( final List<IProfilPoint> pointList, PointProperty pointProperty,
      double value ) throws ProfilDataException;
  /**
   * @param pointProperty
   * @param value
   * @throws ProfilDataException
   * �ndert alle Punkte des Profils @see setValuesFor( final List<IProfilPoint> pointList, PointProperty pointProperty,
   *   double value )
   */
  public void setValuesFor( final PointProperty pointProperty, final double value )
  throws ProfilDataException;
}