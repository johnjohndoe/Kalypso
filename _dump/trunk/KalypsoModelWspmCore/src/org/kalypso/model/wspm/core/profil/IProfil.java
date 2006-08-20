/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public interface IProfil
{
  public static enum RAUHEIT_TYP
  {
    ks,
    kst
  };

  public static enum WEHR_TYP
  {
    rundkronig,
    breitkronig,
    scharfkantig,
    Beiwert
  };

  public static enum PROFIL_PROPERTY
  {
    /** ObjectTyp=List<String> */
    KOMMENTAR,
    /** ObjectTyp=String */
    MEHRFELDBRUECKE,
    /** ObjectTyp=List<String> */
    METASTRINGS,
    /** ObjectTyp=RAUHEIT_TYP */
    RAUHEIT_TYP,
    /** ObjectTyp=String */
    STATUS,
    /** ObjectTyp=String */
    VERZWEIGUNGSKENNUNG,
    /** ObjectTyp=String */
    WASSERSPIEGEL
  }

  /**
   * @param point
   * @param devider
   * @return IProfilDevider
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider );

  public void addDevider( IProfilDevider devider );

  public POINT_PROPERTY[] getDependenciesFor( final POINT_PROPERTY property );

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
   * sucht den nächsten Punkt bei breite ,findet aber auf jeden Fall den ersten Punkt in der Liste als nächsten
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

  public IProfilPoint findPoint( final double breite, final double hoehe, final POINT_PROPERTY property );

  /**
   * @param index
   * @param breite
   * @param delta
   * @return
   */
  public IProfilPoint findPoint( final int index, final double breite, final double delta );

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

  public LinkedList<IProfilPoint> getPoints( );

  public IProfilPoints getProfilPoints( );

  /**
   * @param key
   *          Schlüsselwert einer Hashmap see IProfil.PROFIL_PROPERTY
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
   * Erzeugt einen neuen Punkt und fügt ihn in das Profil ein. Er wird genau in die Mitte des angegebenen Segments
   * gesetzt, seine Werte interpoliert oder fortgesetzt.
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException;

  /**
   * Fügt einen neuen Punkt ein. Wie {@link #insertPoint(IProfilPoint)}, nur Breite und Höhe sind bereits vorgegeben
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite, final double hoehe ) throws ProfilDataException;

  /**
   * Fügt einen bestehenden Punkt ein. return true wenn die Liste geändert wurde, sonst false.
   * 
   * @param thePointBefore
   * @param point
   * @return
   * @throws ProfilDataException
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point ) throws ProfilDataException;

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
   * @param point
   *          to remove
   */
  public boolean removePoint( final IProfilPoint point );

  /**
   * @param pointProperty
   * @return alle übriggebliebenen Eigenschaften
   */
  public boolean removePointProperty( final POINT_PROPERTY pointProperty );

  /**
   * @param key
   *          eine HashMap see IProfil.PROFIL_PROPERTY
   * @return den zugehörigen wert
   */
  public Object removeProperty( final Object key );

  /**
   * ändert den Bauwerkstyp
   * 
   * @see IProfil.BUILDING_TYP setzen der Eigenschaften
   * @see IProfilBuilding.setValue
   * @param buildingTyp
   * @throws ProfilDataException
   */
  public void setBuilding( final IProfilBuilding building ) throws ProfilDataException;

  /**
   * @param key
   * @param value
   * @throws ProfilDataException
   * @see PROFIL_PROPERTY
   */
  public void setProperty( final Object key, final Object value ) throws ProfilDataException;

  public void setActivePoint( final IProfilPoint point );

  /**
   * @return Returns the activePoint.
   */
  public IProfilPoint getActivePoint( );

  public POINT_PROPERTY getActiveProperty( );

  public void setActiveProperty( POINT_PROPERTY activeProperty );

  public void setStation( final double station );

  public double getStation( );
}