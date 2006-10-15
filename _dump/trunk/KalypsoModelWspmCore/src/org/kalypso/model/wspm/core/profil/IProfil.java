/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
   * gibt Null zur�ck wenn das profil nicht ge�ndert wurde TODO: wann ist dies der Fall?
   */
  public IProfilPoint addPoint( final double breite, final double hoehe );

  /**
   * @param pointProperty
   * @return POINT_PROPERTY[] with all current pointproperties
   */
  public POINT_PROPERTY[] addPointProperty( final POINT_PROPERTY pointProperty );

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
   *          Schl�sselwert einer Hashmap see IProfil.PROFIL_PROPERTY
   * @return Wert zu key oder null
   */
  public Object getProperty( Object key );

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
   * @return alle �briggebliebenen Eigenschaften
   */
  public boolean removePointProperty( final POINT_PROPERTY pointProperty );

  /**
   * @param key
   *          eine HashMap see IProfil.PROFIL_PROPERTY
   * @return den zugeh�rigen wert
   */
  public Object removeProperty( final Object key );

  /**
   * �ndert den Bauwerkstyp
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