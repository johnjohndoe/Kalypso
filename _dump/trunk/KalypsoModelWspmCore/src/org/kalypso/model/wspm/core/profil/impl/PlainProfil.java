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
package org.kalypso.model.wspm.core.profil.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.buildings.building.AbstractProfilBuilding;
import org.kalypso.model.wspm.core.profil.impl.devider.DeviderComparator;
import org.kalypso.model.wspm.core.profil.impl.devider.ProfilDevider;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoints;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class PlainProfil implements IProfilConstants, IProfil
{
  private IProfilBuilding m_building;

  private final ArrayList<IProfilDevider> m_devider = new ArrayList<IProfilDevider>();

  private final ProfilPoints m_points;

  private final Map<Object, Object> m_profilMetaData;

  /**
   * Der aktive Punkt des Profils: in der Tabelle derjenige, auf welchem der Table-Cursor sitzt. Im Diagramm der zuletzt
   * angeklickte. Die sichtbaren Trenner werden auch hier verwaltet
   */
  private IProfilPoint m_activePoint;

  private POINT_PROPERTY m_activeProperty;

  private double m_station = Double.NaN;

  private final String m_type;

  public PlainProfil( final String type )
  {
    m_type = type;
    m_profilMetaData = new HashMap<Object, Object>();
    m_profilMetaData.put( IProfilConstants.RAUHEIT_TYP, DEFAULT_RAUHEIT_TYP );
    m_points = new ProfilPoints();
    m_points.addProperty( POINT_PROPERTY.BREITE );
    m_points.addProperty( POINT_PROPERTY.HOEHE );
    m_building = null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#addDevider(org.kalypso.model.wspm.core.profil.IProfilPoint,
   *      org.kalypso.model.wspm.core.profil.IProfil.DEVIDER_TYP)
   */
  public IProfilDevider addDevider( IProfilPoint point, String devider )
  {
    IProfilDevider pd = new ProfilDevider( devider, point );
    addDevider( pd );

    return pd;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.ProfilPoints#addPoint(double,double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
  {
    return m_points.addPoint( breite, hoehe );
  }

  /**
   * @return ein Array aller von der eigefügten PoinbtProperty abhängigen pointProperties
   * @see org.kalypso.model.wspm.core.profil.IProfil#addPointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] addPointProperty( final POINT_PROPERTY pointProperty )
  {
    if( pointProperty == null )
      return null;

    final POINT_PROPERTY[] depending = m_points.getDependenciesFor( pointProperty );

    for( POINT_PROPERTY pd : depending )
      m_points.addProperty( pd );

    return depending;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getBuilding()
   */
  public IProfilBuilding getBuilding( )
  {
    return m_building;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getDevider(DEVIDER_TYP[])
   */
  public IProfilDevider[] getDevider( final String[] deviderTypes )
  {
    final ArrayList<IProfilDevider> deviderList = new ArrayList<IProfilDevider>();
    for( IProfilDevider devider : m_devider )
    {
      for( String deviderTyp : deviderTypes )
      {
        if( deviderTyp.compareTo( devider.getTyp() ) == 0 )
        {
          deviderList.add( devider );
        }
      }
    }

    Collections.sort( deviderList, new DeviderComparator() );
    return  deviderList.toArray( new IProfilDevider[0] );
  }

  public IProfilDevider[] getDevider( )
  {
    final ArrayList<IProfilDevider> deviderList = new ArrayList<IProfilDevider>( m_devider );
    Collections.sort( deviderList, new DeviderComparator() );
    return deviderList.toArray( new IProfilDevider[0] );
  }

  public IProfilDevider[] getDevider( final IProfilPoint point )
  {
    final ArrayList<IProfilDevider> deviderList = new ArrayList<IProfilDevider>();
    for( IProfilDevider devider : m_devider )
    {
      if( devider.getPoint() == point )
        deviderList.add( devider );
    }
    return deviderList.toArray( new IProfilDevider[0] );
  }

  public IProfilDevider[] getDevider( String deviderTyp )
  {
    return getDevider( new String[] { deviderTyp } );
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<POINT_PROPERTY> getPointProperties( final boolean filterNonVisible )
  {
    if( filterNonVisible )
      return m_points.getVisibleProperties();
    return m_points.getExistingProperties();
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getPoints()
   */
  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_points.getPoints();
  }

  public IProfilPoints getProfilPoints( )
  {
    return m_points;
  }

  public Object getProperty( Object key )
  {
    return m_profilMetaData.get( key );
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#moveDevider(org.kalypso.model.wspm.core.profildata.tabledata.DeviderKey,
   *      org.kalypso.model.wspm.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint moveDevider( IProfilDevider devider, IProfilPoint newPosition )
  {
    final IProfilPoint oldPkt = ((ProfilDevider) devider).setPoint( newPosition );

    return oldPkt;
  }

  /**
   * @throws ProfilDataException
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#removeBuilding()
   */
  public IProfilBuilding removeBuilding( ) throws ProfilDataException
  {
    final IProfilBuilding oldBuilding = m_building;
    if( m_building instanceof AbstractProfilBuilding )
      ((AbstractProfilBuilding) m_building).removeProfilProperties( this );

    return oldBuilding;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeDevider(org.kalypso.model.wspm.core.profil.IProfilDevider)
   */
  public IProfilDevider removeDevider( IProfilDevider devider )
  {
    return m_devider.remove( devider ) ? devider : null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#removePoint(org.kalypso.model.wspm.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IProfilPoint point )
  {
    for( IProfilDevider devider : m_devider )
    {
      if( devider.getPoint() == point )
        return false;
    }
    return m_points.removePoint( point );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removePointProperty(org.kalypso.model.wspm.core.profil.POINT_PROPERTY)
   */
  public boolean removePointProperty( final POINT_PROPERTY pointProperty )
  {

    if( pointProperty == null )
      return false;

    return m_points.removeProperty( pointProperty );

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getDependenciesFor(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] getDependenciesFor( POINT_PROPERTY property )
  {
    return m_points.getDependenciesFor( property );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#removeProperty(java.lang.Object)
   */
  public Object removeProperty( Object key )
  {
    final Object value = m_profilMetaData.get( key );
    m_profilMetaData.remove( key );
    return value;

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setBuilding(org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP)
   */
  public void setBuilding( final IProfilBuilding building ) throws ProfilDataException
  {
    if( m_building != null )
      removeBuilding();
    m_building = building;
    if( m_building instanceof AbstractProfilBuilding )
      ((AbstractProfilBuilding) m_building).addProfilProperties( this );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setProperty(java.lang.Object, java.lang.Object)
   */
  public void setProperty( Object key, Object value )
  {
    m_profilMetaData.put( key, value );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#addDevider(org.kalypso.model.wspm.core.profil.IProfilDevider)
   */
  public void addDevider( IProfilDevider devider )
  {
    m_devider.add( devider );
  }

  public POINT_PROPERTY getActiveProperty( )
  {
    return m_activeProperty;
  }

  public void setActiveProperty( POINT_PROPERTY activeProperty )
  {
    m_activeProperty = activeProperty;
  }

  public void setActivePoint( final IProfilPoint point )
  {
    m_activePoint = point;
  }

  /**
   * @return Returns the activePoint.
   */
  public IProfilPoint getActivePoint( )
  {
    return m_activePoint;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#setStation(double)
   */
  public void setStation( final double station )
  {
    m_station = station;
  }

  public double getStation( )
  {
    return m_station;
  }
  
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#getType()
   */
  public String getType( )
  {
    return m_type;
  }
}