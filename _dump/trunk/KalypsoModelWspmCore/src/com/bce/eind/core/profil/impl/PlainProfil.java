package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilBuildingFactory;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.buildings.building.AbstractProfilBuilding;
import com.bce.eind.core.profil.impl.devider.DeviderComparator;
import com.bce.eind.core.profil.impl.devider.ProfilDevider;
import com.bce.eind.core.profil.impl.points.ProfilPoint;
import com.bce.eind.core.profil.impl.points.ProfilPoints;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class PlainProfil implements IProfilConstants, IProfil
{
  private IProfilBuilding m_building;

  private final ArrayList<IProfilDevider> m_devider = new ArrayList<IProfilDevider>();

  private final ProfilPoints m_points;

  private final HashMap<Object, Object> m_profilMetaData;

  public PlainProfil( )
  {
    m_profilMetaData = new HashMap<Object, Object>();
    m_points = new ProfilPoints();
    m_points.addProperty( POINT_PROPERTY.BREITE );
    m_points.addProperty( POINT_PROPERTY.HOEHE );
    m_building = ProfilBuildingFactory.createProfilBuilding( BUILDING_TYP.NONE );

  }

  /**
   * @see com.bce.eind.core.profil.IProfil#addDevider(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfil.DEVIDER_TYP)
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider )
  {
    IProfilDevider pd = new ProfilDevider( devider, point );
    m_devider.add( pd );

    return pd;
  }

  /**
   * @see com.bce.eind.core.profil.ProfilPoints#addPoint(double,double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
  {
    return m_points.addPoint( breite, hoehe );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#addPointProperty(com.bce.eind.core.profil.POINT_PROPERTY)
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
   * @see com.bce.eind.core.profil.IProfil#findNearestPoint(double)
   */
  public IProfilPoint findNearestPoint( final double breite )
  {
    IProfilPoint pkt = m_points.getFirst();

    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      try
      {
        IProfilPoint p = ptIt.next();
        if( Math.abs( pkt.getValueFor( POINT_PROPERTY.BREITE ) - breite ) > Math.abs( p
            .getValueFor( POINT_PROPERTY.BREITE )
            - breite ) )
          pkt = p;
      }
      catch( ProfilDataException e )
      {
        // sollte nie passieren da Breite immer vorhanden ist
      }
    }
    return pkt;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#findPoint(double, double)
   */
  public IProfilPoint findPoint( final double breite, final double delta )
  {
    final IProfilPoint pkt = findNearestPoint( breite );
    try
    {
      final double xpos = pkt.getValueFor( POINT_PROPERTY.BREITE );
      return (Math.abs( xpos - breite ) <= delta) ? pkt : null;
    }
    catch( ProfilDataException e1 )
    {
      // sollte nie passieren da Breite immer vorhanden ist
      return null;
    }
  }

  public IProfilPoint findPoint( int index, double breite, double delta )
  {

    if( (index >= m_points.size()) || (index < 0) )
      return findPoint( breite, delta );
    final IProfilPoint pkt = m_points.get( index );

    if( pkt == null )
      return findPoint( breite, delta );
    try
    {
      if( pkt.getValueFor( POINT_PROPERTY.BREITE ) == breite )
        return pkt;
    }
    catch( ProfilDataException e )
    {
      // // sollte nie passieren da Breite immer vorhanden ist
      return null;
    }
    return findPoint( breite, delta );

  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getBuilding()
   */
  public IProfilBuilding getBuilding( )
  {
    return m_building;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDevider(DEVIDER_TYP[])
   */
  public IProfilDevider[] getDevider( final DEVIDER_TYP[] deviderTypes )
  {
    final ArrayList<IProfilDevider> deviderList = new ArrayList<IProfilDevider>();
    for( IProfilDevider devider : m_devider )
    {
      for( DEVIDER_TYP deviderTyp : deviderTypes )
      {
        if( devider.getTyp() == deviderTyp )
        {
          deviderList.add( devider );
        }
      }

    }
    Collections.sort( deviderList, new DeviderComparator() );
    return deviderList.isEmpty() ? null : deviderList.toArray( new IProfilDevider[deviderList
        .size()] );
  }

  public IProfilDevider[] getDevider( DEVIDER_TYP deviderTyp )
  {
    return getDevider( new DEVIDER_TYP[]
    { deviderTyp } );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<POINT_PROPERTY> getPointProperties( final boolean filterNonVisible )
  {
    if( filterNonVisible )
      return m_points.getVisibleProperties();
    return m_points.getExistingProperties();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_points.unmodifiable();
  }

  public Object getProperty( Object key )
  {
    return m_profilMetaData.get( key );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException
  {
    final double[] values = new double[m_points.size()];
    int i = 0;
    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {

      values[i] = ptIt.next().getValueFor( pointProperty );

      i++;
    }
    return values;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#insertPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException
  {
    final int index = m_points.indexOf( thePointBefore ) + 1;
    final IProfilPoint thePointNext = m_points.get( index );
    final IProfilPoint point = ProfilUtil.splitSegment( thePointBefore, thePointNext );
    m_points.add( index, point );

    return point;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(com.bce.eind.core.profilinterface.IPoint,
   *      double, double)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe )
  {
    final ProfilPoint point = (ProfilPoint)m_points.addPoint( thePointBefore );
    point.setValueFor( POINT_PROPERTY.HOEHE, hoehe );
    point.setValueFor( POINT_PROPERTY.BREITE, breite );

    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#insertPoint(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException
  {
    final Collection<POINT_PROPERTY> newPP = point.getProperties();
    final Collection<POINT_PROPERTY> existingPP = point.getProperties();

    if( newPP.size() != existingPP.size() )
      return false;
    for( POINT_PROPERTY pp : newPP )
    {
      if( !existingPP.contains( pp ) )
        return false;
    }
    return m_points.insertPoint( thePointBefore, point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#moveDevider(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint moveDevider( IProfilDevider devider, IProfilPoint newPosition )
  {
    final IProfilPoint oldPkt = ((ProfilDevider)devider).setPoint( newPosition );

    return oldPkt;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#removeBuilding()
   */
  public IProfilBuilding removeBuilding( ) throws ProfilDataException
  {
    final IProfilBuilding oldBuilding = m_building;
    if( m_building instanceof AbstractProfilBuilding )
      ((AbstractProfilBuilding)m_building).removeProfilProperties( this );

    m_building = ProfilBuildingFactory.createProfilBuilding( BUILDING_TYP.NONE );

    return oldBuilding;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#removeDevider(com.bce.eind.core.profil.IProfilDevider)
   */
  public IProfilDevider removeDevider( IProfilDevider devider )
  {
    return m_devider.remove( devider ) ? devider : null;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IProfilPoint point )
  {
    return m_points.removePoint( point );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#removePointProperty(com.bce.eind.core.profil.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] removePointProperty( final POINT_PROPERTY pointProperty )
  {
    if( pointProperty == null )
      return null;
    final POINT_PROPERTY[] depending = m_points.getDependenciesFor( pointProperty );

    for( POINT_PROPERTY pp : depending )
    {
      m_points.removeProperty( pp );
    }
    return depending;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#removeProperty(java.lang.Object)
   */
  public Object removeProperty( Object key )
  {
    final Object value = m_profilMetaData.get( key );
    m_profilMetaData.remove( key );
    return value;

  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setBuilding(com.bce.eind.core.profil.IProfil.BUILDING_TYP)
   */
  public void setBuilding( final IProfilBuilding building ) throws ProfilDataException
  {
    removeBuilding();
    m_building = building;
    if( m_building instanceof AbstractProfilBuilding )
      ((AbstractProfilBuilding)m_building).addProfilProperties( this );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setProperty(java.lang.Object, java.lang.Object)
   */
  public void setProperty( Object key, Object value )
  {
    m_profilMetaData.put( key, value );

  }

  /**
   * @see com.bce.eind.core.profil.IProfil#isSpecialPoint(com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean isSpecialPoint( IProfilPoint point )
  {
    final IProfilDevider[] deviders = getDevider( DEVIDER_TYP.values() );
    for( IProfilDevider devider : deviders )
    {
      if( devider.getPoint() == point )
        return true;
    }
    return false;
  }
}