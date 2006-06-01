package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoints;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.PARAMETER;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.buildings.building.AbstractProfilBuilding;
import com.bce.eind.core.profil.impl.devider.DeviderComparator;
import com.bce.eind.core.profil.impl.devider.ProfilDevider;
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

  private final Map<Object, Object> m_profilMetaData;

  /**
   * Der aktive Punkt des Profils: in der Tabelle derjenige, auf welchem der Table-Cursor sitzt. Im Diagramm der zuletzt
   * angeklickte. Die sichtbaren Trenner werden auch hier verwaltet
   */
  private IProfilPoint m_activePoint;

  private POINT_PROPERTY m_activeProperty;

  private LinkedList<IProfilDevider.DEVIDER_TYP> m_visibleDevider = new LinkedList<IProfilDevider.DEVIDER_TYP>();

  public PlainProfil( )
  {
    m_profilMetaData = new HashMap<Object, Object>();
    m_points = new ProfilPoints();
    m_points.addProperty( POINT_PROPERTY.BREITE );
    m_points.addProperty( POINT_PROPERTY.HOEHE );
    m_building = null;
    m_visibleDevider.addAll( Arrays.asList( IProfilDevider.DEVIDER_TYP.values() ) );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#addDevider(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfil.DEVIDER_TYP)
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider )
  {
    IProfilDevider pd = new ProfilDevider( devider, point );
    addDevider( pd );

    return pd;
  }

  public void setDeviderVisibility( final IProfilDevider.DEVIDER_TYP deviderTyp, final boolean visible )
  {
    if( visible )
    {
      if( !m_visibleDevider.contains( deviderTyp ) )
      {
        m_visibleDevider.add( deviderTyp );
      }

    }
    else
      m_visibleDevider.remove( deviderTyp );
  }

  public boolean getDeviderVisibility( final IProfilDevider.DEVIDER_TYP deviderTyp )
  {

    return m_visibleDevider.contains( deviderTyp );

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
        if( Math.abs( pkt.getValueFor( POINT_PROPERTY.BREITE ) - breite ) > Math.abs( p.getValueFor( POINT_PROPERTY.BREITE ) - breite ) )
          pkt = p;
      }
      catch( ProfilDataException e )
      {
        // sollte nie passieren da Breite immer vorhanden ist
      }
    }
    return pkt;
  }

  public IProfilPoint findNearestPoint( final double breite, final double hoehe,final POINT_PROPERTY property )
  {
    IProfilPoint pkt = m_points.getFirst();

    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      try
      {
        IProfilPoint p = ptIt.next();
        if( (Math.abs( pkt.getValueFor( POINT_PROPERTY.BREITE ) - breite ) > Math.abs( p.getValueFor( POINT_PROPERTY.BREITE ) - breite ))
        || (Math.abs( pkt.getValueFor( property ) - hoehe ) > Math.abs( p.getValueFor( property ) - hoehe )) )
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

  public IProfilPoint findPoint( final double breite, final double hoehe, final POINT_PROPERTY property )
  {
    final IProfilPoint pkt = findNearestPoint( breite, hoehe,property );
    final Double delta = (Double)property.getParameter(PARAMETER.PRECISION);
    try
    {
      final double xpos = pkt.getValueFor( POINT_PROPERTY.BREITE );
      final double ypos = pkt.getValueFor( property );
      if( (Math.abs( xpos - breite ) <= delta) && (Math.abs( ypos - hoehe ) <= delta) )
      {
        return pkt;
      }
      else
        return null;

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
    return deviderList.isEmpty() ? null : deviderList.toArray( new IProfilDevider[deviderList.size()] );
  }

  public IProfilDevider[] getDevider( DEVIDER_TYP deviderTyp )
  {
    return getDevider( new DEVIDER_TYP[] { deviderTyp } );
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
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(com.bce.eind.core.profilinterface.IPoint, double, double)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite, final double hoehe )
  {
    final IProfilPoint point = m_points.addPoint( thePointBefore );
    point.setValueFor( POINT_PROPERTY.HOEHE, hoehe );
    point.setValueFor( POINT_PROPERTY.BREITE, breite );

    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#insertPoint(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point ) throws ProfilDataException
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
    final IProfilPoint oldPkt = ((ProfilDevider) devider).setPoint( newPosition );

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
      ((AbstractProfilBuilding) m_building).removeProfilProperties( this );

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
    for(IProfilDevider devider:m_devider)
    {
      if (devider.getPoint() == point) return false;
    }
    return m_points.removePoint( point );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#removePointProperty(com.bce.eind.core.profil.POINT_PROPERTY)
   */
  public boolean removePointProperty( final POINT_PROPERTY pointProperty )
  {

    if( pointProperty == null )
      return false;

    return m_points.removeProperty( pointProperty );

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfil#getDependenciesFor(com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] getDependenciesFor( POINT_PROPERTY property )
  {
    return m_points.getDependenciesFor( property );
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
    if( m_building != null )
      removeBuilding();
    m_building = building;
    if( m_building instanceof AbstractProfilBuilding )
      ((AbstractProfilBuilding) m_building).addProfilProperties( this );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setProperty(java.lang.Object, java.lang.Object)
   */
  public void setProperty( Object key, Object value )
  {
    m_profilMetaData.put( key, value );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#addDevider(com.bce.eind.core.profil.IProfilDevider)
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

}