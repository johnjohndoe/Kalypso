package org.kalypso.model.wspm.core.profil.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.buildings.building.AbstractProfilBuilding;
import org.kalypso.model.wspm.core.profil.impl.devider.DeviderComparator;
import org.kalypso.model.wspm.core.profil.impl.devider.ProfilDevider;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoints;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;

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

  //private LinkedList<IProfilDevider.DEVIDER_TYP> m_visibleDevider = new LinkedList<IProfilDevider.DEVIDER_TYP>();

  private double m_station = Double.NaN;

  public PlainProfil( )
  {
    m_profilMetaData = new HashMap<Object, Object>();
    m_profilMetaData.put( PROFIL_PROPERTY.RAUHEIT_TYP,DEFAULT_RAUHEIT_TYP );
    m_points = new ProfilPoints();
    m_points.addProperty( POINT_PROPERTY.BREITE );
    m_points.addProperty( POINT_PROPERTY.HOEHE );
    m_building = null;
    //m_visibleDevider.addAll( Arrays.asList( IProfilDevider.DEVIDER_TYP.values() ) );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#addDevider(org.kalypso.model.wspm.core.profil.IProfilPoint,
   *      org.kalypso.model.wspm.core.profil.IProfil.DEVIDER_TYP)
   */
  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider )
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
   * @see org.kalypso.model.wspm.core.profil.IProfil#findNearestPoint(double)
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

  public IProfilPoint findNearestPoint( final double breite, final double hoehe, final POINT_PROPERTY property )
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
   * @see org.kalypso.model.wspm.core.profil.IProfil#findPoint(double, double)
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
    final IProfilPoint pkt = findNearestPoint( breite, hoehe, property );
    final Double delta = (Double) property.getParameter( PARAMETER.PRECISION );
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
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getBuilding()
   */
  public IProfilBuilding getBuilding( )
  {
    return m_building;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getDevider(DEVIDER_TYP[])
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
   * @throws ProfilDataException
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getValuesFor(org.kalypso.model.wspm.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException
  {
    final double[] values = new double[m_points.size()];
    int i = 0;
    for(IProfilPoint point : m_points )
    {
      values[i] = point.getValueFor( pointProperty );
      i++;
    }
    return values;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#insertPoint(org.kalypso.model.wspm.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException
  {
    final int index = m_points.indexOf( thePointBefore ) + 1;
    final IProfilPoint thePointNext = index == 0 ? null : m_points.get( index );
    final IProfilPoint point = ProfilUtil.splitSegment( thePointBefore, thePointNext );
    if( point == null )
      return addPoint( 0.0, 0.0 );
    m_points.add( index, point );
    return point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#addPoint(org.kalypso.model.wspm.core.profilinterface.IPoint,
   *      double, double)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite, final double hoehe )
  {
    final IProfilPoint point = m_points.addPoint( thePointBefore );
    point.setValueFor( POINT_PROPERTY.HOEHE, hoehe );
    point.setValueFor( POINT_PROPERTY.BREITE, breite );

    return point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfil#insertPoint(org.kalypso.model.wspm.core.profil.IProfilPoint,
   *      org.kalypso.model.wspm.core.profil.IProfilPoint)
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
    return (m_points.insertPoint( thePointBefore, point )!=null);
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

  /*
   * (non-Javadoc)
   * 
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
}