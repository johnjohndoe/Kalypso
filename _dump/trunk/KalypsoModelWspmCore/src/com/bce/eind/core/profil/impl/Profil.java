package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointChange;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.DEVIDER_PROPERTY;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class Profil implements IProfil
{
  private final List<IProfilListener> m_listeners = new ArrayList<IProfilListener>( 10 );

  private PlainProfil m_profil = new PlainProfil();

  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider )
  {

    final IProfilDevider result = m_profil.addDevider( point, devider );
    fireDeviderChanged( point, result );
    return result;

  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(double, double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
  {
    final IProfilPoint addPoint = m_profil.addPoint( breite, hoehe );

    firePointsAdded( new IProfilPoint[]
    { addPoint } );

    return addPoint;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public PointProperty[] addPointProperty( final PointProperty pointProperty )
  {
    final PointProperty[] newProperties = m_profil.addPointProperty( pointProperty );

    firePointPropertiesAdded( newProperties );

    return newProperties;
  }

  public void addProfilListener( final IProfilListener pl )
  {
    m_listeners.add( pl );
  }

  public IProfilPoint findNearestPoint( final double breite )
  {
    return m_profil.findNearestPoint( breite );
  }

  public IProfilPoint findPoint( final double breite, final double delta )
  {
    return m_profil.findPoint( breite, delta );
  }

  public IProfilPoint findPoint( int index, double breite, double delta )
  {
    return m_profil.findPoint( index, breite, delta );
  }

  public void fireBuildingChanged( )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingChanged();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(double, double)
   */
  /*
   * public IProfilPoint getPoint( final double breite, final double hoehe ) { return
   * m_profil.getPoint( breite, hoehe ); }
   */

  public void fireBuildingDataChanged( final IProfilBuilding building,
      final ProfilBuildingProperty buildingProperty, final double value )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingDataChanged( building, buildingProperty, value );
  }

  public void fireCommentChanged( )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onCommentChanged();
  }

  public void fireDeviderChanged( final IProfilPoint point, final IProfilDevider devider )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onDeviderChanged( point, devider );
  }

  public void fireMetaDataChanged( final Object key, final Object value )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onMetaDataChanged( key, value );
  }

  public void firePointPropertiesAdded( final PointProperty[] addedProperties )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointPropertiesAdded( addedProperties );
  }

  public void firePointPropertiesRemoved( final PointProperty[] removeProperties )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointPropertiesRemoved( removeProperties );
  }

  public void firePointsAdded( final IProfilPoint[] newPoints )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointsAdded( newPoints );
  }

  public void firePointsRemoved( final IProfilPoint[] removedPoints )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointsRemoved( removedPoints );
  }

  public void firePointValuesChanged( final PointChange[] changes )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointValuesChanged( changes );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getBuilding()
   */
  public IProfilBuilding getBuilding( )
  {
    return m_profil.getBuilding();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDevider(DEVIDER_TYP[])
   */
  public IProfilDevider[] getDevider( DEVIDER_TYP[] deviderTypes )
  {
    return m_profil.getDevider( deviderTypes );
  }

  public IProfilDevider[] getDevider( DEVIDER_TYP deviderTyp )
  {
    return m_profil.getDevider( deviderTyp );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<PointProperty> getPointProperties( final boolean filterNonVisible )
  {
    return m_profil.getPointProperties( filterNonVisible );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_profil.getPoints();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getMetaData()
   */
  public Object getProperty( Object key )
  {
    return m_profil.getProperty( key );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( final PointProperty pointProperty ) throws ProfilDataException
  {
    return m_profil.getValuesFor( pointProperty );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#insertPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException
  {
    final IProfilPoint point = m_profil.insertPoint( thePointBefore );

    firePointsAdded( new IProfilPoint[]
    { point } );

    return point;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(com.bce.eind.core.profilinterface.IPoint,
   *      double, double)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException
  {
    final IProfilPoint point = m_profil.insertPoint( thePointBefore, breite, hoehe );

    firePointsAdded( new IProfilPoint[]
    { point } );

    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#insertPoint(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException
  {
    final boolean result = m_profil.insertPoint( thePointBefore, point );

    firePointsAdded( new IProfilPoint[]
    { point } );

    return result;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#moveDevider(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint moveDevider( final IProfilDevider devider, final IProfilPoint newPosition )
  {
    final IProfilPoint oldPkt = m_profil.moveDevider( devider, newPosition );

    fireDeviderChanged( newPosition, devider );

    return oldPkt;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeBuilding()
   */
  public IProfilBuilding removeBuilding( )
  {
    final IProfilBuilding building = m_profil.removeBuilding();

    fireBuildingChanged();

    return building;
  }

  public IProfilDevider removeDevider( IProfilDevider devider )
  {
    IProfilDevider removedDevider = m_profil.removeDevider( devider );
    if( removedDevider != null )
      fireDeviderChanged( null, removedDevider );
    return removedDevider;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IProfilPoint point )
  {
    final boolean result = m_profil.removePoint( point );

    firePointsRemoved( new IProfilPoint[]
    { point } );

    return result;
  }

  /**
   * @return
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public PointProperty[] removePointProperty( final PointProperty pointProperty )
  {
    final PointProperty[] removeProperties = m_profil.removePointProperty( pointProperty );

    firePointPropertiesRemoved( removeProperties );

    return removeProperties;
  }

  public void removeProfilListener( final IProfilListener pl )
  {
    m_listeners.remove( pl );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeExtendedData(java.lang.String)
   */
  public Object removeProperty( final Object key )
  {
    final Object result = m_profil.removeProperty( key );

    fireMetaDataChanged( key, null );
    return result;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#setBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setBuilding( final IProfil.BUILDING_TYP buildingTyp ) throws ProfilDataException
  {
    m_profil.setBuilding( buildingTyp );

    fireBuildingChanged();
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setProperty(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.Object)
   */
  public void setProperty( final Object key, final Object value )
  {
    m_profil.setProperty( key, value );

    fireMetaDataChanged( key, value );
  }

  public void setValueFor( final IProfilPoint point, final PointProperty pointProperty,
      final double value ) throws ProfilDataException
  {
    setValues( new PointChange[]
    { new PointChange( point, pointProperty, value ) } );
  }

  /** Interne Methode die wirklich die Daten ändert. Schickt KEINEN event ! */
  public void setValues( final PointChange[] changes ) throws ProfilDataException
  {
    m_profil.setValues( changes );

    firePointValuesChanged( changes );
  }

  public void setValuesFor( final List<IProfilPoint> pointList, PointProperty pointProperty,
      double value ) throws ProfilDataException
  {
    final List<PointChange> changes = new ArrayList<PointChange>( pointList.size() );
    for( final IProfilPoint point : pointList )
      changes.add( new PointChange( point, pointProperty, value ) );

    setValues( changes.toArray( new PointChange[changes.size()] ) );
  }

  public void setValuesFor( PointProperty pointProperty, double value ) throws ProfilDataException
  {
    final List<IProfilPoint> allPoints = getPoints();
    setValuesFor( allPoints, pointProperty, value );

  }

  public void setValueFor( IProfilDevider devider, Object property ,Object value)
  {
    devider.setValueFor(property,value );
    fireDeviderChanged( null, devider );

  }

}