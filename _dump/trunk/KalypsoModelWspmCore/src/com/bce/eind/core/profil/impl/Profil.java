package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.changes.AbstractChange;
import com.bce.eind.core.profil.changes.BuildingAdd;
import com.bce.eind.core.profil.changes.BuildingChange;
import com.bce.eind.core.profil.changes.DeviderChange;
import com.bce.eind.core.profil.changes.PointChange;
import com.bce.eind.core.profil.changes.ProfilChange;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class Profil implements IProfil, IProfilConstants
{
  private final List<IProfilListener> m_listeners = new ArrayList<IProfilListener>( 10 );

  private PlainProfil m_profil = new PlainProfil();

  public IProfilDevider addDevider( IProfilPoint point, DEVIDER_TYP devider )
  {

    final IProfilDevider result = m_profil.addDevider( point, devider );
    fireDeviderAdded(      new DeviderChange( null, null, devider ) );

    return result;

  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(double, double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
  {
    final IProfilPoint addedPoint = m_profil.addPoint( breite, hoehe );

    firePointsAdded(  new PointChange( addedPoint, null, breite ) );

    return addedPoint;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public POINT_PROPERTY[] addPointProperty( final POINT_PROPERTY pointProperty )
  {
    final POINT_PROPERTY[] newProperties = m_profil.addPointProperty( pointProperty );
    final PointChange[] changes = new PointChange[newProperties.length];
    for( int i = 0; i < changes.length; i++ )
    {
      changes[i] = new PointChange( null, newProperties[i], 0.0 );
    }
    firePointPropertiesAdded( changes[0] );

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

  public void fireBuildingAdded( final BuildingChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingAdded( change );
  }

  public void fireBuildingChanged( BuildingChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingChanged( change );
  }

  public void fireBuildingRemoved( BuildingChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingRemoved( change );
  }

  public void fireDeviderChanged( final DeviderChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onDeviderChanged( change );
  }

  public void fireDeviderAdded( final DeviderChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onDeviderAdded( change );
  }

  public void fireDeviderRemoved( final DeviderChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onDeviderRemoved( change );
  }

  public void fireProfilDataChanged( final ProfilChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onProfilDataChanged( change );
  }

  public void firePointPropertiesAdded( final PointChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointPropertiesAdded( change );
  }

  public void firePointPropertiesRemoved( final PointChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointPropertiesRemoved( change );
  }

  public void firePointsAdded( final PointChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointsAdded( change );
  }

  public void firePointsRemoved( final PointChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointsRemoved( change );
  }

  public void firePointChanged( final PointChange change )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointsChanged( change );
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
  public LinkedList<POINT_PROPERTY> getPointProperties( final boolean filterNonVisible )
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
  public double[] getValuesFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException
  {
    return m_profil.getValuesFor( pointProperty );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#insertPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException
  {
    final IProfilPoint point = m_profil.insertPoint( thePointBefore );

    firePointsAdded( new PointChange( point, null, null )  );

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
    firePointsAdded(  new PointChange( point, null, null ) );

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

    firePointsAdded( new PointChange( point, null, null )  );

    return result;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#moveDevider(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint moveDevider( final IProfilDevider devider, final IProfilPoint newPosition )
  {
    final IProfilPoint oldPkt = m_profil.moveDevider( devider, newPosition );

    fireDeviderChanged( new DeviderChange( devider, null, newPosition )  );

    return oldPkt;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeBuilding()
   */
  public IProfilBuilding removeBuilding( )
  {
    final IProfilBuilding building;
    try
    {
      building = m_profil.removeBuilding();
      fireBuildingRemoved( new BuildingChange( building, null, null )  );

      return building;
    }
    catch( ProfilDataException e )
    {
      return null;
    }

  }

  public IProfilDevider removeDevider( IProfilDevider devider )
  {
    IProfilDevider removedDevider = m_profil.removeDevider( devider );
    if( removedDevider != null )
      fireDeviderRemoved( new DeviderChange( removedDevider, null, null )  );
    return removedDevider;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public boolean removePoint( final IProfilPoint point )
  {
    final boolean result = m_profil.removePoint( point );
    if( result )
    {
      firePointsRemoved(  new PointChange( point, null, null )  );
    }
    return result;
  }

  /**
   * @return
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public POINT_PROPERTY[] removePointProperty( final POINT_PROPERTY pointProperty )
  {
    final POINT_PROPERTY[] removedProperties = m_profil.removePointProperty( pointProperty );
    final PointChange[] changes = new PointChange[removedProperties.length];
    for( int i = 0; i < changes.length; i++ )
    {
      changes[i] = new PointChange( null, removedProperties[i], null );
    }
    firePointPropertiesAdded( changes[0] );

    return removedProperties;
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
    final Object result = m_profil.getProperty( key );

    //TODO KIM event fireProfilDataChanged(  new ProfilChange( m_profil, key, null )  );
    return result;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#setBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setBuilding( final BUILDING_TYP buildingTyp ) throws ProfilDataException
  {

    final BuildingAdd change = new BuildingAdd(buildingTyp );
    change.doChange(m_profil);
    fireEvents(new AbstractChange[]{change});
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.IProfil#setProperty(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.Object)
   */
  public void setProperty( final Object key, final Object value ) throws ProfilDataException
  {

    m_profil.setProperty( key, value );

//  TODO KIM event fireProfilDataChanged(  new ProfilChange( m_profil, key, value )  );
  }

  public void setValueFor( final IProfilPoint point, final POINT_PROPERTY pointProperty,
      final double value ) throws ProfilDataException
  {

    m_profil.setValueFor( point, pointProperty, value );
    firePointChanged( new PointChange( point, pointProperty, value )  );
  }

  public void fireEvents( AbstractChange[] changes )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
    {
      for( AbstractChange change : changes )
      {
        change.fireEvent( l );
      }
    }

  }

  /** Interne Methode die wirklich die Daten ändert. Schickt KEINEN event ! */
  public void setValues( final AbstractChange[] changes ) throws ProfilDataException
  {
     for( final AbstractChange change : changes )
    {
     change.doChange(m_profil);

    }
  }

  // public void setValuesFor( final List<IProfilPoint> pointList, POINT_PROPERTY pointProperty,
  // double value ) throws ProfilDataException
  // {
  // final List<PointChange> changes = new ArrayList<PointChange>( pointList.size() );
  // for( final IProfilPoint point : pointList )
  // changes.add( new PointChange( point, pointProperty, value ) );
  //
  // setValues( changes.toArray( new PointChange[changes.size()] ) );
  // }
  //
  // public void setValuesFor( POINT_PROPERTY pointProperty, double value ) throws
  // ProfilDataException
  // {
  // final List<IProfilPoint> allPoints = getPoints();
  // setValuesFor( allPoints, pointProperty, value );
  //
  // }

  public void setValueFor( IProfilDevider devider, DEVIDER_PROPERTY property, Object value ) throws ProfilDataException
  {
    final DeviderChange change = new DeviderChange( devider, property, value );
    change.doChange(m_profil);
    fireEvents(new AbstractChange[]{change});

  }

  public void setValueFor( final IProfilBuilding building, final BUILDING_PROPERTY property,
      final Object value ) throws ProfilDataException
  {
    final BuildingChange change = new BuildingChange( building, property, value );
    change.doChange(m_profil);
    fireEvents(new AbstractChange[]{change});

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfil#isSpecialPoint(com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean isSpecialPoint( IProfilPoint point )
  {
    return m_profil.isSpecialPoint( point );
  }

}