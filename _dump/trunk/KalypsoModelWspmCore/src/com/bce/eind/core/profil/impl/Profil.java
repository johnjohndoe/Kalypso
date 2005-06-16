package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.DeviderKey;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilPointProperty;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class Profil implements IProfil
{
  private final List<IProfilListener> m_listeners = new ArrayList<IProfilListener>( 10 );

  private PlainProfil m_profil = new PlainProfil();

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(double, double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final IProfilPoint addPoint = m_profil.addPoint( breite, hoehe );

    firePointsAdded( new IProfilPoint[]
    { addPoint } );

    return addPoint;
  }

  /**
   * @see com.bce.eind.core.profilUserInterface#addExtendedData(com.bce.eind.extendeddata.CustomProfilData)
   */
  public void addProfilMetaData( final METADATA metaDataKey, final Object data )
  {
    m_profil.addProfilMetaData( metaDataKey, data );

    fireMetaDataChanged( metaDataKey, data );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public ProfilPointProperty[] addProfilPointProperty( final ProfilPointProperty pointProperty )
      throws ProfilDataException
  {
    final ProfilPointProperty[] newProperties = m_profil.addProfilPointProperty( pointProperty );

    firePointPropertiesAdded( newProperties );

    return newProperties;
  }

  public void addUnknownObject( final Object unknownData )
  {
    m_profil.addUnknownObject( unknownData );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getComment()
   */
  public String getComment( )
  {
    return m_profil.getComment();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDevider(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public IProfilPoint getDevider( final DeviderKey deviderKey )
  {
    return m_profil.getDevider( deviderKey );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public TRENNFLAECHEN_TYP getDeviderTyp( DeviderKey deviderKey )
  {
    return m_profil.getDeviderTyp( deviderKey );
  }

  /**
   * @return null, falls point der letzte Punkt ist
   * @see com.bce.eind.core.profilinterface.IProfil#getNextPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint getNextPoint( final IProfilPoint point ) throws ProfilDataException
  {
    return m_profil.getNextPoint( point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(double, double)
   */
  public IProfilPoint getPoint( final double breite, final double hoehe )
  {
    return m_profil.getPoint( breite, hoehe );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(int)
   */
  public IProfilPoint getPoint( int index )
  {
    return m_profil.getPoint( index );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public List<IProfilPoint> getPoints( )
  {
    return m_profil.getPoints();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints(com.bce.eind.core.profilinterface.IPoint)
   */
  public List<IProfilPoint> getPoints( final IProfilPoint startPoint )
  {
    return m_profil.getPoints( startPoint );

  }

  public List<IProfilPoint> getPoints( final IProfilPoint startPoint, final IProfilPoint endPoint )
  {
    return m_profil.getPoints( startPoint, endPoint );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPointsAtPos(double)
   */
  public LinkedList<IProfilPoint> getPointsAtPos( double breite )
  {
    return m_profil.getPointsAtPos( breite );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getSize()
   */
  public int getPointsCount( )
  {
    return m_profil.getPointsCount();
  }

  /**
   * @return null, falls point der erste Punkt ist
   * @see com.bce.eind.core.profil.IProfil#getPreviousPoint(com.bce.eind.core.profil.IProfilPoint)
   */
  public IProfilPoint getPreviousPoint( IProfilPoint point ) throws ProfilDataException
  {
    return m_profil.getPreviousPoint( point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getProfilBuilding()
   */
  public IProfilBuilding getProfilBuilding( )
  {
    return m_profil.getProfilBuilding();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getMetaData()
   */
  public Object getProfilMetaData( METADATA metaData )
  {
    return m_profil.getProfilMetaData( metaData );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<ProfilPointProperty> getProfilPointProperties( final boolean filterNonVisible )
  {
    return m_profil.getProfilPointProperties( filterNonVisible );
  }

  /**
   * @see com.bce.eind.core.profilapi.ProfilApi#getRauheitTyp()
   */
  public RAUHEITEN_TYP getRauheitTyp( )
  {
    return m_profil.getRauheitTyp();
  }

  /**
   * @return Returns the unknownCoordData.
   */
  public List<Object> getUnknownObjects( )
  {
    return m_profil.getUnknownObjects();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( final ProfilPointProperty pointProperty )
      throws ProfilDataException
  {
    return m_profil.getValuesFor( pointProperty );
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#indexOf(com.bce.eind.core.profil.IProfilPoint)
   */
  public int indexOf( final IProfilPoint point )
  {
    return m_profil.indexOf( point );
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
  public void moveDevider( final DeviderKey deviderKey, final IProfilPoint newPosition )
      throws ProfilDataException
  {
    m_profil.moveDevider( deviderKey, newPosition );

    // TODO: eventuell eigener event typ?
    firePointValuesChanged( null );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public void removePoint( final IProfilPoint point )
  {
    m_profil.removePoint( point );

    firePointsRemoved( new IProfilPoint[]
    { point } );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeProfilBuilding()
   */
  public IProfilBuilding removeProfilBuilding( )
  {
    final IProfilBuilding building = m_profil.removeProfilBuilding();

    fireBuildingChanged();

    return building;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeExtendedData(java.lang.String)
   */
  public void removeProfilMetaData( final IProfil.METADATA metaData )
  {
    m_profil.removeProfilMetaData( metaData );

    fireMetaDataChanged( metaData, null );
  }

  /**
   * @return
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public ProfilPointProperty[] removeProfilPointProperty( final ProfilPointProperty pointProperty )
  {
    final ProfilPointProperty[] removeProperties = m_profil
        .removeProfilPointProperty( pointProperty );

    firePointPropertiesRemoved( removeProperties );

    return removeProperties;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#setComment(java.lang.String)
   */
  public void setComment( final String comment )
  {
    m_profil.setComment( comment );

    fireCommentChanged();
  }

  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final ProfilPointProperty pointProperty ) throws ProfilDataException
  {
    m_profil.setDevider( leftPoint, rightPoint, pointProperty );

    firePointValuesChanged( null );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#setDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      int)
   */
  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException
  {
    final boolean result = m_profil.setDeviderTyp( deviderKey, deviderTyp );

    firePointValuesChanged( null );

    return result;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#setProfilBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setProfilBuilding( final IProfil.BUILDING_TYP buildingTyp )
      throws ProfilDataException
  {
    m_profil.setProfilBuilding( buildingTyp );

    fireBuildingChanged();
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setProfilMetaData(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.Object)
   */
  public void setProfilMetaData( METADATA metaDataKey, Object data )
  {
    m_profil.setProfilMetaData( metaDataKey, data );

    fireMetaDataChanged( metaDataKey, data );
  }

  /**
   * @see com.bce.eind.core.profilapi.ProfilApi#setRauheitTyp(java.lang.String)
   */
  public void setRauheitTyp( final RAUHEITEN_TYP r )
  {
    m_profil.setRauheitTyp( r );
  }

  public void setValueFor( final IProfilPoint point, final ProfilPointProperty pointProperty,
      final double value ) throws ProfilDataException
  {
    setValues( new ProfilChange[]
    { new ProfilChange( point, pointProperty, value ) } );
  }

  public void setValuesFor( final List<IProfilPoint> pointList, ProfilPointProperty pointProperty,
      double value ) throws ProfilDataException
  {
    final List<ProfilChange> changes = new ArrayList<ProfilChange>( pointList.size() );
    for( final IProfilPoint point : pointList )
      changes.add( new ProfilChange( point, pointProperty, value ) );

    setValues( changes.toArray( new ProfilChange[changes.size()] ) );
  }

  public void editBuilding( final ProfilBuildingProperty buildingProperty, final double value )
      throws ProfilBuildingException
  {
    m_profil.editBuilding( buildingProperty, value );

    fireBuildingDataChanged( m_profil.getProfilBuilding(), buildingProperty, value );
  }

  public IProfilPoint getPointCloseTo( final double breite ) throws ProfilDataException
  {
    return m_profil.getPointCloseTo( breite );
  }

  /** Interne Methode die wirklich die Daten ändert. Schickt KEINEN event ! */
  public void setValues( final ProfilChange[] changes ) throws ProfilDataException
  {
    m_profil.setValues( changes );

    firePointValuesChanged( changes );
  }

  public void addProfilListener( final IProfilListener pl )
  {
    m_listeners.add( pl );
  }

  public void removeProfilListener( final IProfilListener pl )
  {
    m_listeners.remove( pl );
  }

  public void fireBuildingChanged( )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onBuildingChanged();
  }

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

  public void fireMetaDataChanged( final METADATA metadata, final Object value )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onMetaDataChanged( metadata, value );
  }

  public void firePointPropertiesAdded( final ProfilPointProperty[] addedProperties )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointPropertiesAdded( addedProperties );
  }

  public void firePointPropertiesRemoved( final ProfilPointProperty[] removeProperties )
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

  public void firePointValuesChanged( final ProfilChange[] changes )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onPointValuesChanged( changes );
  }
}