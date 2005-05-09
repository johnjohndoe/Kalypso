package com.bce.eind.core.profil.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Vector;

import javax.swing.event.EventListenerList;

import com.bce.eind.ProfilBuildingFactory;
import com.bce.eind.core.profil.DeviderKey;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilListener;
import com.bce.eind.core.profil.ProfilPointProperty;
import com.bce.eind.core.profil.impl.points.ProfilPoint;
import com.bce.eind.core.profil.impl.points.ProfilPoints;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class Profil implements IProfil
{

  private IProfilBuilding m_building;

  private String m_comment = "";

  private IProfil.RAUHEITEN_TYP m_rauheit;

  // private final HashMap<IProfilPointProperty, Object> m_extendedPointData;

  /*
   * TODO: Kim: diese kommentare zu richtigen javadocs machen und '(non-Javadoc)' entfernen Am
   * besten in Deinen Einstellungen ‰ndern! (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addCommentLine(java.lang.String)
   */

  private final EventListenerList m_listeners = new EventListenerList();

  private final ProfilPoints m_points;

  private final HashMap<METADATA, Object> m_profilMetaData;

  private final List m_unknownData;

  public Profil( )
  {
    m_profilMetaData = new HashMap<METADATA, Object>();
    m_points = new ProfilPoints();
    m_unknownData = new Vector();
    // m_extendedPointData = new HashMap<IProfilPointProperty, Object>();
    m_points.addProperty( ProfilPointProperty.BREITE );
    m_points.addProperty( ProfilPointProperty.HOEHE );
    m_points.addProperty( ProfilPointProperty.RAUHEIT );
    m_points.addProperty( ProfilPointProperty.TRENNFLAECHE );
    m_points.addProperty( ProfilPointProperty.DURCHSTROEMTE );
    m_building = ProfilBuildingFactory.createProfilBuilding( IProfil.BUILDING_TYP.BLD_NONE );
  }

  public void addCommentLine( String line )
  {
    if( m_comment.length() > 0 )
    {
      m_comment = m_comment + "\n" + line;
    }
    else
      m_comment = line;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(double, double)
   */
  public IProfilPoint addPoint( double breite, double hoehe ) throws ProfilDataException
  {
    final IProfilPoint newPoint = m_points.addPoint( breite, hoehe );
    // KIM fireTableChanged( new ProfilTableEvent( null, newPoint ) );
    return newPoint;
  }

  public void addProfilListener( final ProfilListener pl )
  {
    m_listeners.add( ProfilListener.class, pl );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilUserInterface#addExtendedData(com.bce.eind.extendeddata.CustomProfilData)
   */
  public void addProfilMetaData( final METADATA metaDataKey, final Object data )
  {
    this.m_profilMetaData.put( metaDataKey, data );
    // KIM fireDataChanged( new ProfilMetaDataEvent( metaDataKey, data ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public boolean addProfilPointProperty( ProfilPointProperty pointProperty )
  {
    if( (pointProperty == ProfilPointProperty.BEWUCHS_AX)
        | (pointProperty == ProfilPointProperty.BEWUCHS_AY)
        | (pointProperty == ProfilPointProperty.BEWUCHS_DP) )
    {
      m_points.addProperty( ProfilPointProperty.BEWUCHS_AX );
      m_points.addProperty( ProfilPointProperty.BEWUCHS_AY );
      m_points.addProperty( ProfilPointProperty.BEWUCHS_DP );
      return true;
    }
    if( (pointProperty == ProfilPointProperty.HOCHWERT)
        | (pointProperty == ProfilPointProperty.RECHTSWERT) )
    {
      m_points.addProperty( ProfilPointProperty.HOCHWERT );
      m_points.addProperty( ProfilPointProperty.RECHTSWERT );
      return true;
    }
    m_points.addProperty( pointProperty );
    // KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
    return true;
  }

  public void addUnknownObject( final Object unknownData )
  {
    m_unknownData.add( unknownData );
  }

  public void fireMetaDataChanged( final METADATA metaData, String value )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onMetaDataChanged( metaData, value );
  }

  public void firePointChanged( final IProfilPoint point, ProfilPointProperty pointProperty )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onPointChanged( point, pointProperty );
  }

  public void fireProfilDataChanged( final ProfilPointProperty pointProperty, final Object value )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onProfilDataChanged( pointProperty, value );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getComment()
   */
  public String getComment( )
  {
    return m_comment;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getDevider(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public IProfilPoint getDevider( DeviderKey deviderKey )
  {
    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      try
      {
        final double x = pt.getValueFor( deviderKey.getProfilPointProperty() );
        final double y = deviderKey.getValue();
        if( x * y > 0 )
          return pt;
      }
      catch( ProfilDataException e )
      {
        e.printStackTrace();
      }

    }
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public TRENNFLAECHEN_TYP getDeviderTyp( DeviderKey deviderKey )
  {
    if( deviderKey.getProfilPointProperty() != ProfilPointProperty.TRENNFLAECHE )
      return IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_UNDEFINED;
    final IProfilPoint pktTrennflaeche = this.getDevider( deviderKey );

    try
    {
      final int deviderTypKey = (int)Math.abs( pktTrennflaeche.getValueFor( deviderKey
          .getProfilPointProperty() ) );
      return IProfil.TRENNFLAECHEN_TYP.values()[deviderTypKey];
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      return IProfil.TRENNFLAECHEN_TYP.TRENNFLAECHE_UNDEFINED;
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addDevider(com.bce.eind.core.profilinterface.IProfilPoint,
   *      com.bce.eind.core.profilinterface.IProfilPoint,
   *      com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public final LinkedList<ProfilPointProperty> getExistingPointProperties( )
  {
    return m_points.getExistingProperties();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getExtendedTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  /*
   * public Object getExtendedPointData( IProfilPointProperty pointProperty ) { return
   * this.m_extendedPointData.get( pointProperty ); }
   */
  /*
   * @return null, falls point der letzte Punkt ist
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getNextPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint getNextPoint( IProfilPoint point ) throws ProfilDataException
  {
    if( !m_points.contains( point ) )
      throw new ProfilDataException( "Punkt existiert nicht" );

    if( point == m_points.getLast() )
      return null;

    final ListIterator<IProfilPoint> ptIt = m_points.listIterator( m_points.indexOf( point ) + 1 );

    return ptIt.next();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(double, double)
   */
  public IProfilPoint getPoint( double breite, double hoehe )
  {
    return m_points.getPoint( breite, hoehe );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(int)
   */
  public IProfilPoint getPoint( int index )
  {
    if( (index > -1) & (index < m_points.size()) )
      return m_points.get( index );
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public List<IProfilPoint> getPoints( )
  {
    return m_points.unmodifiable();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints(com.bce.eind.core.profilinterface.IPoint)
   */
  public List<IProfilPoint> getPoints( IProfilPoint startPoint )
  {
    final int lastIndex = m_points.indexOf( m_points.getLast() );
    final int firstIndex = m_points.indexOf( startPoint );
    if( firstIndex < 0 )
      return null;
    return m_points.unmodifiable().subList( firstIndex, lastIndex );

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPointsAtPos(double)
   */
  public LinkedList<IProfilPoint> getPointsAtPos( double breite )
  {
    final LinkedList<IProfilPoint> pointList = new LinkedList<IProfilPoint>();
    final int precision = ProfilPointProperty.BREITE.getPrecision();
    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      try
      {
        if( Math.abs( pt.getValueFor( ProfilPointProperty.BREITE ) - breite ) < Math
            .exp( -precision ) )
          pointList.add( pt );
      }
      catch( ProfilDataException e )
      {
        e.printStackTrace();
      }
    }
    return pointList;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getSize()
   */
  public int getPointsCount( )
  {
    return m_points.size();
  }

  /**
   * @return null, falls point der erste Punkt ist
   * @see com.bce.eind.core.profil.IProfil#getPreviousPoint(com.bce.eind.core.profil.IProfilPoint)
   */
  public IProfilPoint getPreviousPoint( IProfilPoint point ) throws ProfilDataException
  {
    if( !m_points.contains( point ) )
      throw new ProfilDataException( "Punkt existiert nicht" );

    if( point == m_points.getFirst() )
      return null;

    final ListIterator<IProfilPoint> ptIt = m_points.listIterator( m_points.indexOf( point ) );

    return ptIt.previous();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getProfilBuilding()
   */
  public IProfilBuilding getProfilBuilding( )
  {
    return m_building;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getMetaData()
   */
  public Object getProfilMetaData( METADATA metaData )
  {
    return this.m_profilMetaData.get( metaData );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<ProfilPointProperty> getProfilPointProperties( final boolean filterNonVisible )
  {
    if( filterNonVisible )
      return m_points.getVisibleProperties();
    return m_points.getExistingProperties();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilapi.ProfilApi#getRauheitTyp()
   */
  public RAUHEITEN_TYP getRauheitTyp( )
  {
    return m_rauheit;
  }

  /**
   * @return Returns the unknownCoordData.
   */
  public List getUnknownObjects( )
  {
    return m_unknownData;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( ProfilPointProperty pointProperty ) throws ProfilDataException
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

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfil#indexOf(com.bce.eind.core.profil.IProfilPoint)
   */
  public int indexOf( IProfilPoint point )
  {
    return this.m_points.indexOf( point );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#insertPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( IProfilPoint thePointBefore ) throws ProfilDataException
  {
    IProfilPoint thePointNext = getNextPoint( thePointBefore );
    final IProfilPoint point = ProfilUtil.splitSegment( thePointBefore, thePointNext );
    m_points.add( m_points.indexOf( thePointNext ), point );
    // KIM fireTableChanged( new ProfilTableEvent( null, point ) );
    return point;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(com.bce.eind.core.profilinterface.IPoint,
   *      double, double)
   */
  public IProfilPoint insertPoint( IProfilPoint thePointBefore, double breite, double hoehe )
      throws ProfilDataException
  {
    final ProfilPoint point = (ProfilPoint)m_points.addPoint( thePointBefore );
    point.setValueFor( ProfilPointProperty.HOEHE, hoehe );
    point.setValueFor( ProfilPointProperty.BREITE, breite );
    // KIM fireTableChanged( new ProfilTableEvent( null, point ) );
    return point;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#moveDevider(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public void moveDevider( DeviderKey deviderKey, IProfilPoint newPosition )
      throws ProfilDataException
  {
    final ProfilPoint oldPosition = (ProfilPoint)getDevider( deviderKey );
    if( !(profilPointExists( newPosition ) & (profilPointExists( oldPosition ))) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final double deviderValue = oldPosition.getValueFor( deviderKey.getProfilPointProperty() );
    ((ProfilPoint)newPosition).setValueFor( deviderKey.getProfilPointProperty(), deviderValue );
    oldPosition.setValueFor( deviderKey.getProfilPointProperty(), 0.0 );
    // KIM fireTableChanged( new ProfilTableEvent( deviderKey.getColumnKey(), newPosition ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#editPoint(com.bce.eind.core.profilinterface.IPoint,
   *      com.bce.eind.core.profildata.tabledata.ColumnKey, double)
   */
  public boolean profilPointExists( final IProfilPoint point )
  {
    return (m_points.indexOf( point ) > -1);
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public boolean removePoint( IProfilPoint point )
  {

    final boolean result = m_points.removePoint( point );
    // KIM fireTableChanged( new ProfilTableEvent( null, point ) );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removeProfilBuilding()
   */
  public IProfilBuilding removeProfilBuilding( )
  {
    final IProfilBuilding oldBuilding = m_building;
    for( final Iterator<ProfilPointProperty> pbIt = m_building.getProfilPointProperties()
        .iterator(); pbIt.hasNext(); )
    {
      removeProfilPointProperty( pbIt.next() );
    }
    m_building = ProfilBuildingFactory.createProfilBuilding(BUILDING_TYP.BLD_NONE);
    return oldBuilding;
  }

  public void removeProfilListener( final ProfilListener pl )
  {
    m_listeners.remove( ProfilListener.class, pl );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removeExtendedData(java.lang.String)
   */
  public boolean removeProfilMetaData( com.bce.eind.core.profil.IProfil.METADATA metaData )
  {
    final boolean result = (m_profilMetaData.remove( metaData ) != null);
    fireMetaDataChanged( metaData, "" );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public boolean removeProfilPointProperty( ProfilPointProperty pointProperty )
  {
    boolean result;
    if( (pointProperty == ProfilPointProperty.BEWUCHS_AX)
        | (pointProperty == ProfilPointProperty.BEWUCHS_AY)
        | (pointProperty == ProfilPointProperty.BEWUCHS_DP) )
    {
      result = (m_points.removeProperty( ProfilPointProperty.BEWUCHS_AX )
          & m_points.removeProperty( ProfilPointProperty.BEWUCHS_AY ) & m_points
          .removeProperty( ProfilPointProperty.BEWUCHS_DP ));
    }
    if( (pointProperty == ProfilPointProperty.HOCHWERT)
        | (pointProperty == ProfilPointProperty.RECHTSWERT) )
    {
      result = (m_points.removeProperty( ProfilPointProperty.HOCHWERT ) & m_points
          .removeProperty( ProfilPointProperty.RECHTSWERT ));
    }
    result = m_points.removeProperty( pointProperty );
    // KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setComment(java.lang.String)
   */
  public void setComment( String comment )
  {
    m_comment = comment;

  }

  public void setDevider( IProfilPoint leftPoint, IProfilPoint rightPoint,
      ProfilPointProperty pointProperty ) throws ProfilDataException
  {
    if( !(profilPointExists( leftPoint ) & (profilPointExists( rightPoint ))) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    setValuesFor( pointProperty, 0.0 );
    setValueFor( leftPoint, pointProperty, -1.0 );
    setValueFor( rightPoint, pointProperty, 1.0 );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      int)
   */
  public boolean setDeviderTyp( DeviderKey deviderKey, TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException
  {
    if( deviderKey.getProfilPointProperty() != ProfilPointProperty.TRENNFLAECHE )
      throw new ProfilDataException( "Trenner muﬂ vom Typ Trennfl‰che sein" );
    final ProfilPoint point = (ProfilPoint)getDevider( deviderKey );
    if( point == null )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    // final ProfilPointProperty pointProperty = deviderKey.getProfilPointProperty();
    // final double value = deviderKey.getValue() * deviderTyp.ordinal();
    final boolean result = point.setValueFor( ProfilPointProperty.TRENNFLAECHE, Math
        .signum( deviderKey.getValue() )
        * deviderTyp.ordinal() );
    // KIM fireTableChanged( new ProfilTableEvent( deviderKey.getColumnKey(), point ) );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setProfilBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setProfilBuilding( IProfil.BUILDING_TYP buildingTyp )
  {
    removeProfilBuilding();
    m_building = ProfilBuildingFactory.createProfilBuilding( buildingTyp );
    for( final Iterator<ProfilPointProperty> pbIt = m_building.getProfilPointProperties()
        .iterator(); pbIt.hasNext(); )
    {
      addProfilPointProperty( pbIt.next() );
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setExtendedTableData(com.bce.eind.core.profildata.tabledata.TableDataKey,
   *      java.lang.Object)
   */
  /*
   * public void setProfilData( IProfilPointProperty pointProperty, Object value ) {
   * this.m_extendedPointData.put( pointProperty, value ); fireProfilDataChanged( pointProperty,
   * value ); }
   */
  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfil#setProfilMetaData(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.Object)
   */
  public boolean setProfilMetaData( METADATA metaDataKey, Object data )
  {
    final boolean result = (this.m_profilMetaData.put( metaDataKey, data ) != null);
    fireMetaDataChanged( metaDataKey, data.toString() );
    return result;

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilapi.ProfilApi#setRauheitTyp(java.lang.String)
   */
  public void setRauheitTyp( RAUHEITEN_TYP r )
  {
    m_rauheit = r;
    fireProfilDataChanged( ProfilPointProperty.RAUHEIT, r );
  }

  public boolean setValueFor( IProfilPoint point, ProfilPointProperty pointProperty, double value )
      throws ProfilDataException
  {
    if( profilPointExists( point ) )
    {
      final boolean result = ((ProfilPoint)point).setValueFor( pointProperty, value );
      // KIM fireTableChanged( new ProfilTableEvent( columnKey, point ) );
      return result;
    }
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey,
   *      double)
   */
  public void setValuesFor( ProfilPointProperty pointProperty, double value )
      throws ProfilDataException
  {
    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      ((ProfilPoint)ptIt.next()).setValueFor( pointProperty, value );
    }
    // KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profil.IProfil#insertPoint(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean insertPoint( IProfilPoint thePointBefore, IProfilPoint point )
      throws ProfilDataException
  {
    return m_points.insertPoint( thePointBefore, point );
  }

}