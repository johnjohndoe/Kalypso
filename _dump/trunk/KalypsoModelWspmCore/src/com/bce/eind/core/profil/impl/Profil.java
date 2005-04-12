package com.bce.eind.core.profil.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import javax.swing.event.EventListenerList;

import com.bce.eind.ProfilBuildingFactory;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilListener;
import com.bce.eind.core.profil.impl.devider.DeviderKey;
import com.bce.eind.core.profil.impl.points.ProfilPointProperty;
import com.bce.eind.core.profil.impl.points.ProfilPoints;
import com.bce.eind.core.profil.util.ProfilUtil;


/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class Profil implements IProfil
{

  private IProfilBuilding m_building;

  private String m_comment = "";

  private final EventListenerList m_listeners = new EventListenerList();

  /* TODO: Kim: diese kommentare zu richtigen javadocs machen und '(non-Javadoc)' entfernen
   * Am besten in Deinen Einstellungen ‰ndern!
   * 
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addCommentLine(java.lang.String)
   */

  private final HashMap<METADATA, Object> m_profilMetaData;

  private final ProfilPoints m_table;

  private final HashMap<ProfilPointProperty, Object> m_tableData;

  private final List m_unknownData;

  public Profil( )
  {
    m_profilMetaData = new HashMap<METADATA, Object>();
    m_table = new ProfilPoints();
    m_unknownData = new Vector();
    m_tableData = new HashMap<ProfilPointProperty, Object>();
    m_table.addColumn( BREITE );
    m_table.addColumn( HOEHE );
    m_table.addColumn( RAUHEIT );
    m_table.addColumn( TRENNFLAECHE );
    m_table.addColumn( DURCHSTROEMTE );
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
    final IProfilPoint newPoint = m_table.addPoint( breite, hoehe );
  //KIM  fireTableChanged( new ProfilTableEvent( null, newPoint ) );
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
    //KIM fireDataChanged( new ProfilMetaDataEvent( metaDataKey, data ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public boolean addTableData( ProfilPointProperty columnKey )
  {
    if( (columnKey == BEWUCHS_AX) | (columnKey == BEWUCHS_AY) | (columnKey == BEWUCHS_DP) )
    {
      m_table.addColumn( BEWUCHS_AX );
      m_table.addColumn( BEWUCHS_AY );
      m_table.addColumn( BEWUCHS_DP );
      return true;
    }
    if( (columnKey == HOCHWERT) | (columnKey == RECHTSWERT) )
    {
      m_table.addColumn( HOCHWERT );
      m_table.addColumn( RECHTSWERT );
      return true;
    }
    m_table.addColumn( columnKey );
    //KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
    return true;
  }

  public void addUnknownData(final Object unknownData )
  {
    m_unknownData.add( unknownData );
  }

  public void fireMetaDataChanged( final METADATA metaData, String value )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onMetaDataChanged( metaData,value);
  }

  public void fireProfilDataChanged( final ProfilPointProperty pointProperty, final Object value )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onProfilDataChanged( pointProperty,value);
  }

  public void firePointChanged( final IProfilPoint point, ProfilPointProperty pointProperty )
  {
    final ProfilListener[] pols = m_listeners.getListeners( ProfilListener.class );
    for( int i = 0; i < pols.length; i++ )
      pols[i].onPointChanged( point,pointProperty);
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
    for( final Iterator<IProfilPoint> ptIt = m_table.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      try
      {
        final double x = pt.getValueFor( deviderKey.getColumnKey() );
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
    if( deviderKey.getColumnKey() != IProfil.TRENNFLAECHE )
      return IProfil.TRENNFLAECHEN_TYP.UNDEFINED;
    final IProfilPoint pktTrennflaeche = this.getDevider( deviderKey );

    try
    {
      final int deviderTypKey = (int)Math.abs( pktTrennflaeche.getValueFor( deviderKey
          .getColumnKey() ) );
      return IProfil.TRENNFLAECHEN_TYP.values()[deviderTypKey];
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      return IProfil.TRENNFLAECHEN_TYP.UNDEFINED;
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#addDevider(com.bce.eind.core.profilinterface.IProfilPoint,
   *      com.bce.eind.core.profilinterface.IProfilPoint,
   *      com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public final LinkedList<ProfilPointProperty> getExistingTableDataKeys( )
  {
    return m_table.getExistingColumns();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getExtendedTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public Object getExtendedTableData( ProfilPointProperty tableDataKey )
  {

    return this.m_tableData.get( tableDataKey );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getNextPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint getNextPoint( IProfilPoint point ) throws ProfilDataException
  {
    final int index = m_table.indexOf( point ) + 1;
    try
    {
      return getPoint( index );
    }
    catch( IndexOutOfBoundsException e )
    {
      throw new ProfilDataException( e.getMessage() );
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(double, double)
   */
  public IProfilPoint getPoint( double breite, double hoehe )
  {
    return m_table.getPoint( breite, hoehe );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(int)
   */
  public IProfilPoint getPoint( int index )
  {
    if( (index > -1) & (index < m_table.size()) )
      return m_table.get( index );
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public List<IProfilPoint> getPoints( )
  {
    return m_table.unmodifiable();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints(com.bce.eind.core.profilinterface.IPoint)
   */
  public List<IProfilPoint> getPoints( IProfilPoint startPoint )
  {
    final int lastIndex = m_table.indexOf( m_table.getLast() );
    final int firstIndex = m_table.indexOf( startPoint );
    if( firstIndex < 0 )
      return null;
    return m_table.unmodifiable().subList( firstIndex, lastIndex );

  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getPointsAtPos(double)
   */
  public LinkedList<IProfilPoint> getPointsAtPos( double breite )
  {
    final LinkedList<IProfilPoint> pointList = new LinkedList<IProfilPoint>();
    for( final Iterator<IProfilPoint> ptIt = m_table.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      try
      {
        if( pt.getValueFor( IProfil.BREITE ) == breite )
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
    return m_table.size();
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
   * @see com.bce.eind.core.profilapi.ProfilApi#getRauheitTyp()
   */
  public RAUHEITEN_TYP getRauheitTyp( )
  {
    return (RAUHEITEN_TYP)this.m_tableData.get( RAUHEIT );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<ProfilPointProperty> getTableDataKeys( )
  {
    return m_table.getVisibleColumns();
  }

  /**
   * @return Returns the unknownCoordData.
   */
  public List getUnknownData( )
  {
    return m_unknownData;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( ProfilPointProperty columnKey ) throws ProfilDataException
  {
    final double[] values = new double[m_table.size()];
    int i = 0;
    for( final Iterator<IProfilPoint> ptIt = m_table.iterator(); ptIt.hasNext(); )
    {
      values[i] = ptIt.next().getValueFor( columnKey );
      i++;
    }
    return values;
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
    m_table.add( m_table.indexOf( thePointNext ), point );
   //KIM fireTableChanged( new ProfilTableEvent( null, point ) );
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
    final IProfilPoint point = m_table.addPoint( thePointBefore );
    point.setValueFor( IProfil.HOEHE, hoehe );
    point.setValueFor( IProfil.BREITE, breite );
   //KIM fireTableChanged( new ProfilTableEvent( null, point ) );
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
    final IProfilPoint oldPosition = getDevider( deviderKey );
    if( !(profilPointExists( newPosition ) & (profilPointExists( oldPosition ))) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final double deviderValue = oldPosition.getValueFor( deviderKey.getColumnKey() );
    newPosition.setValueFor( deviderKey.getColumnKey(), deviderValue );
    oldPosition.setValueFor( deviderKey.getColumnKey(), 0.0 );
  //KIM  fireTableChanged( new ProfilTableEvent( deviderKey.getColumnKey(), newPosition ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#editPoint(com.bce.eind.core.profilinterface.IPoint,
   *      com.bce.eind.core.profildata.tabledata.ColumnKey, double)
   */
  public boolean profilPointExists( final IProfilPoint point )
  {
    return (m_table.indexOf( point ) > -1);
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public boolean removePoint( IProfilPoint point )
  {

    final boolean result = m_table.removePoint( point );
    //KIM fireTableChanged( new ProfilTableEvent( null, point ) );
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
    for( final Iterator<ProfilPointProperty> pbIt = m_building.getTableDataKeys().iterator(); pbIt.hasNext(); )
    {
      removeTableData(pbIt.next());
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
    fireMetaDataChanged(metaData, "" );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public boolean removeTableData( ProfilPointProperty columnKey )
  {
    boolean result;
    if( (columnKey == BEWUCHS_AX) | (columnKey == BEWUCHS_AY) | (columnKey == BEWUCHS_DP) )
    {
      result = (m_table.removeColumn( BEWUCHS_AX ) & m_table.removeColumn( BEWUCHS_AY ) & m_table
          .removeColumn( BEWUCHS_DP ));
    }
    if( (columnKey == HOCHWERT) | (columnKey == RECHTSWERT) )
    {
      result = (m_table.removeColumn( HOCHWERT ) & m_table.removeColumn( RECHTSWERT ));
    }
    result = m_table.removeColumn( columnKey );
    //KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
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

  public void setDevider( IProfilPoint leftPoint, IProfilPoint rightPoint, ProfilPointProperty columnKey )
      throws ProfilDataException
  {
    if( !(profilPointExists( leftPoint ) & (profilPointExists( rightPoint ))) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    setValuesFor( columnKey, 0.0 );
    setValueFor( leftPoint, columnKey, -1.0 );
    setValueFor( rightPoint, columnKey, 1.0 );
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
    if( deviderKey.getColumnKey() != IProfil.TRENNFLAECHE )
      throw new ProfilDataException( "Trenner muﬂ vom Typ Trennfl‰che sein" );
    final IProfilPoint point = getDevider( deviderKey );
    if( point == null )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final ProfilPointProperty columnKey = deviderKey.getColumnKey();
    final double value = deviderKey.getValue() * deviderTyp.ordinal();
    final boolean result = point.setValueFor( columnKey, value );
    //KIM fireTableChanged( new ProfilTableEvent( deviderKey.getColumnKey(), point ) );
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setExtendedTableData(com.bce.eind.core.profildata.tabledata.TableDataKey,
   *      java.lang.Object)
   */
  public void setProfilData( ProfilPointProperty pointProperty, Object value )
  {
    this.m_tableData.put( pointProperty, value );
    fireProfilDataChanged( pointProperty, value );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfil#setProfilBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setProfilBuilding( IProfilBuilding profilBuilding )
  {
    removeProfilBuilding();
    m_building = profilBuilding;

  }

 
  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilapi.ProfilApi#setRauheitTyp(java.lang.String)
   */
  public void setRauheitTyp( RAUHEITEN_TYP r )
  {
    setProfilData(RAUHEIT, r );
    fireProfilDataChanged( RAUHEIT, r );
  }

  public boolean setValueFor( IProfilPoint point, ProfilPointProperty columnKey, double value )
      throws ProfilDataException
  {
    if( profilPointExists( point ) )
    {
      final boolean result = point.setValueFor( columnKey, value );
     //KIM  fireTableChanged( new ProfilTableEvent( columnKey, point ) );
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
  public void setValuesFor( ProfilPointProperty columnKey, double value ) throws ProfilDataException
  {
    for( final Iterator<IProfilPoint> ptIt = m_table.iterator(); ptIt.hasNext(); )
    {
      ptIt.next().setValueFor( columnKey, value );
    }
    //KIM fireTableChanged( new ProfilTableEvent( columnKey, null ) );
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfil#setProfilMetaData(com.bce.eind.core.profil.IProfil.METADATA, java.lang.Object)
   */
  public boolean setProfilMetaData( METADATA metaDataKey, Object data )
  {
    final boolean result = (this.m_profilMetaData.put( metaDataKey, data ) != null);
    fireMetaDataChanged( metaDataKey, data.toString() );
    return result;

  }
}