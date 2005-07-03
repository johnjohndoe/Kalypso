package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Vector;

import com.bce.eind.ProfilBuildingFactory;
import com.bce.eind.core.profil.DeviderKey;
import com.bce.eind.core.profil.IPlainProfil;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilPointProperty;
import com.bce.eind.core.profil.impl.points.ProfilPoint;
import com.bce.eind.core.profil.impl.points.ProfilPoints;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class PlainProfil implements IPlainProfil
{
  private IProfilBuilding m_building;

  private String m_comment = "";

  private final ProfilPoints m_points;

  private final HashMap<METADATA, Object> m_profilMetaData;

  /** TODO: Gesezt, damit nie null, sollte aber von aussen auf einen Standardwert gesetzt sein */
  private IProfil.RAUHEITEN_TYP m_rauheit = RAUHEITEN_TYP.ks;

  private final List<Object> m_unknownData;

  public PlainProfil( )
  {
    m_profilMetaData = new HashMap<METADATA, Object>();
    m_points = new ProfilPoints();
    m_unknownData = new Vector<Object>();
    m_points.addProperty( ProfilPointProperty.BREITE );
    m_points.addProperty( ProfilPointProperty.HOEHE );
    m_points.addProperty( ProfilPointProperty.RAUHEIT );
    m_points.addProperty( ProfilPointProperty.TRENNFLAECHE );
    m_points.addProperty( ProfilPointProperty.DURCHSTROEMTE );
    m_building = ProfilBuildingFactory.createProfilBuilding( IProfil.BUILDING_TYP.NONE );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(double, double)
   */
  public IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    return m_points.addPoint( breite, hoehe );
  }

  /**
   * @see com.bce.eind.core.profilUserInterface#addExtendedData(com.bce.eind.extendeddata.CustomProfilData)
   */
  public void addProfilMetaData( final METADATA metaDataKey, final Object data )
  {
    m_profilMetaData.put( metaDataKey, data );
  }

  private void setDefaults( final ProfilPointProperty property ) throws ProfilDataException
  {
    final List<IProfilPoint> points = getPoints();

    final ProfilPoint firstPoint = (ProfilPoint)(points.isEmpty() ? null : points.get( 0 ));
    final ProfilPoint lastPoint = (ProfilPoint)(points.isEmpty() ? null : points
        .get( points.size() - 1 ));

    if( property == ProfilPointProperty.BORDVOLL )
    {
      final ProfilPoint lpkt = (ProfilPoint)getDevider( DeviderKey.TRENNFLAECHE_L );
      final ProfilPoint rpkt = (ProfilPoint)getDevider( DeviderKey.TRENNFLAECHE_R );

      final ProfilPoint leftPoint = (lpkt == null ? firstPoint : lpkt);
      final ProfilPoint rightPoint = (rpkt == null ? lastPoint : rpkt);

      setDevider(leftPoint, rightPoint, property );
    }
    else if( property == ProfilPointProperty.TRENNFLAECHE )
      setDevider( firstPoint, lastPoint, property );
    else if( property == ProfilPointProperty.DURCHSTROEMTE )
      setDevider( firstPoint, lastPoint, property );
  }

  /**
   * @return
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#addColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public ProfilPointProperty[] addProfilPointProperty( final ProfilPointProperty pointProperty )
      throws ProfilDataException
  {
    final ProfilPointProperty[] newProperties;

    if( (pointProperty == ProfilPointProperty.BEWUCHS_AX)
        | (pointProperty == ProfilPointProperty.BEWUCHS_AY)
        | (pointProperty == ProfilPointProperty.BEWUCHS_DP) )
    {
      m_points.addProperty( ProfilPointProperty.BEWUCHS_AX );
      m_points.addProperty( ProfilPointProperty.BEWUCHS_AY );
      m_points.addProperty( ProfilPointProperty.BEWUCHS_DP );

      newProperties = new ProfilPointProperty[]
      { ProfilPointProperty.BEWUCHS_AX, ProfilPointProperty.BEWUCHS_AY,
          ProfilPointProperty.BEWUCHS_DP };
    }
    else if( (pointProperty == ProfilPointProperty.HOCHWERT)
        | (pointProperty == ProfilPointProperty.RECHTSWERT) )
    {
      m_points.addProperty( ProfilPointProperty.HOCHWERT );
      m_points.addProperty( ProfilPointProperty.RECHTSWERT );

      newProperties = new ProfilPointProperty[]
      { ProfilPointProperty.HOCHWERT, ProfilPointProperty.RECHTSWERT };
    }
    else
    {
      m_points.addProperty( pointProperty );
      newProperties = new ProfilPointProperty[]
      { pointProperty };
    }

    setDefaults( pointProperty );

    return newProperties;
  }

  public void addUnknownObject( final Object unknownData )
  {
    m_unknownData.add( unknownData );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getComment()
   */
  public String getComment( )
  {
    return m_comment;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDevider(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public IProfilPoint getDevider( final DeviderKey deviderKey )
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

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey)
   */
  public TRENNFLAECHEN_TYP getDeviderTyp( final DeviderKey deviderKey )
  {
    if( deviderKey.getProfilPointProperty() != ProfilPointProperty.TRENNFLAECHE )
      return IProfil.TRENNFLAECHEN_TYP.UNDEFINED;
    final IProfilPoint pktTrennflaeche = getDevider( deviderKey );
    if( pktTrennflaeche == null )
      return IProfil.TRENNFLAECHEN_TYP.UNDEFINED;

    try
    {
      final int deviderTypKey = (int)Math.abs( pktTrennflaeche.getValueFor( deviderKey
          .getProfilPointProperty() ) );
      return IProfil.TRENNFLAECHEN_TYP.values()[deviderTypKey];
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      return IProfil.TRENNFLAECHEN_TYP.UNDEFINED;
    }

  }

  /**
   * @return null, falls point der letzte Punkt ist
   * @see com.bce.eind.core.profilinterface.IProfil#getNextPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint getNextPoint( final IProfilPoint point ) throws ProfilDataException
  {
    if( !m_points.contains( point ) )
      throw new ProfilDataException( "Punkt existiert nicht" );

    if( point == m_points.getLast() )
      return null;

    final ListIterator<IProfilPoint> ptIt = m_points.listIterator( m_points.indexOf( point ) + 1 );

    return ptIt.next();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(double, double)
   */
  public IProfilPoint getPoint( final double breite, final double hoehe )
  {
    return m_points.getPoint( breite, hoehe );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoint(int)
   */
  public IProfilPoint getPoint( int index )
  {
    if( (index > -1) & (index < m_points.size()) )
      return m_points.get( index );
    return null;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints()
   */
  public List<IProfilPoint> getPoints( )
  {
    return m_points.unmodifiable();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getPoints(com.bce.eind.core.profilinterface.IPoint)
   */
  public List<IProfilPoint> getPoints( final IProfilPoint startPoint )
  {
    return getPoints( startPoint, m_points.getLast() );

  }

  public List<IProfilPoint> getPoints( final IProfilPoint startPoint, final IProfilPoint endPoint )
  {
    final int lastIndex = m_points.indexOf( endPoint );
    final int firstIndex = m_points.indexOf( startPoint );
    if( (firstIndex < 0) | (lastIndex < 0) )
      return null;
    return m_points.unmodifiable().subList( firstIndex, lastIndex );
  }

  /**
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

  /**
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

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getProfilBuilding()
   */
  public IProfilBuilding getProfilBuilding( )
  {
    return m_building;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getMetaData()
   */
  public Object getProfilMetaData( METADATA metaData )
  {
    return this.m_profilMetaData.get( metaData );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getTableDataKeys()
   */
  public LinkedList<ProfilPointProperty> getProfilPointProperties( final boolean filterNonVisible )
  {
    if( filterNonVisible )
      return m_points.getVisibleProperties();
    return m_points.getExistingProperties();
  }

  /**
   * @see com.bce.eind.core.profilapi.ProfilApi#getRauheitTyp()
   */
  public RAUHEITEN_TYP getRauheitTyp( )
  {
    return m_rauheit;
  }

  /**
   * @return Returns the unknownCoordData.
   */
  public List<Object> getUnknownObjects( )
  {
    return m_unknownData;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#getValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public double[] getValuesFor( final ProfilPointProperty pointProperty )
      throws ProfilDataException
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
   * @see com.bce.eind.core.profil.IProfil#indexOf(com.bce.eind.core.profil.IProfilPoint)
   */
  public int indexOf( final IProfilPoint point )
  {
    return this.m_points.indexOf( point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#insertPoint(com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore ) throws ProfilDataException
  {
    final IProfilPoint thePointNext = getNextPoint( thePointBefore );
    final IProfilPoint point = ProfilUtil.splitSegment( thePointBefore, thePointNext );
    m_points.add( m_points.indexOf( thePointNext ), point );

    return point;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#addPoint(com.bce.eind.core.profilinterface.IPoint,
   *      double, double)
   */
  public IProfilPoint insertPoint( final IProfilPoint thePointBefore, final double breite,
      final double hoehe ) throws ProfilDataException
  {
    final ProfilPoint point = (ProfilPoint)m_points.addPoint( thePointBefore );
    point.setValueFor( ProfilPointProperty.HOEHE, hoehe );
    point.setValueFor( ProfilPointProperty.BREITE, breite );

    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#insertPoint(com.bce.eind.core.profil.IProfilPoint,
   *      com.bce.eind.core.profil.IProfilPoint)
   */
  public boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException
  {
    return m_points.insertPoint( thePointBefore, point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#moveDevider(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      com.bce.eind.core.profilinterface.IProfilPoint)
   */
  public boolean moveDevider( final DeviderKey deviderKey, final IProfilPoint newPosition )
      throws ProfilDataException
  {
    final ProfilPoint oldPosition = (ProfilPoint)getDevider( deviderKey );
    if( oldPosition == newPosition )
      return false;

    if( (newPosition.getValueFor( deviderKey.getProfilPointProperty() ) != 0) )
      throw new ProfilDataException( "ung¸ltige Position" );
    if( !(profilPointExists( newPosition ) & (profilPointExists( oldPosition ))) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final double deviderValue = oldPosition.getValueFor( deviderKey.getProfilPointProperty() );
    ((ProfilPoint)newPosition).setValueFor( deviderKey.getProfilPointProperty(), deviderValue );
    oldPosition.setValueFor( deviderKey.getProfilPointProperty(), 0.0 );
    
    return true;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#editPoint(com.bce.eind.core.profilinterface.IPoint,
   *      com.bce.eind.core.profildata.tabledata.ColumnKey, double)
   */
  private boolean profilPointExists( final IProfilPoint point )
  {
    return m_points.indexOf( point ) > -1;
  }

  /**
   * @throws ProfilDataException 
   * @see com.bce.eind.core.profilinterface.IProfil#removePoint(com.bce.eind.core.profilinterface.IPoint)
   */
  public void removePoint( final IProfilPoint point ) throws ProfilDataException
  {
    if( !m_points.contains( point ) )
      throw new ProfilDataException( "Punkt exisitert nicht: " + point );

    m_points.removePoint( point );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeProfilBuilding()
   */
  public IProfilBuilding removeProfilBuilding( )
  {
    final IProfilBuilding oldBuilding = m_building;
    for( final ProfilPointProperty property : m_building.getProfilPointProperties() )
      removeProfilPointProperty( property );

    m_building = ProfilBuildingFactory.createProfilBuilding( BUILDING_TYP.NONE );

    return oldBuilding;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#removeExtendedData(java.lang.String)
   */
  public void removeProfilMetaData( final IProfil.METADATA metaData )
  {
    // TODO: exception werfen, wenn metadata nicht existiert
    m_profilMetaData.remove( metaData );
  }

  /**
   * @return
   * @see com.bce.eind.core.profilinterface.IProfil#removeColumn(com.bce.eind.core.profildata.tabledata.ColumnKey)
   */
  public ProfilPointProperty[] removeProfilPointProperty( final ProfilPointProperty pointProperty )
  {
    final ProfilPointProperty[] removeProperties;
    if( (pointProperty == ProfilPointProperty.BEWUCHS_AX)
        | (pointProperty == ProfilPointProperty.BEWUCHS_AY)
        | (pointProperty == ProfilPointProperty.BEWUCHS_DP) )
    {
      m_points.removeProperty( ProfilPointProperty.BEWUCHS_AX );
      m_points.removeProperty( ProfilPointProperty.BEWUCHS_AY );
      m_points.removeProperty( ProfilPointProperty.BEWUCHS_DP );

      removeProperties = new ProfilPointProperty[]
      { ProfilPointProperty.BEWUCHS_AX, ProfilPointProperty.BEWUCHS_AY,
          ProfilPointProperty.BEWUCHS_DP };
    }
    else if( (pointProperty == ProfilPointProperty.HOCHWERT)
        | (pointProperty == ProfilPointProperty.RECHTSWERT) )
    {
      m_points.removeProperty( ProfilPointProperty.HOCHWERT );
      m_points.removeProperty( ProfilPointProperty.RECHTSWERT );

      removeProperties = new ProfilPointProperty[]
      { ProfilPointProperty.HOCHWERT, ProfilPointProperty.RECHTSWERT };
    }
    else
    {
      m_points.removeProperty( pointProperty );
      removeProperties = new ProfilPointProperty[]
      { pointProperty };
    }

    return removeProperties;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#setComment(java.lang.String)
   */
  public void setComment( final String comment )
  {
    if( m_comment.equals( comment ) )
      return;

    m_comment = comment;
  }

  public void setDevider( final IProfilPoint leftPoint, final IProfilPoint rightPoint,
      final ProfilPointProperty pointProperty ) throws ProfilDataException
  {
    if( !( profilPointExists( leftPoint ) & (profilPointExists( rightPoint ) ) ) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );

    setValuesFor( pointProperty, 0.0 );
    setValueFor( leftPoint, pointProperty, -1.0 );
    setValueFor( rightPoint, pointProperty, 1.0 );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#setDeviderTyp(com.bce.eind.core.profildata.tabledata.DeviderKey,
   *      int)
   */
  public boolean setDeviderTyp( final DeviderKey deviderKey, final TRENNFLAECHEN_TYP deviderTyp )
      throws ProfilDataException
  {
    if( deviderKey.getProfilPointProperty() != ProfilPointProperty.TRENNFLAECHE )
      throw new ProfilDataException( "Trenner muﬂ vom Typ Trennfl‰che sein" );
    final ProfilPoint point = (ProfilPoint)getDevider( deviderKey );
    if( point == null )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );

    final boolean result = point.setValueFor( ProfilPointProperty.TRENNFLAECHE, Math
        .signum( deviderKey.getValue() )
        * deviderTyp.ordinal() );

    return result;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profilinterface.IProfil#setProfilBuilding(com.bce.eind.core.profilinterface.IProfilBuilding)
   */
  public void setProfilBuilding( final IProfil.BUILDING_TYP buildingTyp )
      throws ProfilDataException
  {
    removeProfilBuilding();
    m_building = ProfilBuildingFactory.createProfilBuilding( buildingTyp );

    for( final ProfilPointProperty property : m_building.getProfilPointProperties() )
      addProfilPointProperty( property );

    if( buildingTyp == IProfil.BUILDING_TYP.BRUECKE )
    {
      for( final Iterator<IProfilPoint> pktIt = m_points.iterator(); pktIt.hasNext(); )
      {
        final ProfilPoint pkt = (ProfilPoint)pktIt.next();
        try
        {
          final double h = pkt.getValueFor( ProfilPointProperty.HOEHE );
          pkt.setValueFor( ProfilPointProperty.OBERKANTEBRUECKE, h );
          pkt.setValueFor( ProfilPointProperty.UNTERKANTEBRUECKE, h );
        }
        catch( final ProfilDataException e )
        {
          // TODO:Kim RuntimeException werfen
        }
      }
    }
  }

  /**
   * @see com.bce.eind.core.profil.IProfil#setProfilMetaData(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.Object)
   */
  public void setProfilMetaData( final METADATA metaDataKey, final Object data )
  {
    m_profilMetaData.put( metaDataKey, data );
  }

  /**
   * @see com.bce.eind.core.profilapi.ProfilApi#setRauheitTyp(java.lang.String)
   */
  public void setRauheitTyp( final RAUHEITEN_TYP r )
  {
    m_rauheit = r;

    // TODO: neuer event typ?
    // fireProfilDataChanged( ProfilPointProperty.RAUHEIT, r );
  }

  private void setValueFor( final IProfilPoint point, final ProfilPointProperty pointProperty,
      final double value ) throws ProfilDataException
  {
    setValues( new ProfilChange[]
    { new ProfilChange( point, pointProperty, value ) } );
  }

  private void setValuesFor( final List<IProfilPoint> pointList, ProfilPointProperty pointProperty,
      double value ) throws ProfilDataException
  {
    final List<ProfilChange> changes = new ArrayList<ProfilChange>( pointList.size() );
    for( final IProfilPoint point : pointList )
      changes.add( new ProfilChange( point, pointProperty, value ) );

    setValues( changes.toArray( new ProfilChange[changes.size()] ) );
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfil#setValuesFor(com.bce.eind.core.profildata.tabledata.ColumnKey,
   *      double)
   */
  private void setValuesFor( final ProfilPointProperty pointProperty, final double value )
      throws ProfilDataException
  {
    final List<IProfilPoint> allPoints = getPoints();
    setValuesFor( allPoints, pointProperty, value );
  }

  public void editBuilding( final ProfilBuildingProperty buildingProperty, final double value )
      throws ProfilBuildingException
  {
    if( !m_building.hasProperty(buildingProperty) )
      throw new ProfilBuildingException( "Eigenschaft exisitert nicht: " + buildingProperty );

    m_building.setValue( buildingProperty, value );
  }

  public IProfilPoint getPointCloseTo( final double breite ) throws ProfilDataException
  {
    IProfilPoint pkt = getPoint( 0 );

    for( final Iterator<IProfilPoint> ptIt = m_points.iterator(); ptIt.hasNext(); )
    {
      IProfilPoint p = ptIt.next();
      final double distance = Math.abs( breite - p.getValueFor( ProfilPointProperty.BREITE ) );
      if( Math.abs( pkt.getValueFor( ProfilPointProperty.BREITE ) - breite ) > distance )
        pkt = p;
    }
    return pkt;
  }

  public void setValues( final ProfilChange[] changes ) throws ProfilDataException
  {
    for( final ProfilChange change : changes )
    {
      final IProfilPoint point = change.getPoint();
      if( !profilPointExists( point ) )
        throw new ProfilDataException( "Profilpunkt exisitiert nicht: " + point );

      ((ProfilPoint)point).setValueFor( change.getColumn(), change.getNewValue() );
    }
  }

}