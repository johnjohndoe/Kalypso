/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPointProperties;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint>
{
  private final LinkedList<IProfilPointProperty> m_pointProperties = new LinkedList<IProfilPointProperty>();

  private List<IProfilPoint> m_unmodifiable = Collections.synchronizedList(Collections.unmodifiableList( this ));

  public List<IProfilPoint> unmodifiable( )
  {
    return m_unmodifiable;
  }

  public final LinkedList<IProfilPointProperty> getExistingProperties( )
  {
    return m_pointProperties;
  }

  public final LinkedList<IProfilPointProperty> getVisibleProperties( )
  {
    LinkedList<IProfilPointProperty> visibleColumns = new LinkedList<IProfilPointProperty>();
    for( final Iterator<IProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final IProfilPointProperty columnKey = ecIt.next();
      if( columnKey.isVisible() )
        visibleColumns.add( columnKey );
    }
    return visibleColumns;
  }

  public final void addProperty( final IProfilPointProperty columnKey )
  {
    if( propertyExists( columnKey ) )
      return;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint)pt).addProperty( columnKey );
    }
    m_pointProperties.add( columnKey );

  }

  public final boolean removeProperty( final IProfilPointProperty columnKey )
  {
    if( !columnKey.isOptional() )
      return false;
    if( !(propertyExists( columnKey )) )
      return true;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint)pt).removeProperty( columnKey );
    }
    m_pointProperties.remove( columnKey );
    return true;
  }

  public final boolean propertyExists( final IProfilPointProperty columnKey )
  {
    return m_pointProperties.contains( columnKey );
  }

  public final IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<IProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }
    point.setValueFor( IProfilPointProperties.HOEHE, hoehe );
    point.setValueFor( IProfilPointProperties.BREITE, breite );
    this.add( point );
    return point;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<IProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }
    final int pktIndex = this.indexOf( thePointBefore ) + 1;
    if( pktIndex < this.size() )
      this.add( pktIndex, point );
    else
      this.add( point );
    return point;
  }

  public final IProfilPoint getPoint( final double breite, final double hoehe )
  {
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint point = ptIt.next();
      try
      {
        if( (point.getValueFor( IProfilPointProperties.HOEHE ) == hoehe)
            & (point.getValueFor( IProfilPointProperties.BREITE ) == breite) )
          return point;
      }
      catch( ProfilDataException e )
      {
        e.printStackTrace();
      }
    }
    return null;
  }

  public final boolean removePoint( final IProfilPoint point )
  {
    return this.remove( point );
  }
}
