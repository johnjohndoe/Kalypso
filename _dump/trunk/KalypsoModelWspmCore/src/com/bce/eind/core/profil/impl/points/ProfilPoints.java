/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint>
{
  private final LinkedList<ProfilPointProperty> m_pointProperties = new LinkedList<ProfilPointProperty>();

  private List<IProfilPoint> m_unmodifiable = Collections.synchronizedList(Collections.unmodifiableList( this ));

  public List<IProfilPoint> unmodifiable( )
  {
    return m_unmodifiable;
  }

  public final LinkedList<ProfilPointProperty> getExistingColumns( )
  {
    return m_pointProperties;
  }

  public final LinkedList<ProfilPointProperty> getVisibleColumns( )
  {
    LinkedList<ProfilPointProperty> visibleColumns = new LinkedList<ProfilPointProperty>();
    for( final Iterator<ProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final ProfilPointProperty columnKey = ecIt.next();
      if( columnKey.isVisible() )
        visibleColumns.add( columnKey );
    }
    return visibleColumns;
  }

  public final void addColumn( final ProfilPointProperty columnKey )
  {
    if( columnExists( columnKey ) )
      return;
    for( final Iterator ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final ProfilPoint pt = (ProfilPoint)ptIt.next();
      pt.addColumn( columnKey );
    }
    m_pointProperties.add( columnKey );

  }

  public final boolean removeColumn( final ProfilPointProperty columnKey )
  {
    if( !columnKey.isOptional() )
      return false;
    if( !(columnExists( columnKey )) )
      return true;
    for( final Iterator ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final ProfilPoint pt = (ProfilPoint)ptIt.next();
      pt.removeColumn( columnKey );
    }
    m_pointProperties.remove( columnKey );
    return true;
  }

  public final boolean columnExists( final ProfilPointProperty columnKey )
  {
    return m_pointProperties.contains( columnKey );
  }

  public final IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addColumn( (ProfilPointProperty)ecIt.next() );
    }
    point.setValueFor( IProfil.HOEHE, hoehe );
    point.setValueFor( IProfil.BREITE, breite );
    this.add( point );
    return point;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addColumn( (ProfilPointProperty)ecIt.next() );
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
    for( final Iterator ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final ProfilPoint point = (ProfilPoint)ptIt.next();
      try
      {
        if( (point.getValueFor( IProfil.HOEHE ) == hoehe)
            & (point.getValueFor( IProfil.BREITE ) == breite) )
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
