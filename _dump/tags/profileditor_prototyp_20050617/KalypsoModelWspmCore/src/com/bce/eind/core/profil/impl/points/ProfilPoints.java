/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilPointProperty;

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

  public final LinkedList<ProfilPointProperty> getExistingProperties( )
  {
    return m_pointProperties;
  }

  public final LinkedList<ProfilPointProperty> getVisibleProperties( )
  {
    LinkedList<ProfilPointProperty> visibleProperties = new LinkedList<ProfilPointProperty>();
    for( final Iterator<ProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final ProfilPointProperty pointProperty = ecIt.next();
      if( pointProperty.isVisible() )
        visibleProperties.add( pointProperty );
    }
    return visibleProperties;
  }

  public final void addProperty( final ProfilPointProperty pointProperty )
  {
    if( propertyExists( pointProperty ) )
      return;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint)pt).addProperty( pointProperty );
    }
    m_pointProperties.add( pointProperty );

  }

  public final boolean removeProperty( final ProfilPointProperty pointProperty )
  {
    if( !pointProperty.isOptional() )
      return false;
    if( !(propertyExists( pointProperty )) )
      return true;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint)pt).removeProperty( pointProperty );
    }
    m_pointProperties.remove( pointProperty );
    return true;
  }

  public final boolean propertyExists( final ProfilPointProperty pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  public final IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<ProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }
    point.setValueFor( ProfilPointProperty.HOEHE, hoehe );
    point.setValueFor( ProfilPointProperty.BREITE, breite );
    this.add( point );
    return point;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<ProfilPointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
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
      final ProfilPoint point = (ProfilPoint)ptIt.next();
      try
      {
        if(point.isPosition( breite,hoehe))
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
  public final boolean insertPoint(final IProfilPoint thePointBefore, final IProfilPoint point ) throws ProfilDataException
  {
    if (m_pointProperties.size()!= point.getProperties().size())
      throw new ProfilDataException("ungültiger Punkt");
    for (final Iterator<ProfilPointProperty> ppIt = point.getProperties().iterator();ppIt.hasNext();)
    {
      if(!m_pointProperties.contains(ppIt.next()))
      {
        throw new ProfilDataException("ungültiger Punkt");
      }
    }
    final int pktIndex = indexOf( thePointBefore ) + 1;
    if( pktIndex < size() )
      add( pktIndex, point );
    else
      add( point );
    return true;
  }
}
