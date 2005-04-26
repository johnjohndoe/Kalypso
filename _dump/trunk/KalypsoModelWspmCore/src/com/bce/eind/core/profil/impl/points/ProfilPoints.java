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
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint>
{
  private final LinkedList<POINT_PROPERTY> m_pointProperties = new LinkedList<POINT_PROPERTY>();

  private List<IProfilPoint> m_unmodifiable = Collections.synchronizedList(Collections.unmodifiableList( this ));

  public List<IProfilPoint> unmodifiable( )
  {
    return m_unmodifiable;
  }

  public final LinkedList<POINT_PROPERTY> getExistingProperties( )
  {
    return m_pointProperties;
  }

  public final LinkedList<POINT_PROPERTY> getVisibleProperties( )
  {
    LinkedList<POINT_PROPERTY> visibleProperties = new LinkedList<POINT_PROPERTY>();
    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final POINT_PROPERTY pointProperty = ecIt.next();
      if( ProfilPointProperties.getPointProperty(pointProperty).isVisible() )
        visibleProperties.add( pointProperty );
    }
    return visibleProperties;
  }

  public final void addProperty( final POINT_PROPERTY pointProperty )
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

  public final boolean removeProperty( final POINT_PROPERTY pointProperty )
  {
    if( !ProfilPointProperties.getPointProperty(pointProperty).isOptional() )
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

  public final boolean propertyExists( final POINT_PROPERTY pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  public final IProfilPoint addPoint( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }
    point.setValueFor( POINT_PROPERTY.HOEHE, hoehe );
    point.setValueFor( POINT_PROPERTY.BREITE, breite );
    this.add( point );
    return point;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
      throws ProfilDataException
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
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
        if(point.isEqualPosition( breite,hoehe))
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
    for (final Iterator<POINT_PROPERTY> ppIt = point.getProperties().iterator();ppIt.hasNext();)
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
