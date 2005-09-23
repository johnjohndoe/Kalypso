package com.bce.eind.core.profil.impl.points;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import org.bce.java.lang.UnmodifiableLinkedList;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.PARAMETER;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint>
{
  private final HashMap<POINT_PROPERTY, POINT_PROPERTY[]> m_dependencies = new HashMap<POINT_PROPERTY, POINT_PROPERTY[]>();

  private final LinkedList<POINT_PROPERTY> m_pointProperties = new LinkedList<POINT_PROPERTY>();

  private LinkedList<IProfilPoint> m_unmodifiable = 
    new UnmodifiableLinkedList<IProfilPoint>( this );

  public ProfilPoints( )
  {
    super();
    m_dependencies.put( POINT_PROPERTY.RECHTSWERT, new POINT_PROPERTY[]
    { POINT_PROPERTY.HOCHWERT } );
    m_dependencies.put( POINT_PROPERTY.HOCHWERT, new POINT_PROPERTY[]
    { POINT_PROPERTY.RECHTSWERT } );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_AX, new POINT_PROPERTY[]
    { POINT_PROPERTY.BEWUCHS_AY, POINT_PROPERTY.BEWUCHS_DP } );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_AY, new POINT_PROPERTY[]
    { POINT_PROPERTY.BEWUCHS_AX, POINT_PROPERTY.BEWUCHS_DP } );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_DP, new POINT_PROPERTY[]
    { POINT_PROPERTY.BEWUCHS_AY, POINT_PROPERTY.BEWUCHS_AY } );
  }

  /*
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   */
  public final IProfilPoint addPoint( final double breite, final double hoehe )

  {
    final ProfilPoint point = new ProfilPoint();

    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }

    if( point.setValueFor( POINT_PROPERTY.HOEHE, hoehe )
        & point.setValueFor( POINT_PROPERTY.BREITE, breite ) )
    {
      if( add( point ) )
        return point;
    }
    return null;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
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

  public POINT_PROPERTY[] getDependenciesFor( final POINT_PROPERTY pointProperty )
  {
    final POINT_PROPERTY[] dep = m_dependencies.get( pointProperty );
    return (dep == null) ? new POINT_PROPERTY[] {} : dep;
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
      if( (Boolean)pointProperty.getParameter( PARAMETER.VISIBLE ) )
        visibleProperties.add( pointProperty );
    }
    return visibleProperties;
  }

  public final boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException
  {
    if( m_pointProperties.size() != point.getProperties().size() )
      throw new ProfilDataException( "ungültiger Punkt" );
    for( final Iterator<POINT_PROPERTY> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
    {
      if( !m_pointProperties.contains( ppIt.next() ) )
      {
        throw new ProfilDataException( "ungültiger Punkt" );
      }
    }
    final int pktIndex = indexOf( thePointBefore ) + 1;
    if( pktIndex < size() )
      add( pktIndex, point );
    else
      add( point );
    return true;
  }

  /*
   * public final IProfilPoint getPoint( final double breite, final double hoehe ) { for( final
   * Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); ) { final ProfilPoint point =
   * (ProfilPoint)ptIt.next(); try { if( point.isPosition( breite, hoehe ) ) return point; } catch(
   * ProfilDataException e ) { e.printStackTrace(); } } return null; }
   */

  public final boolean propertyExists( final POINT_PROPERTY pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  public final boolean removePoint( final IProfilPoint point )
  {
    return this.remove( point );
  }

  public final boolean removeProperty( final POINT_PROPERTY pointProperty )
  {
    if( !(Boolean)pointProperty.getParameter( PARAMETER.OPTIONAL ) )
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

  public void setDependenciesFor( POINT_PROPERTY pointProperty, POINT_PROPERTY[] dependencies )
  {
    m_dependencies.put( pointProperty, dependencies );
  }

  public LinkedList<IProfilPoint> unmodifiable( )
  {
    return m_unmodifiable;
  }
}
