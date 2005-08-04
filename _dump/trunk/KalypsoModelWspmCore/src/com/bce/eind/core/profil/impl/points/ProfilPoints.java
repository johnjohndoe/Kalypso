package com.bce.eind.core.profil.impl.points;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import org.bce.java.lang.UnmodifiableLinkedList;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint>
{
  private final HashMap<PointProperty, PointProperty[]> m_dependencies = new HashMap<PointProperty, PointProperty[]>();

  private final LinkedList<PointProperty> m_pointProperties = new LinkedList<PointProperty>();

  private LinkedList<IProfilPoint> m_unmodifiable = 
    new UnmodifiableLinkedList<IProfilPoint>( this );

  public ProfilPoints( )
  {
    super();
    m_dependencies.put( PointProperty.RECHTSWERT, new PointProperty[]
    { PointProperty.HOCHWERT } );
    m_dependencies.put( PointProperty.HOCHWERT, new PointProperty[]
    { PointProperty.RECHTSWERT } );
    m_dependencies.put( PointProperty.BEWUCHS_AX, new PointProperty[]
    { PointProperty.BEWUCHS_AY, PointProperty.BEWUCHS_DP } );
    m_dependencies.put( PointProperty.BEWUCHS_AY, new PointProperty[]
    { PointProperty.BEWUCHS_AX, PointProperty.BEWUCHS_DP } );
    m_dependencies.put( PointProperty.BEWUCHS_DP, new PointProperty[]
    { PointProperty.BEWUCHS_AY, PointProperty.BEWUCHS_AY } );
  }

  /*
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   */
  public final IProfilPoint addPoint( final double breite, final double hoehe )

  {
    final ProfilPoint point = new ProfilPoint();

    for( final Iterator<PointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }

    if( point.setValueFor( PointProperty.HOEHE, hoehe )
        & point.setValueFor( PointProperty.BREITE, breite ) )
    {
      if( add( point ) )
        return point;
    }
    return null;
  }

  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<PointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
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

  public final void addProperty( final PointProperty pointProperty )
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

  public PointProperty[] getDependenciesFor( final PointProperty pointProperty )
  {
    final PointProperty[] dep = m_dependencies.get( pointProperty );
    return (dep == null) ? new PointProperty[] {} : dep;
  }

  public final LinkedList<PointProperty> getExistingProperties( )
  {
    return m_pointProperties;
  }

  public final LinkedList<PointProperty> getVisibleProperties( )
  {
    LinkedList<PointProperty> visibleProperties = new LinkedList<PointProperty>();
    for( final Iterator<PointProperty> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final PointProperty pointProperty = ecIt.next();
      if( (Boolean)pointProperty.getParameter( PointProperty.PARAMETER.VISIBLE ) )
        visibleProperties.add( pointProperty );
    }
    return visibleProperties;
  }

  public final boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException
  {
    if( m_pointProperties.size() != point.getProperties().size() )
      throw new ProfilDataException( "ungültiger Punkt" );
    for( final Iterator<PointProperty> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
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

  public final boolean propertyExists( final PointProperty pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  public final boolean removePoint( final IProfilPoint point )
  {
    return this.remove( point );
  }

  public final boolean removeProperty( final PointProperty pointProperty )
  {
    if( !(Boolean)pointProperty.getParameter( PointProperty.PARAMETER.OPTIONAL ) )
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

  public void setDependenciesFor( PointProperty pointProperty, PointProperty[] dependencies )
  {
    m_dependencies.put( pointProperty, dependencies );
  }

  public LinkedList<IProfilPoint> unmodifiable( )
  {
    return m_unmodifiable;
  }
}
