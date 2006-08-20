/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil.impl.points;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import org.kalypso.contribs.java.lang.UnmodifiableLinkedList;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class ProfilPoints extends LinkedList<IProfilPoint> implements IProfilPoints
{
  private final HashMap<POINT_PROPERTY, POINT_PROPERTY[]> m_dependencies = new HashMap<POINT_PROPERTY, POINT_PROPERTY[]>();

  private final LinkedList<POINT_PROPERTY> m_pointProperties = new LinkedList<POINT_PROPERTY>();

  private LinkedList<IProfilPoint> m_unmodifiable = new UnmodifiableLinkedList<IProfilPoint>( this );

  public ProfilPoints( )
  {
    super();
    setDependencies();
  }

  private void setDependencies( )
  {
    final POINT_PROPERTY[] bewuchs = new POINT_PROPERTY[] { POINT_PROPERTY.BEWUCHS_AX, POINT_PROPERTY.BEWUCHS_AY, POINT_PROPERTY.BEWUCHS_DP };
    final POINT_PROPERTY[] geoCoord = new POINT_PROPERTY[] { POINT_PROPERTY.HOCHWERT, POINT_PROPERTY.RECHTSWERT };
    m_dependencies.put( POINT_PROPERTY.RECHTSWERT, geoCoord );
    m_dependencies.put( POINT_PROPERTY.HOCHWERT, geoCoord );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_AX, bewuchs );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_AY, bewuchs );
    m_dependencies.put( POINT_PROPERTY.BEWUCHS_DP, bewuchs );
  }

  /*
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   */
  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addPoint(double, double)
   */
  public final IProfilPoint addPoint( final double breite, final double hoehe )

  {
    final ProfilPoint point = new ProfilPoint();

    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      point.addProperty( ecIt.next() );
    }

    if( point.setValueFor( POINT_PROPERTY.HOEHE, hoehe ) & point.setValueFor( POINT_PROPERTY.BREITE, breite ) )
    {
      if( add( point ) )
        return point;
    }
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addPoint(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
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

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addProperty(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final void addProperty( final POINT_PROPERTY pointProperty )
  {
    if( propertyExists( pointProperty ) )
      return;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint) pt).addProperty( pointProperty );
    }
    m_pointProperties.add( pointProperty );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getDependenciesFor(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] getDependenciesFor( final POINT_PROPERTY pointProperty )
  {
    final POINT_PROPERTY[] dep = m_dependencies.get( pointProperty );
    return (dep == null) ? new POINT_PROPERTY[] { pointProperty } : dep;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getExistingProperties()
   */
  public final LinkedList<POINT_PROPERTY> getExistingProperties( )
  {
    return m_pointProperties;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getVisibleProperties()
   */
  public final LinkedList<POINT_PROPERTY> getVisibleProperties( )
  {
    LinkedList<POINT_PROPERTY> visibleProperties = new LinkedList<POINT_PROPERTY>();
    for( final Iterator<POINT_PROPERTY> ecIt = m_pointProperties.iterator(); ecIt.hasNext(); )
    {
      final POINT_PROPERTY pointProperty = ecIt.next();
      if( (Boolean) pointProperty.getParameter( PARAMETER.VISIBLE ) )
        visibleProperties.add( pointProperty );
    }
    return visibleProperties;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#insertPoint(org.kalypso.model.wspm.core.profil.IProfilPoint,
   *      org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public final IProfilPoint insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point ) throws ProfilDataException
  {
    if( point == null )
    {
      return addPoint( 0.0, 0.0 );
    }
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
    return point;
  }

  /*
   * public final IProfilPoint getPoint( final double breite, final double hoehe ) { for( final Iterator<IProfilPoint>
   * ptIt = this.iterator(); ptIt.hasNext(); ) { final ProfilPoint point = (ProfilPoint)ptIt.next(); try { if(
   * point.isPosition( breite, hoehe ) ) return point; } catch( ProfilDataException e ) { e.printStackTrace(); } }
   * return null; }
   */

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#propertyExists(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final boolean propertyExists( final POINT_PROPERTY pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#removePoint(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public final boolean removePoint( final IProfilPoint point )
  {
    return remove( point );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#removeProperty(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final boolean removeProperty( final POINT_PROPERTY pointProperty )
  {
    if( !(Boolean) pointProperty.getParameter( PARAMETER.OPTIONAL ) )
      return false;
    if( !(propertyExists( pointProperty )) )
      return true;
    for( final Iterator<IProfilPoint> ptIt = this.iterator(); ptIt.hasNext(); )
    {
      final IProfilPoint pt = ptIt.next();
      ((ProfilPoint) pt).removeProperty( pointProperty );
    }
    m_pointProperties.remove( pointProperty );
    return true;
  }

  // public void setDependenciesFor( POINT_PROPERTY pointProperty, POINT_PROPERTY[] dependencies )
  // {
  // m_dependencies.put( pointProperty, dependencies );
  // }

  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_unmodifiable;
  }

}
