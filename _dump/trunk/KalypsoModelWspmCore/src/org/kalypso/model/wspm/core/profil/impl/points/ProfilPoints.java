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

  /**
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addPoint(double, double)
   */
  public final IProfilPoint addPoint( final double breite, final double hoehe )

  {
    final ProfilPoint point = new ProfilPoint();

    for( POINT_PROPERTY pp : m_pointProperties )
    {
      point.addProperty( pp );
    }
    point.setValueFor( POINT_PROPERTY.HOEHE, hoehe );
    point.setValueFor( POINT_PROPERTY.BREITE, breite );
    add( point );
    return point;
  }

  /**
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addPoint(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public final IProfilPoint addPoint( final IProfilPoint thePointBefore )
  {
    final int pktIndex = this.indexOf( thePointBefore ) + 1;
    if( pktIndex == 0 )
      return null;
    final ProfilPoint point = new ProfilPoint();
    for( POINT_PROPERTY pp : m_pointProperties )
    {
      point.addProperty( pp );
    }

    if( pktIndex < this.size() )
      this.add( pktIndex, point );
    else
      this.add( point );
    return point;
  }

  /**
   * add a new pointProperty with the default value 0.0 to all points of this List
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addProperty(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final void addProperty( final POINT_PROPERTY pointProperty )
  {
    if( propertyExists( pointProperty ) )
      return;
    for( IProfilPoint pp : this )
    {
      ((ProfilPoint) pp).addProperty( pointProperty );
    }
    m_pointProperties.add( pointProperty );

  }

  /**
   * returns an array of bundled pointProperties or an array of this pointProperty
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getDependenciesFor(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public POINT_PROPERTY[] getDependenciesFor( final POINT_PROPERTY pointProperty )
  {
    final POINT_PROPERTY[] dep = m_dependencies.get( pointProperty );
    return (dep == null) ? new POINT_PROPERTY[] { pointProperty } : dep;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getExistingProperties()
   */
  public final LinkedList<POINT_PROPERTY> getExistingProperties( )
  {
    return new UnmodifiableLinkedList<POINT_PROPERTY>( m_pointProperties );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#getVisibleProperties()
   */
  public final LinkedList<POINT_PROPERTY> getVisibleProperties( )
  {
    LinkedList<POINT_PROPERTY> visibleProperties = new LinkedList<POINT_PROPERTY>();
    for( POINT_PROPERTY pp : m_pointProperties )
    {
      if( (Boolean) pp.getParameter( PARAMETER.VISIBLE ) )
        visibleProperties.add( pp );
    }
    return visibleProperties;
  }

  /**
   * add an existing point to this list
   * 
   * @throws ProfilDataException
   *           if the containimg PointProperties are incompatible
   * @return the given point if this operation succed otherwise null
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
    for( POINT_PROPERTY pp : point.getProperties() )
    {
      if( !m_pointProperties.contains( pp ) )
      {
        throw new ProfilDataException( "ungültiger Punkt" );
      }
    }

    final int pktIndex = indexOf( thePointBefore ) + 1;
    if( pktIndex == 0 )
      return null;
    if( pktIndex < size() )
      add( pktIndex, point );
    else
      add( point );
    return point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#propertyExists(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final boolean propertyExists( final POINT_PROPERTY pointProperty )
  {
    return m_pointProperties.contains( pointProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#removePoint(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public final boolean removePoint( final IProfilPoint point )
  {
    return remove( point );
  }

  /**
   * remove the pointProperty from all points of this List
   * 
   * @return true if the given pointProperty is optional or does not exist in this List
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#removeProperty(org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY)
   */
  public final boolean removeProperty( final POINT_PROPERTY pointProperty )
  {
    if( !(Boolean) pointProperty.getParameter( PARAMETER.OPTIONAL ) )
      return false;
    if( !(propertyExists( pointProperty )) )
      return true;
    for( IProfilPoint pp : this )
    {
      ((ProfilPoint) pp).removeProperty( pointProperty );
    }
    m_pointProperties.remove( pointProperty );
    return true;
  }

  public LinkedList<IProfilPoint> getPoints( )
  {
    return m_unmodifiable;
  }

}
