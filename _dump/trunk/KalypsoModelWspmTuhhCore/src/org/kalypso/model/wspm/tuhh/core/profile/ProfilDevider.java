/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author kimwerner
 */
public class ProfilDevider implements IProfilPointMarker
{
  final private String m_Typ;

  final private HashMap<String, Object> m_values;

  IProfilPoint m_point = null;

  private final String m_label;

  public ProfilDevider( final String typ, final String label, final String[] keys )
  {
    m_Typ = typ;
    m_label = label;
    m_values = new HashMap<String, Object>();
    if( keys.length != 0 )
    {
      for( final String key : keys )
      {
        m_values.put( key, null );
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getPoint()
   */
  public IProfilPoint getPoint( )
  {
    return m_point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getTyp()
   */
  public String getMarkerId( )
  {
    return m_Typ;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getValueFor(java.lang.String)
   */
  public Object getValueFor( String key )
  {
    return m_values.get( key );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#setPoint(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public IProfilPoint setPoint( IProfilPoint point )
  {
    final IProfilPoint oldPoint = m_point;
    m_point = point;
    return oldPoint;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#setValuesFor(java.lang.String, java.lang.Object)
   */
  public void setValueFor( String key, Object value )
  {
    if( m_values.containsKey( key ) )
      m_values.put( key, value );
    else
      throw new IllegalArgumentException();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getKeys()
   */
  public Collection<String> getKeys( )
  {
    return Collections.unmodifiableCollection( m_values.keySet() );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getMarkerLabel()
   */
  public String getMarkerLabel( )
  {
    return m_label;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getGmlObject()
   */
  public Object getGmlObject( )
  {
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( getMarkerId() ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( getMarkerId() ) )
    {
      final Boolean pos = (Boolean) getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BOESCHUNG );
      return pos == null || pos.booleanValue() ? "high" : "low";
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( getMarkerId() ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( getMarkerId() ) )
      return getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT );

    throw new UnsupportedOperationException( "Unknown devider type: " + this );
  }

}
