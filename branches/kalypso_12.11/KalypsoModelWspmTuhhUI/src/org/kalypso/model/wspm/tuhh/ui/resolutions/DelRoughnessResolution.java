/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class DelRoughnessResolution extends AbstractProfilMarkerResolution
{
  private boolean m_initialized = false;

  private String m_roughness;

  public DelRoughnessResolution( )
  {
    this( null );

    m_initialized = false;
  }

  public DelRoughnessResolution( final String roughnessToDelete )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.DelRoughnessResolution.0" ), null, null ); //$NON-NLS-1$

    m_roughness = roughnessToDelete;
    m_initialized = true;
  }

  @Override
  public String getSerializedParameter( )
  {
    final StringBuilder params = new StringBuilder( super.getSerializedParameter() );
    params.append( ';' );

    params.append( m_roughness );

    return params.toString();
  }

  @Override
  public boolean resolve( final IProfile profil )
  {
    if( m_initialized )
    {
      if( m_roughness == null )
        return false;

      final IComponent comp = profil.hasPointProperty( m_roughness );
      if( comp != null )
        profil.removePointProperty( comp );

      return true;
    }

    throw new IllegalStateException();
  }

  @Override
  public void setData( final String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      m_roughness = params[1];

      m_initialized = true;
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }
}
