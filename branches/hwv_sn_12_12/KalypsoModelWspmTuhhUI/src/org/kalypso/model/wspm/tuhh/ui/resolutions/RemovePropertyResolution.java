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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class RemovePropertyResolution extends AbstractProfilMarkerResolution
{
  private static final String STR_REMOVE_COMPONENT = Messages.getString("RemovePropertyResolution_0"); //$NON-NLS-1$

  private static final String STR_REMOVE_COMPONENT_DESCRIPTION = Messages.getString("RemovePropertyResolution_1"); //$NON-NLS-1$

  private int m_componentIndex;

  public RemovePropertyResolution( final int componentIndex )
  {
    super( STR_REMOVE_COMPONENT, STR_REMOVE_COMPONENT_DESCRIPTION, null );
    m_componentIndex = componentIndex;
  }

  public RemovePropertyResolution( )
  {
    this( -1 );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public boolean resolve( final IProfil profile )
  {
    final IComponent component = profile.getResult().getComponent( m_componentIndex );
    final String componentLabel = ComponentUtilities.getComponentLabel( component );
    final String opMsg = String.format( Messages.getString("RemovePropertyResolution_2"), componentLabel ); //$NON-NLS-1$

    final ProfilOperation operation = new ProfilOperation( opMsg, profile, true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyRemove( profile, component ) );
    new ProfilOperationJob( operation ).schedule();

    return true; // profile.getResult().removeComponent( m_componentIndex );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    // FIXME: bad API: this implementation details should be hidden in abstract implementation (at least)
    return super.getSerializedParameter() + ";" + m_componentIndex; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( final String parameterStream )
  {
    // FIXME: bad API: this implementation details should be hidden in abstract implementation (at least)
    final String[] params = getParameter( parameterStream );
    try
    {
      m_componentIndex = new Integer( params[1] ).intValue();
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }

}
