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

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class EditPointResolution extends AbstractProfilMarkerResolution
{
   private int m_index;

   private String m_propertyId;

   private double m_value;
   
  /**
   * @param deviderTyp,deviderIndex
   *            devider=IProfil.getDevider(deviderTyp)[deviderIndex]
   */
  public EditPointResolution( final int index, final IComponent property, final double value )
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.EditPointResolution.0" ,property.getName() ), null, null ); //$NON-NLS-1$ 
    m_index = index;
    m_propertyId = property.getId();
    m_value = value;
  }
  public EditPointResolution( final int index, final String propertyID, final double value )
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.EditPointResolution.0" ,propertyID ), null, null ); //$NON-NLS-1$ 
    m_index = index;
    m_propertyId = propertyID;
    m_value = value;
  }
  public EditPointResolution()
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.EditPointResolution.2"), null, null ); //$NON-NLS-1$
    m_index = -1;
    m_propertyId = ""; //$NON-NLS-1$
    m_value = Double.NaN;
  }
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
 @Override
public boolean resolve( final IProfil profil )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length == 0 )
      return false;
    final IRecord point = points[m_index];
    if( point == null )
      return false;
    final int comp = profil.indexOfProperty( m_propertyId );
    if( comp > -1 )
    {
      point.setValue( comp, m_value );
      return true;
    }
    return false;
  }
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
     return super.getSerializedParameter()+";"+m_index+";"+m_propertyId+";"+m_value; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( final String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      m_index = new Integer( params[1] );
      m_propertyId = params[2];
      m_value= new Double( params[3] );
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }

  }
}
