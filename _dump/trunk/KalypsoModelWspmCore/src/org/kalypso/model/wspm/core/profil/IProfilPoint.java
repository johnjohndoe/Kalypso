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
package org.kalypso.model.wspm.core.profil;

/**
 * @author kimwerner
 */
public interface IProfilPoint
{
  /**
   * @return doubleValue stored with its corresponding profilPointPropertyId
   * @see {@link IProfilPointProperty.getId()}
   * @throws IllegalArgumentException
   *           if the pointProperty is not supported
   * @see #hasProperty(String)
   * @see #addProperty(String)
   * @see #getProperties()
   */
  public double getValueFor( final String pointPropertyId ) throws IllegalArgumentException;

  /**
   * @return false if the pointPropertyId is not a valid key in the underlying HashMap
   * @see #hasProperty(String)
   * @see #addProperty(String)
   * @see #getProperties()
   */
  public boolean setValueFor( final String pointPropertyId, final double value );

  /**
   * @return false if the pointPropertyId is not a valid key in the underlying HashMap
   */
  public boolean hasProperty( final String pointPropertyId );

  /**
   * adds the pointPropertyId as a valid key to the underlying HashMap
   */
  public void addProperty( final String pointPropertyId );

  /**
   * @return a Copy of this point, addable its profile
   * @see {@link IProfil.addPoint(point)}
   */
  public IProfilPoint clonePoint( );

  /**
   * @return all supperted ProfilPointProperties 
   * @see #hasProperty(String)
   */
  public String[] getProperties( );
}
