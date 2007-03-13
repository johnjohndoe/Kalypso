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
package org.kalypso.model.wspm.core.profil;

/**
 * @author kimwerner
 */
public interface IProfileObject
{

  public String getId( );

  /**
   * @return the ProfilePointProperties used by this Object
   */
  public String[] getPointProperties( );

  /**
   * @return the keys this Object held as a key,value
   * @see getValueFor(key)
   */
  public String[] getObjectProperties( );

  /**
   * @return the value this Object held as a key,value
   * @throws IllegalArgumentException
   *           if the key is not supported by this Object
   * @see hasProperty(key)
   * @see setValue(key,value)
   */
  public Object getValueFor( final String key ) throws IllegalArgumentException;

  /**
   * @return a friendly Name for the given key used as key,value, stored in this ProfileObject
   */
  public String getLabelFor( final String key );

  /**
   * @return the added Object
   * @throws IllegalArgumentException
   *           if the key is not supported by this Object
   * @see hasProperty(key)
   * @see getValueFor(key)
   */
  public Object setValue( final String key, final Object value ) throws IllegalArgumentException;

  /**
   * @return true if the Object support the given key
   * @see setValue(key,value)
   * @see getValueFor(key)
   */
 public boolean hasProperty( final String key );

  public String getName( );

}
