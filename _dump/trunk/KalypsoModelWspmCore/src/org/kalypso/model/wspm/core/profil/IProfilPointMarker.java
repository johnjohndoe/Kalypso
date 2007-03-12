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

public interface IProfilPointMarker
{
  /**
   * @return the stored objectValue belongs to this key
   * @throws IllegalArgumentException
   *           if the key is not supported
   * @see setValueFor(key,value)
   */
  public Object getValueFor( final String key ) throws IllegalArgumentException;

  /**
   * @throws IllegalArgumentException
   *           if the key is not supported
   *           <p>
   *           otherwise overwrite the stored object with the given value
   */
  public void setValueFor( final String key, final Object value ) throws IllegalArgumentException;

  /**
   * @return all the keys supported by this pointMarker
   */
  public String[] getKeys( );

  /**
   * @return the ProfilePoint captured by this pointMarker
   * @see {@link IProfil.getPointMarkerFor(point)}
   */
  public IProfilPoint getPoint( );

  /**
   * capture the profilePoint
   * 
   * @return the profilePoint captured before, may be null
   * @see {@link IProfil.getPointMarkerFor(point)}
   */
  public IProfilPoint setPoint( final IProfilPoint point );

  /**
   * Muss dictionary id sein (und zwar see:ItemDefinition's)!
   */
  public String getMarkerId( );

  /**
   * Returns the value of this marker as it should be written into gml. Must fit to the type defined in the component
   * ItemDefinition
   * 
   * @return May not return <code>null</code>.
   * @see #getMarkerId()
   */
  public Object getGmlObject( );

  /**
   * @return a friendly name for this PointMarker should be the same used in the dictonary
   */
  public String getMarkerLabel( );

}
