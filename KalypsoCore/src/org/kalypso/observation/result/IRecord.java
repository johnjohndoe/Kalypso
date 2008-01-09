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
package org.kalypso.observation.result;

/**
 * A record holds the values for the components of a tuple result.
 * 
 * @author schlienger
 */
public interface IRecord
{
  /**
   * @return the <code>TupleResult</code> that owns this record
   */
  public TupleResult getOwner( );

  /**
   * @return the value for the given component (can be null)
   * @throws IllegalArgumentException
   *             if the component is unknown to this record
   */
  public Object getValue( IComponent comp );

  /**
   * Sets the value for the given component<br>
   * ATTENTION: only ment to be called by {@link TupleResult} itself! Else sorting may fail...<br>
   * TODO: check if this could be made default visible, or if the record may inform its owner to sort itself... (make
   * sure its not called twice)
   * 
   * @throws IllegalArgumentException
   *             if the component is unknown to this record
   */
  public void setValue( IComponent comp, Object value );
}
