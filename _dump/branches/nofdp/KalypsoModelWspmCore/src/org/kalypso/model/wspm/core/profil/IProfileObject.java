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

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * HACK: WSPM Buildings always consists of one IRecord-Set in TupleResult - so we have only getValue(comp) and
 * setValue(comp)
 * 
 * @author kimwerner
 */
public interface IProfileObject
{
  /**
   * @return the ProfilePointProperties used by this Object
   */
  public IComponent[] getPointProperties( );

  /**
   * @return the keys this Object held as a key,value
   * @see getValueFor(key)
   */
  public IComponent[] getObjectProperties( );

  public Object getValue( IComponent component );
  
  public Object getValueFor( String componentID );

  public void setValue( IComponent component, Object value );
  
  public void setValueFor( String componentID, Object value );

  public IObservation<TupleResult> getObservation( );

  public String getId( );
  
  public IComponent getObjectProperty(final String componentId);
}
