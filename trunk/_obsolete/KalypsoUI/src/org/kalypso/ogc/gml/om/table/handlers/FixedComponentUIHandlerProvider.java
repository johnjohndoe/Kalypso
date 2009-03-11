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
package org.kalypso.ogc.gml.om.table.handlers;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.observation.result.TupleResult;

/**
 * @author Dirk Kuch
 * @author Gernot Belger
 */
public class FixedComponentUIHandlerProvider implements IComponentUiHandlerProvider
{
  private final Map<Integer, IComponentUiHandler> m_handler = new LinkedHashMap<Integer, IComponentUiHandler>();

  private final Map<Integer, IComponentUiHandler> m_unmodHandler = Collections.unmodifiableMap( m_handler );

  public void add( final int index, final IComponentUiHandler handler )
  {
    m_handler.put( index, handler );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider#createComponentHandler(org.kalypso.observation.result.TupleResult)
   */
  public Map<Integer, IComponentUiHandler> createComponentHandler( final TupleResult tupleResult )
  {
    return m_unmodHandler;
  }

}
