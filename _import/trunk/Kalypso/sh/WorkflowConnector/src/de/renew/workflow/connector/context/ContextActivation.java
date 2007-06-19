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
package de.renew.workflow.connector.context;

import de.renew.workflow.contexts.ContextType;

/**
 * @author Stefan Kurzbach
 */
public class ContextActivation
{
  private final ContextActivation m_cause;

  private final ContextType m_context;

  private final Object m_result;

  public ContextActivation( final ContextType context, final Object result )
  {
    m_context = context;
    m_result = result;
    m_cause = null;
  }

  public ContextActivation( final ContextType context, final Object result, final ContextActivation cause )
  {
    m_context = context;
    m_result = result;
    m_cause = cause;
  }

  public ContextActivation getCause( )
  {
    return m_cause;
  }

  public ContextType getContext( )
  {
    return m_context;
  }

  public Object getResult( )
  {
    return m_result;
  }
}
