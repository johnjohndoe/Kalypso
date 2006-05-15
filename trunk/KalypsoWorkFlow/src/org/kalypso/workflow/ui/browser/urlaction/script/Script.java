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
package org.kalypso.workflow.ui.browser.urlaction.script;

import java.util.HashMap;
import java.util.Iterator;

/**
 * @author doemming
 */
public class Script
{
  private final String m_scriptType;

  private final HashMap<String, Function> m_functions = new HashMap<String, Function>();

  public Script( final String scriptType )
  {
    m_scriptType = scriptType;
  }

  public String getScriptType( )
  {
    return m_scriptType;
  }

  public void addFunction( final Function function )
  {
    m_functions.put( function.getName(), function );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer result = new StringBuffer();
    result.append( "script type=" ).append( m_scriptType ).append( "\n" );
    final Iterator<Function> iterator = m_functions.values().iterator();
    while( iterator.hasNext() )
    {
      final Function function = iterator.next();
      result.append( function.toString() );
    }
    return result.toString();
  }

  public Function getFunction( String functionName )
  {
    return m_functions.get( functionName );
  }
}
