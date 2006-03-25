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
package org.kalypsodeegree_impl.model.feature.xpath;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author doemming
 */
public abstract class Function implements IFunction
{

  private final String m_name;

  private Pattern m_pattern;

  public Function(final  String name )
  {
    m_name = name;
    m_pattern = Pattern.compile( "^(.*" + m_name + ")\\((.*?)\\)" );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.path.IFunction#getPattern()
   */
  /**
   * @see org.kalypsodeegree_impl.model.feature.path.IFunction#getArgument(java.util.regex.Matcher,
   *      org.kalypsodeegree_impl.model.feature.path.Cond)
   */
  public String getArgument( Matcher matcher, Cond cond )
  {
    final String prefix = matcher.group( 1 );
    String condition = cond.getCond();
    final String argument = condition.substring( prefix.length() +1, condition.length() - 1 );
    return argument;
  }

  public Pattern getPattern( )
  {
    return m_pattern;
  }

}
