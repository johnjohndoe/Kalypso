/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.gmlxpath;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Abstract GMLXPathFunction<br>
 * inteded to be subclasses by implementors of GMLXPathFunctions
 * 
 * @author doemming
 */
public abstract class AbstractGMLXPathFunction implements IGMLXPathFunction
{

  private final String m_name;

  private Pattern m_pattern;

  public AbstractGMLXPathFunction( final String name )
  {
    m_name = name;
    m_pattern = Pattern.compile( "^(.*" + m_name + ")\\((.*?)\\).*" );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.path.IFunction#getArgument(java.util.regex.Matcher,
   *      org.kalypsodeegree_impl.model.feature.path.Cond)
   */
  public String getArgument( Matcher matcher, GMLXPathString cond )
  {
    final String condition = cond.getCond();
    final int start = matcher.start( 2 );
    final int end = matcher.end( 2 );
    final String argument = condition.substring( start, end );
    return argument;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.IFunction#getPattern()
   */
  public Pattern getPattern( )
  {
    return m_pattern;
  }

}
