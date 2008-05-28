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
package org.kalypsodeegree_impl.model.feature.gmlxpath.xelement;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.IGMLXPathFunction;

/**
 * xelement that represents a functional-xpath element
 * 
 * @author doemming
 */
public class XElementFromFunction extends AbstractXElement
{

  private final IGMLXPathFunction m_function;

  private final IXElement m_argumentXElement;

  public XElementFromFunction( IGMLXPathFunction function )
  {
    this( function, null );
  }

  public XElementFromFunction( IGMLXPathFunction function, IXElement argumentXElement )
  {
    m_function = function;
    m_argumentXElement = argumentXElement;

  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateOther(java.lang.Object, boolean)
   */
  @Override
  public Object evaluateOther( Object context, boolean featureTypeLevel ) throws GMLXPathException
  {
    if( context instanceof GMLWorkspace )
    {
      final GMLWorkspace gmlWorkspace = (GMLWorkspace) context;
      return evaluateFeature( gmlWorkspace.getRootFeature(), featureTypeLevel );
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateFeature(org.kalypsodeegree.model.feature.Feature,
   *      boolean)
   */
  @Override
  public Object evaluateFeature( Feature contextFeature, boolean featureTypeLevel ) throws GMLXPathException
  {
    return m_function.evaluate( contextFeature, m_argumentXElement, featureTypeLevel );
  }

}
