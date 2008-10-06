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

/**
 * Abstract GMLXPathOperation intended to be subclassed by implementors of methematical XPath operations
 * 
 * @author doemming
 */
public abstract class GMLXPathMathOperation extends GMLXPathOperation implements IGMLXPathOperation
{
  public GMLXPathMathOperation( String pattern )
  {
    super( pattern );
  }

  public Object operate( Object value1, Object value2 ) throws GMLXPathException
  {
    final double n1;
    if( value1 instanceof Number )
      n1 = ((Number) value1).doubleValue();
    else if( value1 instanceof String )
      n1 = Double.valueOf( (String) value1 );
    else
      throw new GMLXPathException();
    final double n2;
    if( value2 instanceof Number )
      n2 = ((Number) value2).doubleValue();
    else if( value1 instanceof String )
      n2 = Double.valueOf( (String) value2 );
    else
      throw new GMLXPathException();
    return mathOperate( n1, n2 );
  }

  public abstract Object mathOperate( double n1, double n2 );
}
