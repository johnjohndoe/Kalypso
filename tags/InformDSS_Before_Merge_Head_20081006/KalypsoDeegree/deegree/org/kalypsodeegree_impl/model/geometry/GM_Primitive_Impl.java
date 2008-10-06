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
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;
import java.lang.reflect.Array;

import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Primitive;

/**
 * default implementation of the GM_Primitive interface from package jago.model.
 * ------------------------------------------------------------
 * 
 * @version 8.6.2001
 * @author Andreas Poth
 */
abstract class GM_Primitive_Impl extends GM_Object_Impl implements GM_Primitive, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2121656681131551613L;

  /**
   * Creates a new GM_Primitive_Impl object.
   * 
   * @param crs
   */
  protected GM_Primitive_Impl( final String crs )
  {
    super( crs );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    /* A primitive adapts to the array of itself adapted to the desired class. */
    final Class< ? > componentType = adapter.getComponentType();
    if( componentType != null && GM_Object.class.isAssignableFrom( componentType ) )
    {
      final Object adaptedObject = getAdapter( componentType );
      if( adaptedObject == null )
        return null;

      final Object adaptedArray = Array.newInstance( componentType, 1 );
      Array.set( adaptedArray, 0, adaptedObject );

      return adaptedArray;
    }

    return super.getAdapter( adapter );
  }

}