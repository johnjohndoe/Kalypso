/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.contribs.java.lang;

import java.lang.reflect.InvocationTargetException;

/**
 * Supplies clients with objects based on a request. This simple interface is used to provide flexibility in the design
 * of an API. For instance, you might use a supplier as initialiser for an other object, but want to keep the interface
 * clean and extensible. Let it be a supplier, and the public interface won't change, but you are free to extend the
 * body of the supply method.
 * <p>
 * This interface has similarities with the IAdaptable interface from Eclipse, but with more flexibility in the argument
 * since it is an object, not a class.
 * 
 * @author schlienger
 */
public interface ISupplier
{
  /**
   * Supply the caller with a potential object for the given request, or return null if the request cannot be
   * fullfilled.
   * 
   * @param request
   *          some object used to idenfity the request
   * @return some object as response to that request, might be null
   * @throws InvocationTargetException
   *           if an error occured while answering the request
   */
  public Object supply( final Object request ) throws InvocationTargetException;
}
