/*----------------    FILE HEADER  ------------------------------------------

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

 Copyright (C) 2002 Wolfgang Baer - WBaer@gmx.de
 
 Adapted May 2003 by IDgis, The Netherlands - www.idgis.nl
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.rtree;

/**
 * <p>
 * The common exception thrown by problems during the rtree methods. Thats the
 * exception a user of the package get if errors occure.
 * 
 * </p>
 * 
 * @author Wolfgang Bär
 * @version 0.1
 */
public class RTreeException extends Exception
{
  /**
   * Constructor for RTreeException.
   */
  public RTreeException()
  {
    super();
  }

  /**
   * Constructor for RTreeException.
   * 
   * @param message
   */
  public RTreeException( String message )
  {
    super( message );
  }
}