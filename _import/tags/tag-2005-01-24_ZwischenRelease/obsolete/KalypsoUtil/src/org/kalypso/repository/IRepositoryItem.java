/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.repository;

import org.kalypso.util.adapter.IAdaptable;

/**
 * Ein Item innerhalb des Repository. Die Items sind hierarchisch organisiert.
 * 
 * @author schlienger
 */
public interface IRepositoryItem extends IAdaptable
{
  /**
   * Returns the item's name. The name is used to display the item in the
   * repository for instance.
   * 
   * @return name
   */
  public String getName( );

  /**
   * Returns a unique identifier for this item. The identifier should be build
   * using the following rule (URL oriented):
   * <p>
   * 
   * <pre>
   * 
   *  id(item) = rep-id:/item-id
   *  
   * </pre>
   * 
   * <p>
   * Thus, the item's id is made of the id of the item's repository id plus its
   * own id. The identifier should be build according to the URL specification.
   * 
   * @return identifier
   */
  public String getIdentifier( );

  /**
   * returns the parent item to which this one belongs
   * 
   * @return parent item or null if no parent
   * @throws RepositoryException
   */
  public IRepositoryItem getParent( ) throws RepositoryException;

  /**
   * returns true when this item has children
   * 
   * @return hasChildren flag
   * @throws RepositoryException
   */
  public boolean hasChildren( ) throws RepositoryException;

  /**
   * returns the children of this item
   * 
   * @return array of items
   * @throws RepositoryException
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException;

  /**
   * returns the repository into which this item exists
   * 
   * @return repository
   */
  public IRepository getRepository( );
}