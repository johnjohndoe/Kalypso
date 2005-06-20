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
package org.kalypso.repository.virtual;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

/**
 * VirtualRepositoryFactory. Configuration should be built according to the following rules:
 * <p>
 * <ul>
 * <li>name: a unique identifier (also used as the name of the repository) to identify the repository
 * <li>conf: the location of the config file for the repository (this string is used with the file constructor)
 * </ul>
 * 
 * @author schlienger
 */
public class HeadlessVirtualRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository()
  {
    return true;
  }

  /**
   * Configuration string contains the location of the repository specification file (xml)
   * 
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    if( getConfiguration() == null )
      throw new RepositoryException( "Configuration must contain the location" );

    return new VirtualRepository( getClass().getName(), getRepositoryName(), getConfiguration(), isReadOnly() );
  }
}