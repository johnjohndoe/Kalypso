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

/**
 * Configurator for a Repository.
 * 
 * @author schlienger
 */
public interface IRepositoryFactory
{
  /**
   * Sets the configuration string for this factory.
   * @param conf
   */
  public void setConfiguration( final String conf );
  
  public String getConfiguration();

  /**
   * Sets the readOnly flag. When true, the repository is forced in read only
   * mode.
   * @param ro
   */
  public void setReadOnly( final boolean ro );
  
  public boolean isReadOnly();

  /**
   * Vorbereitet die Konfiguration für das erzeugen des Repository.
   * 
   * @return true wenn Benutzer die Konfiguration bestätigt hat.
   * @throws RepositoryException
   */
  public boolean configureRepository() throws RepositoryException;

  /**
   * @return new repository based on the configuration
   * @throws RepositoryException
   */
  public IRepository createRepository() throws RepositoryException;
}