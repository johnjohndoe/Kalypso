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
 * Abstract implementation of the <code>IRepositoryFactory</code> to permit
 * subclasses to inherit from this common functionality.
 * <p>
 * This class provides support for setting the configuration and readonly parameters.
 * 
 * @author schlienger
 */
public abstract class AbstractRepositoryFactory implements IRepositoryFactory
{
  /** configuration string, may be used by subclasses */
  private String m_configuration;
  
  /** readonly flag */
  private boolean m_readOnly;

  public String getConfiguration()
  {
    return m_configuration;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#setConfiguration(java.lang.String)
   */
  public void setConfiguration( final String configuration )
  {
    m_configuration = configuration;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#setReadOnly(boolean)
   */
  public void setReadOnly( final boolean ro )
  {
    m_readOnly = ro;
  }
  
  public boolean isReadOnly()
  {
    return m_readOnly;
  }
}
