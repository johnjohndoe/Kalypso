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

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;

/**
 * Einfache Repository Spezifikation
 * 
 * @author schlienger
 */
public class RepositorySpecification
{
  private final String m_id;

  private final String m_desc;

  private final String m_factoryClass;

  private String m_maxCard;

  /**
   * @param id
   *          specification id
   * @param desc
   *          description that is shown to user when selecting repository
   *          factory
   * @param factoryClass
   *          classname of the repository factory
   * @param maxCard
   *          max cardinality of repositories of this type allowed to user
   */
  public RepositorySpecification( final String id, final String desc, final String factoryClass,
      final String maxCard )
  {
    m_id = id;
    m_desc = desc;
    m_factoryClass = factoryClass;
    m_maxCard = maxCard;
  }

  public String getDesc()
  {
    return m_desc;
  }

  public String getId()
  {
    return m_id;
  }

  public String getMaxCard()
  {
    return m_maxCard;
  }

  /**
   * Erzeugt eine Repository Factory für diese eine Spezifikation.
   * 
   * @throws ClassUtilityException
   */
  public IRepositoryFactory createFactory( final ClassLoader cl ) throws ClassUtilityException
  {
    return (IRepositoryFactory)ClassUtilities.newInstance( m_factoryClass,
        IRepositoryFactory.class, cl );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return m_desc;
  }
}