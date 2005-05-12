/**----------------    FILE HEADER KALYPSO ------------------------------------------
*
*  This file is part of kalypso.
*  Copyright (C) 2004 by:
* 
*  Technical University Hamburg-Harburg (TUHH)
*  Institute of River and coastal engineering
*  Denickestraße 22
*  21073 Hamburg, Germany
*  http://www.tuhh.de/wb
* 
*  and
*  
*  Bjoernsen Consulting Engineers (BCE)
*  Maria Trost 3
*  56070 Koblenz, Germany
*  http://www.bjoernsen.de
* 
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
* 
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
* 
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
* 
*  Contact:
* 
*  E-Mail:
*  g.belger@bjoernsen.de
*  m.schlienger@bjoernsen.de
*  v.doemming@tuhh.de
*   
*  ---------------------------------------------------------------------------*/
package org.kalypso.java.net;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * Abstrakter UrlKatalog. Ableitende Klassen müssen nur den konkreten Katalog füllen.
 * 
 * @author belger
 */
public abstract class AbstractUrlCatalog implements IUrlCatalog
{
  private final HashMap m_catalog = new HashMap();

  public AbstractUrlCatalog() 
  {
    fillCatalog( getClass(), m_catalog );
  }
  
  protected abstract void fillCatalog( final Class myClass, final Map catalog );

  /**
   * @see org.kalypso.java.net.IUrlCatalog#getCatalog()
   */
  public final Map getCatalog()
  {
    return m_catalog;
  }

  /**
   * @see org.kalypso.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public final URL getURL( final String namespace )
  {
    return (URL)m_catalog.get( namespace );
  }
}
