/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.contribs.java.net;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * A url-catalog made up from several url-catalogs.
 * 
 * @author gernot
 */
public class MultiUrlCatalog implements IUrlCatalog
{
  private final Map m_catalog = new HashMap();

  public MultiUrlCatalog( final IUrlCatalog[] catalogs )
  {
    for( int i = 0; i < catalogs.length; i++ )
    {
      final IUrlCatalog catalog = catalogs[i];
      m_catalog.putAll( catalog.getCatalog() );
    }
  }

  /**
   * Iterates the child catalogs to find the url. The order is as given in the constructor.
   * 
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public URL getURL( final String namespace )
  {
    return (URL)m_catalog.get( namespace );
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getCatalog()
   */
  public Map getCatalog()
  {
    return m_catalog;
  }
}
