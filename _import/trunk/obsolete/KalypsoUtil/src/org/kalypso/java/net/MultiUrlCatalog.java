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
package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * A url-catalog made up from several url-catalogs.
 * 
 * @author gernot
 */
public class MultiUrlCatalog implements IUrlCatalog
{
  private final IUrlCatalog[] m_catalogs;

  public MultiUrlCatalog( final IUrlCatalog[] catalogs )
  {
    m_catalogs = catalogs;
  }

  /**
   * Iterates the child catalogs to find the url. The order is as given in the constructor.
   * 
   * @throws MalformedURLException if any of the child catalogs throws it.
   * 
   * @see org.kalypso.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public URL getURL( final String key ) throws MalformedURLException
  {
    for( int i = 0; i < m_catalogs.length; i++ )
    {
      final IUrlCatalog catalog = m_catalogs[i];
      final URL url = catalog.getURL( key );
      if( url != null )
        return url;
    }
    
    return null;
  }

}
