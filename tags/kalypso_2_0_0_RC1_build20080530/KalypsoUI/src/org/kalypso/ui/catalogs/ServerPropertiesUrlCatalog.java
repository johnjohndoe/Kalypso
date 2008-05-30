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
package org.kalypso.ui.catalogs;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Represents the server side url catalog based on a properties file.
 * 
 * @author Gernot Belger
 */
public class ServerPropertiesUrlCatalog implements IUrlCatalog
{
  private IUrlCatalog m_catalog = null;

  public IUrlCatalog getInternalCatalog( )
  {
    if( m_catalog == null )
      m_catalog = KalypsoGisPlugin.getDefault().loadRemoteSchemaCatalog();

    return m_catalog;
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public URL getURL( final String namespace )
  {
    return getInternalCatalog().getURL( namespace );
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getPreferedNamespacePrefix(java.lang.String)
   */
  public String getPreferedNamespacePrefix( final String namespace )
  {
    return getInternalCatalog().getPreferedNamespacePrefix( namespace );
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getCatalog()
   */
  public Map<String, URL> getCatalog( )
  {
    return getInternalCatalog().getCatalog();
  }

}
