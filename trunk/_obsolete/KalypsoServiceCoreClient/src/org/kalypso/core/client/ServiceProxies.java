/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.core.client;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

/**
 * Handles the list of serviceproxy extensions for the proxies of the Kalypso services.
 *
 * @author schlienger
 */
public class ServiceProxies
{
  public final static String EXTENSION_POINT = "org.kalypso.services.core.client.serviceproxy";
  public final static String ATT_INTERFACENAME = "interfacename";
  public final static String ATT_CLASS = "class";

  /** maps interface-name to IConfigurationElement containing classname of serviceproxy implementation */
  private final Map<String, IConfigurationElement> m_map = new HashMap<String, IConfigurationElement>();
  
  public ServiceProxies()
  {
    loadExtensions();
  }
  
  /**
   * Uses the platform extension registry to retrieve all extensions for the serviceproxy extension point.
   */
  private final void loadExtensions()
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( EXTENSION_POINT );

    if( extensionPoint == null )
      return;

    final IExtension[] extensions = extensionPoint.getExtensions();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        final String name = element.getAttribute( ATT_INTERFACENAME );

        m_map.put( name, element );
      }
    }
  }

  /**
   * @return the proxy implementation for the given interface name
   */
  public Object getInstanceFor( final String interfaceName ) throws CoreException
  {
    final IConfigurationElement element = m_map.get( interfaceName );
    
    if( element == null )
      throw new IllegalArgumentException( "ServiceProxy for interface " + interfaceName + " is not defined!" );
    
    return element.createExecutableExtension( ATT_CLASS );
  }
}
