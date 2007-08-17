/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package de.renew.workflow.contexts;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import de.renew.workflow.base.WorkflowModelPlugin;

/**
 * Helper class to read and cache context handler factories from extension point.
 * 
 * @author Stefan Kurzbach
 */
public class ContextHandlerFactoryExtension
{

  private static Map<String, IContextHandlerFactory> THE_FACTORY_MAP = null;

  private final static String CONTEXT_HANDLER_FACTORY_EXTENSION_POINT = "de.renew.workflow.model.contextHandlerFactories"; //$NON-NLS-1$

  public static IContextHandlerFactory getFactory( final ExtensionContext extContext )
  {
    final Map<String, IContextHandlerFactory> map = getFactoryMap();
    if( map == null )
      return null;

    return map.get( extContext.getHandlerFactoryId() );
  }

  private static Map<String, IContextHandlerFactory> getFactoryMap( )
  {
    if( THE_FACTORY_MAP == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( CONTEXT_HANDLER_FACTORY_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_FACTORY_MAP = new HashMap<String, IContextHandlerFactory>( configurationElements.length );

      for( final IConfigurationElement element : configurationElements )
      {
        final String id = element.getAttribute( "id" ); //$NON-NLS-1$
        try
        {
          final IContextHandlerFactory factory = (IContextHandlerFactory) element.createExecutableExtension( "class" ); //$NON-NLS-1$
          THE_FACTORY_MAP.put( id, factory );
        }
        catch( final CoreException e )
        {
          final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", Messages.getString("ContextHandlerFactoryExtension.4") + id, e ); //$NON-NLS-1$ //$NON-NLS-2$
          WorkflowModelPlugin.getInstance().getLog().log( status );
        }
      }
    }

    return THE_FACTORY_MAP;
  }

}
