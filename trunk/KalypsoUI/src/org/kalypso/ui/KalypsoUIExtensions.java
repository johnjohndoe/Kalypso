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
package org.kalypso.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.featureview.control.IFeatureviewControlFactory;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

/**
 * Within this class all extension-point from the KalypsoUI plug-in are handled.
 * 
 * @author Gernot Belger
 */
public class KalypsoUIExtensions
{
  private KalypsoUIExtensions( )
  {
    // do not instantiate
  }

  /* extension-point 'featureViewExtensionControl' */
  private final static String FEATUREVIEW_CONTROL_EXTENSION_POINT = "org.kalypso.ui.featureViewExtensionControl";

  private static Map<String, IConfigurationElement> THE_FEATUREVIEW_CONTROL_MAP = null;

  public static IFeatureviewControlFactory getFeatureviewControlFactory( final String id ) throws CoreException
  {
    final Map<String, IConfigurationElement> map = getFeatureviewControlMap();
    if( map == null )
      return null;

    final IConfigurationElement factoryElement = map.get( id );
    if( factoryElement == null )
      throw new CoreException( StatusUtilities.createErrorStatus( "No feature-control-factory found with id: " + id ) );

    return (IFeatureviewControlFactory) factoryElement.createExecutableExtension( "class" );
  }

  private static synchronized Map<String, IConfigurationElement> getFeatureviewControlMap( )
  {
    if( THE_FEATUREVIEW_CONTROL_MAP == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( FEATUREVIEW_CONTROL_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_FEATUREVIEW_CONTROL_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );

      for( final IConfigurationElement element : configurationElements )
      {
        final String id = element.getAttribute( "id" );
        THE_FEATUREVIEW_CONTROL_MAP.put( id, element );
      }

    }

    return THE_FEATUREVIEW_CONTROL_MAP;
  }

  public static IComponentUiHandlerProvider createComponentUiHandlerProvider( final String componentUiHandlerProviderId )
  {
    final String idToFind = componentUiHandlerProviderId == null ? "org.kalypso.ogc.gml.om.table.handlers.DefaultComponentUiHandlerProvider" : componentUiHandlerProviderId;

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( "org.kalypso.ui.componentUiHandlerProvider" );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

    for( final IConfigurationElement element : configurationElements )
    {
      final String id = element.getAttribute( "id" );
      if( id.equals( idToFind ) )
      {
        try
        {
          return (IComponentUiHandlerProvider) element.createExecutableExtension( "class" );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();

          KalypsoGisPlugin.getDefault().getLog().log( e.getStatus() );

          return null;
        }
      }
    }

    final IStatus status = StatusUtilities.createErrorStatus( "No componenUiHandlerProvider found with id: " + componentUiHandlerProviderId );
    KalypsoGisPlugin.getDefault().getLog().log( status );

    return null;
  }

}
