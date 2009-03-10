/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.mapeditor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypso.ui.ImageProvider;

/**
 * @author kuepfer
 */
public class GisMapOutlinePageExtension
{
  public static List<PluginMapOutlineAction> getRegisteredMapOutlineActions( final IMapModellView gisMapOutlineViewer )
  {
    final ArrayList<PluginMapOutlineAction> actions = new ArrayList<PluginMapOutlineAction>();
    final IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint( "org.kalypso.ui", "mapviewaction" ); //$NON-NLS-1$ //$NON-NLS-2$
    // check if extention point is registered on start up
    if( extensionPoint == null )
      return new ArrayList<PluginMapOutlineAction>();
    final IExtension[] extensions = extensionPoint.getExtensions();
    // no mapview extensions have been registered
    if( extensions == null )
      return new ArrayList<PluginMapOutlineAction>();
    for( final IExtension extension : extensions )
    {
      final IConfigurationElement[] configurationElements = extension.getConfigurationElements();
      for( final IConfigurationElement configurationElement : configurationElements )
      {
        final String title = configurationElement.getAttribute( "title" ); //$NON-NLS-1$
        // TODO: i18n the title!?

        final String resource = configurationElement.getAttribute( "icon" ); //$NON-NLS-1$
        // gets the parent of this element (the plugin which implements this extension)
        final IExtension parent = (IExtension) configurationElement.getParent();
        // gets the plugin id of the parent plugin
        final String pluginID = parent.getNamespace();
        final ImageDescriptor icon = ImageProvider.id( pluginID, resource );
        final String enabled = configurationElement.getAttribute( "enabled" ); //$NON-NLS-1$
        boolean visible = false;
        if( enabled != null && enabled.equals( "true" ) ) //$NON-NLS-1$
          visible = true;
        final String tooltip = configurationElement.getAttribute( "tooltip" ); //$NON-NLS-1$
        // create action delegate
        PluginMapOutlineAction actionDelegate;
        try
        {
          final PluginMapOutlineActionDelegate action = (PluginMapOutlineActionDelegate) configurationElement.createExecutableExtension( "class" ); //$NON-NLS-1$
          actionDelegate = new PluginMapOutlineAction( title, icon, tooltip, gisMapOutlineViewer, action );
          actionDelegate.setEnabled( visible );
          actions.add( actionDelegate );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();
        }
      }
    }
    return actions;
  }

}
