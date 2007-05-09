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
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ui.ImageProvider;

/**
 * @author kuepfer
 */
public class GisMapOutlinePageExtension
{
  public static List<PluginMapOutlineAction> getRegisteredMapOutlineActions( final IMapModellView gisMapOutlineViewer )
  {
    final ArrayList<PluginMapOutlineAction> actions = new ArrayList<PluginMapOutlineAction>();
    IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
    IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint( "org.kalypso.ui", "mapviewaction" );
    // check if extention point is registered on start up
    if( extensionPoint == null )
      return new ArrayList<PluginMapOutlineAction>();
    IExtension[] extensions = extensionPoint.getExtensions();
    // no mapview extensions have been registered
    if( extensions == null )
      return new ArrayList<PluginMapOutlineAction>();
    for( int i = 0; i < extensions.length; i++ )
    {
      IExtension extension = extensions[i];
      IConfigurationElement[] configurationElements = extension.getConfigurationElements();
      for( int j = 0; j < configurationElements.length; j++ )
      {
        IConfigurationElement configurationElement = configurationElements[j];
        String title = configurationElement.getAttribute( "title" );
        String resource = configurationElement.getAttribute( "icon" );
        // gets the parent of this element (the plugin which implements this extension)
        IExtension parent = (IExtension) configurationElement.getParent();
        // gets the plugin id of the parent plugin
        String pluginID = parent.getNamespace();
        ImageDescriptor icon = ImageProvider.id( pluginID, resource );
        String enabled = configurationElement.getAttribute( "enabled" );
        boolean visible = false;
        if( enabled != null && enabled.equals( "true" ) )
          visible = true;
        String tooltip = configurationElement.getAttribute( "tooltip" );
        // create action delegate
        PluginMapOutlineAction actionDelegate;
        try
        {
          PluginMapOutlineActionDelegate action = (PluginMapOutlineActionDelegate) configurationElement.createExecutableExtension( "class" );
          actionDelegate = new PluginMapOutlineAction( title, icon, tooltip, gisMapOutlineViewer, action, gisMapOutlineViewer );
          actionDelegate.setEnabled( visible );
          actions.add( actionDelegate );
        }
        catch( CoreException e )
        {
          e.printStackTrace();
        }
      }
    }
    return actions;
  }

}
