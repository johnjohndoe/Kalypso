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

package org.kalypso;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * @author kuepfer
 * 
 * The main plugin class to be used in the desktop.
 */
public class KalypsoNewNAProjectPlugin extends AbstractUIPlugin
{
  //The shared instance.
  private static KalypsoNewNAProjectPlugin plugin;

  //Resource bundle.
  private ResourceBundle resourceBundle;

  /**
   * The constructor.
   */
  public KalypsoNewNAProjectPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle
          .getBundle( "org.kalypso.wizard.WizardMessages" ); //$NON-NLS-1$
//      resourceBundle = ResourceBundle
//          .getBundle( "org.kalypso.wizard.resources.KalypsoNewProjectPluginResources" ); //$NON-NLS-1$
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
      x.printStackTrace();
    }
  }

  /**
   * This method is called upon plug-in activation
   */
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoNewNAProjectPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not
   * found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoNewNAProjectPlugin.getDefault().getResourceBundle();
    try
    {
      return ( bundle != null ) ? bundle.getString( key ) : key;
    }
    catch( MissingResourceException e )
    {
      e.printStackTrace();
      return key;
    }
  }

  /**
   * Returns the plugin's resource bundle,
   */
  public ResourceBundle getResourceBundle()
  {
    return resourceBundle;
  }
}