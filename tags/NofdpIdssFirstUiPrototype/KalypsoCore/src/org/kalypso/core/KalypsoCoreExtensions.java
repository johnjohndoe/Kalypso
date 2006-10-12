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

package org.kalypso.core;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalogContribution;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.IPropertiesFeatureVisitor;

/**
 * Helper class to read extension-points of this plugin.
 * 
 * @author belger
 */
public class KalypsoCoreExtensions
{
  private final static String VISITOR_EXTENSION_POINT = "org.kalypso.core.featureVisitor";

  private final static String CATALOG_CONTRIBUTIONS_EXTENSION_POINT = "org.kalypso.core.catalogContribution";

  /** id -> config-element */
  private static Map<String, IConfigurationElement> THE_VISITOR_MAP = null;

  public static synchronized FeatureVisitor createFeatureVisitor( final String id, final Properties properties ) throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( VISITOR_EXTENSION_POINT );
    if( THE_VISITOR_MAP == null )
    {
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_VISITOR_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );
      for( int i = 0; i < configurationElements.length; i++ )
      {
        final IConfigurationElement element = configurationElements[i];
        final String configid = element.getAttribute( "id" );
        THE_VISITOR_MAP.put( configid, element );
      }
    }

    if( !THE_VISITOR_MAP.containsKey( id ) )
      return null;

    final IConfigurationElement element = THE_VISITOR_MAP.get( id );
    final FeatureVisitor visitor = (FeatureVisitor) element.createExecutableExtension( "class" );
    if( visitor instanceof IPropertiesFeatureVisitor )
      ((IPropertiesFeatureVisitor) visitor).init( properties );

    return visitor;
  }

  public static void loadXMLCatalogs( final CatalogManager catalogManager )
  {
    if( !Platform.isRunning() )
    {
      System.out.println( "Platform is not running, plugins are not available" );
      return;
    }

    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( CATALOG_CONTRIBUTIONS_EXTENSION_POINT );

    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
    for( int i = 0; i < configurationElements.length; i++ )
    {
      try
      {
        final IConfigurationElement element = configurationElements[i];
        final String name = element.getName();
        if( "catalogContribution".equals( name ) )
        {
          final Object createExecutableExtension = element.createExecutableExtension( "class" );
          final ICatalogContribution catalogContribution = (ICatalogContribution) createExecutableExtension;
          catalogContribution.contributeTo( catalogManager );
        }
        else if( "urnGenerator".equals( name ) )
        {
          final Object createExecutableExtension = element.createExecutableExtension( "class" );
          final IURNGenerator urnGenerator = (IURNGenerator) createExecutableExtension;
          catalogManager.register( urnGenerator );
        }
      }
      catch( final Throwable t )
      {
        // In order to prevent bad code from other plugins (see Eclipse-PDE-Rules)
        // catch exception here and just log it
        final IStatus status = StatusUtilities.statusFromThrowable( t );
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }
  }
}
