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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalogContribution;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.ogc.gml.om.IComponentHandler;
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

  /* extension-point 'componentHandler' */

  private final static String COMPONENT_HANDLER_EXTENSION_POINT = "org.kalypso.core.componentHandler";

  private static Map<String, IComponentHandler> THE_COMPONENT_MAP = null;

  /* Theme-Info Extension-Point */
  private static final String THEME_INFO_EXTENSION_POINT = "org.kalypso.core.themeInfo";

  private static Map<String, IConfigurationElement> THE_THEME_INFO_MAP = null;

  /* GmlSourceProvider Extension-Point */
  private static final String GML_SOURCE_PROVIDER_EXTENSION_POINT = "org.kalypso.core.gmlSourceProvider";

  public static synchronized FeatureVisitor createFeatureVisitor( final String id, final Properties properties ) throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    if( THE_VISITOR_MAP == null )
    {
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( VISITOR_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_VISITOR_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
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
    for( final IConfigurationElement element : configurationElements )
    {
      try
      {
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

  /**
   * @return The handler whichs id-attribute equals the given id. Null if no such handler was found.
   */
  public static IComponentHandler findComponentHandler( final String id )
  {
    if( THE_COMPONENT_MAP == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( COMPONENT_HANDLER_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_COMPONENT_MAP = new HashMap<String, IComponentHandler>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
        try
        {
          // TODO: anti-eclipse! do not instantiate exension before they are REALLY needed
          final String configid = element.getAttribute( "id" );
          final IComponentHandler handler = (IComponentHandler) element.createExecutableExtension( "class" );
          THE_COMPONENT_MAP.put( configid, handler );
        }
        catch( final CoreException e )
        {
          KalypsoCorePlugin.getDefault().getLog().log( e.getStatus() );
        }
      }
    }

    return THE_COMPONENT_MAP.get( id );
  }

  /**
   * @param themeInfoId
   *            Id of the requested extension. Can contain properties. Example:
   *            <code>org.kalypso.core.someId?prop1=value1&props2=values</code>
   */
  public static IKalypsoThemeInfo createThemeInfo( final String themeInfoId, final IKalypsoTheme theme )
  {
    if( THE_THEME_INFO_MAP == null )
    {
      /* Lookup the existing ids only once */

      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( THEME_INFO_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_THEME_INFO_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
        final String configid = element.getAttribute( "id" );
        THE_THEME_INFO_MAP.put( configid, element );
      }
    }

    final String id;
    final Properties props = new Properties();
    if( themeInfoId.contains( "?" ) )
    {
      final int queryPartIndex = themeInfoId.indexOf( '?' );
      id = themeInfoId.substring( 0, queryPartIndex );

      // replace in order to handle empty query
      final String query = themeInfoId.substring( queryPartIndex ).replaceAll( "\\?", "" );
      PropertiesUtilities.collectProperties( query, "&", "=", props );
    }
    else
      id = themeInfoId;

    try
    {
      final IConfigurationElement element = THE_THEME_INFO_MAP.get( id );
      final IKalypsoThemeInfo info = (IKalypsoThemeInfo) element.createExecutableExtension( "class" );
      info.init( theme, props );
      return info;
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoCorePlugin.getDefault().getLog().log( status );
    }

    return null;
  }

  /**
   * @param category
   *            If non-<code>null</code>, returned providers are filtered by this category. Else all registered
   *            providers are returned.
   */
  public static IGmlSourceProvider[] createGmlSourceProvider( final String category )
  {
    final List<IGmlSourceProvider> result = new ArrayList<IGmlSourceProvider>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( GML_SOURCE_PROVIDER_EXTENSION_POINT );
    final IConfigurationElement[] providerElements = extensionPoint.getConfigurationElements();
    for( final IConfigurationElement providerElement : providerElements )
    {
      final String providerId = providerElement.getAttribute( "id" );

      final IConfigurationElement[] categoryElements = providerElement.getChildren( "category" );
      for( final IConfigurationElement categoryElement : categoryElements )
      {
        final String categoryId = categoryElement.getAttribute( "id" );
        if( category == null || category.equals( categoryId ) )
        {
          try
          {
            result.add( (IGmlSourceProvider) providerElement.createExecutableExtension( "class" ) );
          }
          catch( final Throwable e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e, "Failed to create gml source provider: " + providerId );
            KalypsoCorePlugin.getDefault().getLog().log( status );
          }

          /* Add each provider only once */
          break;
        }
      }

    }

    return result.toArray( new IGmlSourceProvider[result.size()] );
  }
}
