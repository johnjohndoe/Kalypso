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

package org.kalypso.metadoc.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExporter;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;

/**
 * Handles the extension points of this plugin.
 * 
 * @author schlienger
 */
public class MetadocExtensions
{
  private static final String TARGETS_EXTENSION_POINT = "org.kalypso.metadoc.exportTarget";

  private static final String EXPORTERS_EXTENSION_POINT = "org.kalypso.metadoc.exporter";

  private static Map m_exporters = null;

  private static Map m_targets = null;

  private MetadocExtensions( )
  {
  }

  /**
   * Retrieve the {@link IExportTarget} extensions
   */
  public static IExportTarget[] retrieveTargets( ) throws CoreException
  {
    final IConfigurationElement[] elements = retrieveConfigurationElementsFor( TARGETS_EXTENSION_POINT );
    final Vector<IExportTarget> items = new Vector<IExportTarget>();
    final List<IStatus> stati = new ArrayList<IStatus>();
    for( int i = 0; i < elements.length; i++ )
    {
      IConfigurationElement element = elements[i];

      try
      {
        final IExportTarget target = (IExportTarget) element.createExecutableExtension( "class" );
        items.add( target );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        stati.add( e.getStatus() );
      }
    }

    if( stati.size() > 0 )
      throw new CoreException( new MultiStatus( KalypsoMetaDocPlugin.getId(), 0, stati.toArray( new IStatus[stati.size()] ), "Nicht alle Target konnten geladen werden", null ) );

    return items.toArray( new IExportTarget[items.size()] );
  }

  /**
   * Retrieves the exporter (from the corresponding extensions) which has the given id
   */
  public static IExporter retrieveExporter( final String id ) throws CoreException
  {
    if( m_exporters == null )
      m_exporters = retrieveExporterInHash( EXPORTERS_EXTENSION_POINT );

    final IConfigurationElement element = (IConfigurationElement) m_exporters.get( id );
    return (IExporter) element.createExecutableExtension( "class" );
  }

  /**
   * Lazy loading of the exporters into map (exporter-id --&gt; conf-element)
   */
  private static Map retrieveExporterInHash( final String extensionPointId )
  {
    final HashMap<String, IConfigurationElement> map = new HashMap<String, IConfigurationElement>();

    final IConfigurationElement[] elements = retrieveConfigurationElementsFor( extensionPointId );
    for( int i = 0; i < elements.length; i++ )
    {
      final IConfigurationElement element = elements[i];

      map.put( element.getAttribute( "id" ), element );
    }

    return map;
  }

  /**
   * Helper retriever that returns the {@link IConfigurationElement}s for the given extension point
   */
  public static IConfigurationElement[] retrieveConfigurationElementsFor( final String extensionPointId )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( extensionPointId );

    if( extensionPoint == null )
      return new IConfigurationElement[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final Vector<IConfigurationElement> items = new Vector<IConfigurationElement>();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
        items.add( elements[j] );
    }

    return items.toArray( new IConfigurationElement[items.size()] );
  }

  /**
   * Retrieves the target (from the corresponding extensions) which has the given id
   */
  public static IExportTarget retrieveTarget( final String id ) throws CoreException
  {
    if( m_targets == null )
      m_targets = retrieveExporterInHash( TARGETS_EXTENSION_POINT );

    final IConfigurationElement element = (IConfigurationElement) m_targets.get( id );
    return (IExportTarget) element.createExecutableExtension( "class" );
  }
}
