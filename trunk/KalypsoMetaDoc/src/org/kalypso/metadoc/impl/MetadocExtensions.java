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
import java.util.List;
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
import org.kalypso.metadoc.KalypsoMetaDocPlugin;

/**
 * Handles the extension points of this plugin.
 * 
 * @author schlienger
 */
public class MetadocExtensions
{
  private static final String TARGETS_EXTENSION_POINT = "org.kalypso.metadoc.exportTarget";

  private MetadocExtensions()
  {}

  public static IExportTarget[] retrieveTargets() throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( TARGETS_EXTENSION_POINT );

    if( extensionPoint == null )
      return new IExportTarget[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final Vector items = new Vector();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      final List stati = new ArrayList();
      for( int j = 0; j < elements.length; j++ )
      {
        try
        {
          final IConfigurationElement element = elements[j];
          final IExportTarget target = (IExportTarget)element.createExecutableExtension( "class" );
          items.add( target );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();
          stati.add( e.getStatus() );
//          ErrorDialog
//              .openError( shell, "Export Targets", "Fehler beim Laden eines Export-Target: " + id, e.getStatus() );
        }
      }
      
      if( stati.size() > 0 )
        throw new CoreException( new MultiStatus( KalypsoMetaDocPlugin.getId(), 0, (IStatus[])stati.toArray( new IStatus[stati.size()] ), "Nicht alle Target konnten geladen werden", null ) );
    }

    return (IExportTarget[])items.toArray( new IExportTarget[items.size()] );
  }
}
