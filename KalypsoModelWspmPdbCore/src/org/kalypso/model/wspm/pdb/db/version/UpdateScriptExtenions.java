/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.pdb.db.version;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class UpdateScriptExtenions
{
  private static final String STR_FAILED_TO_ACCESS_EXTENSION = "Failed to access extension"; //$NON-NLS-1$

  private final static String UPDATE_EXTENSION_POINT = "org.kalypso.model.wspm.pdb.core.updateScript"; //$NON-NLS-1$

  public static UpdateScript[] getUpdateScripts( final Version sourceVersion, final String type )
  {
    final IConfigurationElement[] configurationElements = getElements();

    final List<UpdateScript> scripts = new ArrayList<>();

    for( final IConfigurationElement element : configurationElements )
    {
      try
      {
        final UpdateScript updateScript = new UpdateScript( element );
        final Version scriptVersion = updateScript.getTargetVersion();
        final String scriptType = updateScript.getType();
        if( type.equals( scriptType ) && sourceVersion.compareTo( scriptVersion ) < 0 )
          scripts.add( updateScript );
      }
      catch( final Throwable e )
      {
        final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_ACCESS_EXTENSION, e ); //$NON-NLS-1$
        WspmPdbCorePlugin.getDefault().getLog().log( status );
      }
    }

    /* Sort by version, the scripts get applied in that order */
    Collections.sort( scripts );

    return scripts.toArray( new UpdateScript[scripts.size()] );
  }

  public static UpdateScript getScript( final Version version, final String type )
  {
    final IConfigurationElement[] configurationElements = getElements();

    for( final IConfigurationElement element : configurationElements )
    {
      try
      {
        final UpdateScript updateScript = new UpdateScript( element );
        final Version scriptVersion = updateScript.getTargetVersion();
        final String scriptType = updateScript.getType();
        if( type.equals( scriptType ) && version.compareTo( scriptVersion ) == 0 )
          return updateScript;
      }
      catch( final Throwable e )
      {
        final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_ACCESS_EXTENSION, e ); //$NON-NLS-1$
        WspmPdbCorePlugin.getDefault().getLog().log( status );
      }
    }

    return null;
  }

  private static IConfigurationElement[] getElements( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( UPDATE_EXTENSION_POINT );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
    return configurationElements;
  }

}
