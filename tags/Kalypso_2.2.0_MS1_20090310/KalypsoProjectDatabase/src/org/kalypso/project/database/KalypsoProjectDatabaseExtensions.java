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
package org.kalypso.project.database;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

/**
 * @author Dirk Kuch
 */
public class KalypsoProjectDatabaseExtensions
{

  private static Map<String, IConfigurationElement> DATABASE_TRIGGERS = null;

  private final static String DATABASE_TRIGGER_EXTENSION_POINT = "org.kalypso.project.database.projectDatabaseTrigger";

  /**
   * @param id
   *          remote database project id
   * @return IConfigurationElement of IProjectDatabaseTrigger for remote project type "id"
   */
  public synchronized static IConfigurationElement getProjectDatabaseTriggers( final String id )
  {
    // fill binding map
    if( DATABASE_TRIGGERS == null )
    {
      DATABASE_TRIGGERS = new HashMap<String, IConfigurationElement>();

      /* get extension points */
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IConfigurationElement[] elements = registry.getConfigurationElementsFor( DATABASE_TRIGGER_EXTENSION_POINT );

      for( final IConfigurationElement configurationElement : elements )
      {
        final String remoteId = configurationElement.getAttribute( "remoteId" );
        DATABASE_TRIGGERS.put( remoteId, configurationElement );
      }
    }

    return DATABASE_TRIGGERS.get( id );
  }

}
