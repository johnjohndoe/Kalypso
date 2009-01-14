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
package org.kalypso.project.database.server.trigger;

import java.lang.reflect.Constructor;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.project.database.IProjectDataBaseServerConstant;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseTrigger;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;
import org.osgi.framework.Bundle;

/**
 * @author Dirk Kuch
 */
public class TriggerHelper
{
  public static final String COMMON_FOLDER = "common";

  public static void handleBean( KalypsoProjectBean bean, IConfigurationElement element ) throws Exception
  {
    /* resolve trigger extension class */
    final String pluginid = element.getContributor().getName();
    final Bundle bundle = Platform.getBundle( pluginid );
    final Class< ? extends IProjectDatabaseTrigger> triggerClass = bundle.loadClass( element.getAttribute( "class" ) );
    final Constructor< ? extends IProjectDatabaseTrigger> constructor = triggerClass.getConstructor();

    IProjectDatabaseTrigger trigger = constructor.newInstance();

    final FileSystemManager manager = VFSUtilities.getManager();

    /* resolve global dir */
    String urlGlobalPath = System.getProperty( IProjectDataBaseServerConstant.SERVER_GLOBAL_DATA_PATH );
    FileObject folderGlobal = manager.resolveFile( urlGlobalPath );
    if( !folderGlobal.exists() )
      folderGlobal.createFolder();

    /* global "global" dir */
    FileObject destinationCommonFolder = folderGlobal.resolveFile( COMMON_FOLDER );
    if( !destinationCommonFolder.exists() )
      destinationCommonFolder.createFolder();

    /* global project dir */
    FileObject destinationProjectFolder = folderGlobal.resolveFile( bean.getUnixName() );
    if( !destinationProjectFolder.exists() )
      destinationProjectFolder.createFolder();

    trigger.handleCommonData( bean, destinationCommonFolder );
    trigger.handleProjectData( bean, destinationProjectFolder );
  }

}
