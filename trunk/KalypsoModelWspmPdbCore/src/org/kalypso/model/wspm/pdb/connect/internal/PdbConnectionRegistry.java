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
package org.kalypso.model.wspm.pdb.connect.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;
import org.kalypso.model.wspm.pdb.connect.internal.postgis.PostgisConnectInfo;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * Single point that 'knows' of the different types of connections.<br/>
 * Intended to be bases on extension-point later.
 * 
 * @author Gernot Belger
 */
public class PdbConnectionRegistry
{
  static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  public String[] getRegisteredTypes( )
  {
    return new String[] { PostgisConnectInfo.TYPE };
  }

  public IPdbConnectInfo readConnection( final ISecurePreferences preferences ) throws StorageException
  {
    final String type = preferences.get( PROPERTY_TYPE, ErrorPdbConnectInfo.TYPE );
    final IPdbConnectInfo info = createInfo( type );
    info.readState( preferences );
    return info;
  }

  public IPdbConnectInfo createInfo( final String type )
  {
    if( PostgisConnectInfo.TYPE.equals( type ) )
      return new PostgisConnectInfo();

    final String message = String.format( "Unknown connection type: %s", type );
    final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message );
    return new ErrorPdbConnectInfo( status );
  }
}