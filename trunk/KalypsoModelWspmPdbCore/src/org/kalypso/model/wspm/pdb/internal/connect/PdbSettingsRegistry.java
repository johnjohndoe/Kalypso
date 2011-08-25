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
package org.kalypso.model.wspm.pdb.internal.connect;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.connect.oracle.OracleSettings;
import org.kalypso.model.wspm.pdb.internal.connect.postgis.PostgisSettings;

/**
 * Single point that 'knows' of the different types of connections.<br/>
 * Intended to be bases on extension-point later.
 * 
 * @author Gernot Belger
 */
public class PdbSettingsRegistry
{
  static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  static final String PROPERTY_NAME = "name"; //$NON-NLS-1$

  public String[] getRegisteredTypes( )
  {
    return new String[] { PostgisSettings.TYPE, OracleSettings.TYPE };
  }

  public IPdbSettings readSettings( final ISecurePreferences preferences ) throws StorageException
  {
    final String type = preferences.get( PROPERTY_TYPE, ErrorSettings.TYPE );
    final String name = preferences.get( PROPERTY_NAME, StringUtils.EMPTY );

    final AbstractSettings settings = createSettings( type );
    settings.readState( preferences );
    settings.setName( name );
    return settings;
  }

  public AbstractSettings createSettings( final String type )
  {
    // TODO: we should use extensions here
    if( PostgisSettings.TYPE.equals( type ) )
      return new PostgisSettings();

    if( OracleSettings.TYPE.equals( type ) )
      return new OracleSettings();

    final String message = String.format( "Unknown connection type: %s", type ); //$NON-NLS-1$
    final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message );
    return new ErrorSettings( status );
  }
}