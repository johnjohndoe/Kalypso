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
package org.kalypso.model.wspm.pdb.connect;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.connect.ErrorSettings;
import org.kalypso.model.wspm.pdb.internal.connect.PdbSettingsRegistry;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * Access to the configured connections.
 * 
 * @author Gernot Belger
 */
public final class PdbSettings
{
  private PdbSettings( )
  {
    throw new UnsupportedOperationException( "helper class, do not instantiate" ); //$NON-NLS-1$
  }

  /**
   * Accesses the global kept pdb settings.
   */
  public static IPdbSettings[] getSettings( ) throws PdbConnectException
  {
    return WspmPdbCorePlugin.getDefault().getSettings();
  }

  public static IPdbSettings getSettings( final String settingsName ) throws PdbConnectException
  {
    final IPdbSettings[] allSettings = getSettings();
    for( final IPdbSettings settings : allSettings )
    {
      if( settings.getName().equals( settingsName ) )
        return settings;
    }

    final String msg = String.format( Messages.getString( "PdbSettings_0" ), settingsName ); //$NON-NLS-1$
    throw new PdbConnectException( msg );
  }

  /**
   * Updates the global kept pdb settings.
   */
  public static void setSettings( final IPdbSettings[] settings ) throws PdbConnectException
  {
    WspmPdbCorePlugin.getDefault().setConnections( settings );
  }

  /**
   * Returns all configured connections. If the connections cannot be accessed, one error entry is returned, containing
   * an appropriate error message.
   */
  public static IPdbSettings[] getSettingsOrError( )
  {
    try
    {
      return getSettings();
    }
    catch( final PdbConnectException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString( "PdbSettings_1" ), e ); //$NON-NLS-1$
      final ErrorSettings error = new ErrorSettings( status );
      return new IPdbSettings[] { error };
    }
  }

  public static String[] getSettingsTypes( )
  {
    return getRegistry().getRegisteredTypes();
  }

  private static PdbSettingsRegistry getRegistry( )
  {
    return WspmPdbCorePlugin.getDefault().getConnectionRegistry();
  }

  public static IPdbSettings createSettings( final String type )
  {
    return getRegistry().createSettings( type );
  }
}