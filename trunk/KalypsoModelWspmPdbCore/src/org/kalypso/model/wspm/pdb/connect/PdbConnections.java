/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.kalypso.model.wspm.pdb.connect.internal.ErrorPdbConnectInfo;
import org.kalypso.model.wspm.pdb.connect.internal.PdbConnectionRegistry;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * Access to the configured connnections.
 * 
 * @author Gernot Belger
 */
public final class PdbConnections
{
  private PdbConnections( )
  {
    throw new UnsupportedOperationException( "helper class, do not instantiate" ); //$NON-NLS-1$
  }

  public static IPdbConnectInfo[] getConnections( ) throws PdbConnectException
  {
    return WspmPdbCorePlugin.getDefault().getConnections();
  }

  public static void setConnections( final IPdbConnectInfo[] connections ) throws PdbConnectException
  {
    WspmPdbCorePlugin.getDefault().setConnections( connections );
  }

  /**
   * Returns all configured connections. If the connections cannot be accessed, one error entry is returned, containing
   * an appropriate error message.
   */
  public static IPdbConnectInfo[] getConnectionsOrError( )
  {
    try
    {
      return getConnections();
    }
    catch( final PdbConnectException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Unable to access configured configurations", e );
      final ErrorPdbConnectInfo error = new ErrorPdbConnectInfo( status );
      return new IPdbConnectInfo[] { error };
    }
  }

  public static String[] getConnectionTypes( )
  {
    return getRegistry().getRegisteredTypes();
  }

  private static PdbConnectionRegistry getRegistry( )
  {
    return WspmPdbCorePlugin.getDefault().getConnectionRegistry();
  }

  public static IPdbConnectInfo createConnection( final String type )
  {
    return getRegistry().createInfo( type );
  }
}