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

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 */
public class PdbConnectionWriter
{
  private final IPdbConnectInfo[] m_connections;

  public PdbConnectionWriter( final IPdbConnectInfo[] connections )
  {
    m_connections = connections;
  }

  public void writeConnections( final ISecurePreferences preferences ) throws PdbConnectException
  {
    try
    {
      serializeConnections( preferences );
    }
    catch( final StorageException e )
    {
      throw new PdbConnectException( "Failed to write connections into secure storage", e );
    }
  }

  protected void serializeConnections( final ISecurePreferences preferences ) throws StorageException
  {
    preferences.clear();

    int count = 0;
    for( final IPdbConnectInfo info : m_connections )
    {
      final ISecurePreferences childNode = preferences.node( "" + count++ );
      childNode.put( PdbConnectionRegistry.PROPERTY_TYPE, info.getType(), false );
      info.saveState( childNode );
    }
  }
}