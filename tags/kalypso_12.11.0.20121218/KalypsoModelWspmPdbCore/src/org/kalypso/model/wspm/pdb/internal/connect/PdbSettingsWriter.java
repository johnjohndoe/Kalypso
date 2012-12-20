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

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 */
public class PdbSettingsWriter
{
  private final IPdbSettings[] m_connections;

  public PdbSettingsWriter( final IPdbSettings[] connections )
  {
    m_connections = connections;
  }

  public void writeConnections( final ISecurePreferences preferences ) throws PdbConnectException
  {
    try
    {
      serializeConnections( preferences );
    }
    catch( final Exception e )
    {
      throw new PdbConnectException( "Failed to write connections into secure storage", e ); //$NON-NLS-1$
    }
  }

  protected void serializeConnections( final ISecurePreferences preferences ) throws StorageException, IOException
  {
    preferences.clear();
    final String[] childrenNames = preferences.childrenNames();
    for( final String childName : childrenNames )
    {
      preferences.node( childName ).removeNode();
    }

    int count = 0;
    for( final IPdbSettings settings : m_connections )
    {
      final ISecurePreferences childNode = preferences.node( StringUtils.EMPTY + count++ );
      childNode.put( PdbSettingsRegistry.PROPERTY_TYPE, settings.getType(), false );
      childNode.put( PdbSettingsRegistry.PROPERTY_NAME, settings.getName(), false );

      settings.saveState( childNode );
    }

    preferences.flush();
  }
}