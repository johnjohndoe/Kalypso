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
package org.kalypso.model.wspm.pdb.internal;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.internal.connect.PdbSettingsReader;
import org.kalypso.model.wspm.pdb.internal.connect.PdbSettingsRegistry;
import org.kalypso.model.wspm.pdb.internal.connect.PdbSettingsWriter;
import org.osgi.framework.BundleContext;

public class WspmPdbCorePlugin extends Plugin
{
  public final static String PLUGIN_ID = "org.kalypso.model.wspm.pdb.core"; //$NON-NLS-1$

  private static final String SECURE_SETTINGS_NODE = "wspmPdbCore.connections"; //$NON-NLS-1$

  /* This instance */
  private static WspmPdbCorePlugin plugin;

  private final PdbSettingsRegistry m_registry = new PdbSettingsRegistry();

  public WspmPdbCorePlugin( )
  {
  }

  @Override
  public void start( final BundleContext bundleContext ) throws Exception
  {
    super.start( bundleContext );

    plugin = this;
  }

  @Override
  public void stop( final BundleContext bundleContext ) throws Exception
  {
    plugin = null;

    super.stop( bundleContext );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static WspmPdbCorePlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns the configured connections for pdb.<br/>
   * Changes to these connection will not take effect until {@link #setConnections(IPdbSettings[])} is called.
   * 
   * @return The currently configured connections for pdb.
   */
  public IPdbSettings[] getSettings( ) throws PdbConnectException
  {
    return new PdbSettingsReader().readConnections( getConnectionPreferences() );
  }

  /**
   * Sets the connections for pdb. The connections will be immediately persisted and can be accessed after workbench
   * restart.
   */
  public void setConnections( final IPdbSettings[] connections ) throws PdbConnectException
  {
    new PdbSettingsWriter( connections ).writeConnections( getConnectionPreferences() );
  }

  private ISecurePreferences getConnectionPreferences( )
  {
    final ISecurePreferences securePreferences = SecurePreferencesFactory.getDefault();
    return securePreferences.node( SECURE_SETTINGS_NODE );
  }

  public PdbSettingsRegistry getConnectionRegistry( )
  {
    return m_registry;
  }
}