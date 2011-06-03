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
package org.kalypso.model.wspm.pdb.ui.internal;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class WspmPdbUiPlugin extends AbstractUIPlugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.model.wspm.pdb.ui"; //$NON-NLS-1$

  // The shared instance
  private static WspmPdbUiPlugin plugin;

  private PluginImageProvider m_imgProvider = null;

  private PdbWspmProject m_wspmProject;

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;

    m_imgProvider = new PluginImageProvider( this );
    m_imgProvider.resetTmpFiles();
  }

  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );

    plugin = null;

    if( m_wspmProject != null )
    {
      m_wspmProject.dispose();
      m_wspmProject = null;
    }

    m_imgProvider.resetTmpFiles();
    m_imgProvider = null;
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static WspmPdbUiPlugin getDefault( )
  {
    return plugin;
  }

  public PluginImageProvider getImageProvider( )
  {
    return m_imgProvider;
  }

  public synchronized void setWspmProject( final PdbWspmProject wspmProject )
  {
    m_wspmProject = wspmProject;
  }

  public synchronized PdbWspmProject getWspmProject( )
  {
    return m_wspmProject;
  }
}