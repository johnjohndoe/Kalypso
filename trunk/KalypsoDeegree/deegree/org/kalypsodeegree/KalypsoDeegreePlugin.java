/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.kalypso.preferences.IKalypsoDeegreePreferences;
import org.kalypso.transformation.CRSHelper;
import org.osgi.framework.BundleContext;

public class KalypsoDeegreePlugin extends Plugin
{
  /**
   * The shared instance.
   */
  private static KalypsoDeegreePlugin m_plugin;

  /**
   * Storage for preferences.
   */
  private ScopedPreferenceStore m_preferenceStore;

  /**
   * The coordinate system.
   */
  private String m_coordinateSystem;

  /**
   * The constructor.
   */
  public KalypsoDeegreePlugin( )
  {
    super();

    m_plugin = this;
    m_preferenceStore = null;
    m_coordinateSystem = null;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    savePluginPreferences();

    m_plugin = null;
    m_preferenceStore = null;
    m_coordinateSystem = null;

    super.stop( context );
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoDeegreePlugin getDefault( )
  {
    return m_plugin;
  }

  /**
   * This function returns the coordinate system set in the preferences.
   * 
   * @return The coordinate system.
   */
  public String getCoordinateSystem( )
  {
    if( m_coordinateSystem == null )
    {
      String crsName = getPluginPreferences().getString( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING );
      final boolean knownCRS = CRSHelper.isKnownCRS( crsName );

      if( crsName == null || !knownCRS )
      {
        getPluginPreferences().setValue( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING, IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE );
        System.out.println( "CRS \"" + crsName + "\" in preferences is unknown. setting preferences to CRS \"" + IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE + "\"" );
        crsName = IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE;
      }

      m_coordinateSystem = crsName;
    }

    return m_coordinateSystem;
  }

  /**
   * Copied from {@link org.eclipse.ui.plugin.AbstractUIPlugin}.
   * <p>
   * Returns the preference store for this UI plug-in. This preference store is used to hold persistent settings for
   * this plug-in in the context of a workbench. Some of these settings will be user controlled, whereas others may be
   * internal setting that are never exposed to the user.
   * <p>
   * If an error occurs reading the preference store, an empty preference store is quietly created, initialized with
   * defaults, and returned.
   * </p>
   * <p>
   * <strong>NOTE:</strong> As of Eclipse 3.1 this method is no longer referring to the core runtime compatibility
   * layer and so plug-ins relying on Plugin#initializeDefaultPreferences will have to access the compatibility layer
   * themselves.
   * </p>
   * 
   * @return the preference store
   */
  public IPreferenceStore getPreferenceStore( )
  {
    /* Create the preference store lazily. */
    if( m_preferenceStore == null )
      m_preferenceStore = new ScopedPreferenceStore( new InstanceScope(), getBundle().getSymbolicName() );

    return m_preferenceStore;
  }
}