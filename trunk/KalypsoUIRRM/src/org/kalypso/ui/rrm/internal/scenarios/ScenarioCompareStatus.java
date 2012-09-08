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
package org.kalypso.ui.rrm.internal.scenarios;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * The scenario compare status contains stati for several cases.
 *
 * @author Holger Albert
 */
public class ScenarioCompareStatus
{
  public static final String KEY_HYDROTOPES = "hydrotopes"; //$NON-NLS-1$

  public static final String KEY_MODEL = "model"; //$NON-NLS-1$

  public static final String KEY_PARAMETER = "parameter"; //$NON-NLS-1$

  /**
   * The stati.
   */
  private final Map<String, Map<String, IStatus>> m_stati;

  /**
   * The constructor.
   */
  public ScenarioCompareStatus( )
  {
    m_stati = new HashMap<>();
  }

  /**
   * This function stores a status for an uri and a key.
   *
   * @param uri
   *          The uri of the scenario.
   * @param key
   *          The key.
   * @param status
   *          The status.
   */
  public void putStatus( final String uri, final String key, final IStatus status )
  {
    if( !m_stati.containsKey( uri ) )
      m_stati.put( uri, new HashMap<String, IStatus>() );

    final Map<String, IStatus> stati = m_stati.get( uri );
    stati.put( key, status );
  }

  /**
   * This function returns true, if a status for an uri and a key is contained.
   *
   * @param uri
   *          The uri of the scenario.
   * @param key
   *          The key.
   * @return True, if a status for an uri and a key is contained.
   */
  public boolean hasStatus( final String uri, final String key )
  {
    if( !m_stati.containsKey( uri ) )
      return false;

    final Map<String, IStatus> stati = m_stati.get( uri );
    return stati.containsKey( key );
  }

  /**
   * This function returns a status for an uri and a key.
   *
   * @param uri
   *          The uri of the scenario.
   * @param key
   *          The key.
   * @return The status.
   */
  public IStatus getStatus( final String uri, final String key )
  {
    if( !m_stati.containsKey( uri ) )
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "ScenarioCompareStatus_6" ) ); //$NON-NLS-1$

    final Map<String, IStatus> stati = m_stati.get( uri );
    if( !stati.containsKey( key ) )
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "ScenarioCompareStatus_6" ) ); //$NON-NLS-1$

    return stati.get( key );
  }

  /**
   * This function returns a merged status for an uri. It will merge all stati of all keys for that uri.
   *
   * @param uri
   *          The uri of the scenario.
   * @param statusLabel
   *          The label for the merged status.
   * @return The merged status.
   */
  public IStatus getMergedStatus( final String uri, final String statusLabel )
  {
    if( !m_stati.containsKey( uri ) )
      return null;

    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final Map<String, IStatus> stati = m_stati.get( uri );
    final String[] keys = stati.keySet().toArray( new String[] {} );
    for( final String key : keys )
    {
      final IStatus status = stati.get( key );
      if( status == null )
        continue;

      if( !status.isMultiStatus() )
      {
        collector.add( status );
        continue;
      }

      final IStatusCollector collector1 = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      final IStatus[] children = status.getChildren();
      for( final IStatus oneStatus : children )
        collector1.add( oneStatus );

      collector.add( collector1.asMultiStatus( getLabel( key ) ) );
    }

    return collector.asMultiStatus( statusLabel );
  }

  private String getLabel( final String key )
  {
    if( KEY_MODEL.equals( key ) )
      return Messages.getString("ScenarioCompareStatus.0"); //$NON-NLS-1$

    if( KEY_PARAMETER.equals( key ) )
      return Messages.getString("ScenarioCompareStatus.1"); //$NON-NLS-1$

    if( KEY_HYDROTOPES.equals( key ) )
      return Messages.getString("ScenarioCompareStatus.2"); //$NON-NLS-1$

    return key;
  }
}