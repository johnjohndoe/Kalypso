/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A pool-listener on a {@link PoolableObjectType}, which support saving/restoring of the selection state, when the
 * workspace gets invalidated.
 * <p>
 * The associated object must be a {@link CommandableWorkspace}.
 * </p>
 * <p>
 * Must be disposed off, after usage.
 * </p>
 * <p>
 * The current selection gets stored, when the workspace changes.
 * </p>
 * <p>
 * To restore the previously stored selection, call {@link #restoreSelection()}.
 * </p>
 * 
 * @author belger
 */
public class RestoreSelectionListener implements IPoolListener
{
  private final IFeatureSelectionManager m_selectionManager;

  /** Stored selection, if the workspace gets invalidated. */
  private String[] m_oldSelectionState = null;

  private CommandableWorkspace m_workspace;

  public RestoreSelectionListener( final PoolableObjectType key, final IFeatureSelectionManager selectionManager )
  {
    m_selectionManager = selectionManager;

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.addPoolListener( this, key );
  }

  public void dispose()
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( m_workspace != null )
      saveSelection();

    if( status.isOK() )
      m_workspace = (CommandableWorkspace)newValue;
    else
      m_workspace = null;
  }

  public void restoreSelection()
  {
    // if we have nothing in store, just return
    if( m_oldSelectionState == null )
      return;

    System.out.println( "Restoring selection to: " + ArrayUtils.toString( m_oldSelectionState ) );

    final List easyFeatures = new ArrayList( m_oldSelectionState.length );
    for( int i = 0; i < m_oldSelectionState.length; i++ )
    {
      final String fid = m_oldSelectionState[i];
      if( fid != null )
      {
        final Feature feature = m_workspace.getFeature( fid );
        if( feature != null )
          easyFeatures.add( new EasyFeatureWrapper( m_workspace, feature, null, null ) );
      }
    }
    final EasyFeatureWrapper[] easyArray = (EasyFeatureWrapper[])easyFeatures
        .toArray( new EasyFeatureWrapper[easyFeatures.size()] );

    final Feature[] selectionToRemove = FeatureSelectionHelper.getFeatures(m_selectionManager);
    m_selectionManager.changeSelection( selectionToRemove, easyArray );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    saveSelection();

    m_workspace = null;
  }

  private void saveSelection()
  {
    final Feature[] features = FeatureSelectionHelper.getFeatures( m_selectionManager, m_workspace );

    // do nothing, if no features where selected
    // this avoids concurrency problems if the same features are selected within another theme
    if( features.length != 0 )
    {
      // deselect old selection
      m_selectionManager.changeSelection( features, new EasyFeatureWrapper[0] );

      m_oldSelectionState = new String[features.length];
      for( int i = 0; i < features.length; i++ )
      {
        final Feature feature = features[i];
        final String id = feature.getId();
        m_oldSelectionState[i] = id;
      }
    }
    else
      m_oldSelectionState = null;

    System.out.println( "Selection was saved to: " + ArrayUtils.toString( m_oldSelectionState ) );
  }
}
