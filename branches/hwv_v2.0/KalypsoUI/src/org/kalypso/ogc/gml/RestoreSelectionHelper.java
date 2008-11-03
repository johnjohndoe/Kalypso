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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A pool-listener on a {@link IPoolableObjectType}, which support saving/restoring of the selection state, when the
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
public class RestoreSelectionHelper
{
  private final IFeatureSelectionManager m_selectionManager;

  /** Stored selection */
  private String[] m_oldSelectionState = null;

  private final IPoolableObjectType m_key;

  public RestoreSelectionHelper( final IPoolableObjectType key, final IFeatureSelectionManager selectionManager )
  throws CoreException
  {
    m_key = key;
    m_selectionManager = selectionManager;

    saveSelection( getWorkspace( m_key ) );
  }

  private CommandableWorkspace getWorkspace( final IPoolableObjectType key ) throws CoreException
  {
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
    return (CommandableWorkspace) pool.getObject( key );
  }

  public void restoreSelection() throws CoreException
  {
    // if we have nothing in store, just return
    if( m_oldSelectionState == null )
      return;

    final CommandableWorkspace workspace = getWorkspace( m_key );
    if( workspace == null ) // REMARK: kommisch dass der Workspace hier null ist nach der Rechnung...
      return;

    final List<EasyFeatureWrapper> easyFeatures = new ArrayList<EasyFeatureWrapper>( m_oldSelectionState.length );
    for( final String fid : m_oldSelectionState )
    {
      if( fid != null )
      {
        final Feature feature = workspace.getFeature( fid );
        if( feature != null )
          easyFeatures.add( new EasyFeatureWrapper( workspace, feature, null, null ) );
      }
    }
    final EasyFeatureWrapper[] easyArray = easyFeatures
    .toArray( new EasyFeatureWrapper[easyFeatures.size()] );

    // TODO: we should do a clean() and than select the new features because:
    // - we need both, else some models do not refresh, as exactly the same feature get selected
    // - in this case we can avoid cleaning and restoring (!) the selection before the calculation
    // IN order to make this work, it must be fixed, that the observation occur twice in the legend of the diagram
    // (and twice in the tables, if two selection event occur in a short time.
    
    final Feature[] selectionToRemove = FeatureSelectionHelper.getFeatures( m_selectionManager );
    m_selectionManager.changeSelection( selectionToRemove, easyArray );
  }

  private void saveSelection( final CommandableWorkspace workspace )
  {
    final Feature[] features = FeatureSelectionHelper.getFeatures( m_selectionManager, workspace );

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
  }
}
