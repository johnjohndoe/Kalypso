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
package org.kalypsodeegree_impl.model.feature.event;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta;

/**
 * This factory creates {@link GmlWorkspaceDelta}s.
 * <p>
 * For each event which should be sent from the workspace, one factory-object ist responsible.
 * <p>
 * The factory collects simple changes (feature, value) and create a tree of workspace deltas.
 * 
 * @author Gernot Belger
 */
public class GmlWorkspaceDeltaFactory
{
  private final Map<Feature, Map<IPropertyType, GmlWorkspaceDelta>> m_deltaMap = new HashMap<Feature, Map<IPropertyType, GmlWorkspaceDelta>>();

  private final GMLWorkspace m_workspace;

  private GmlWorkspaceDelta m_rootDelta;

  /** Create an instance each time an event is created to be sent. */
  public GmlWorkspaceDeltaFactory( final GMLWorkspace workspace )
  {
    m_workspace = workspace;
    m_rootDelta = new GmlWorkspaceDelta( null, null, IGmlWorkspaceDelta.NO_CHANGE );
  }

  /**
   * Adds a new workspace delta to the tree of deltas. Don't call this method after {@link #createDelta()} was called
   * once.
   * 
   * @throws IllegalStateException
   *           If this method is called after {@link #createDelta()} was called.
   */
  public void addDelta( final Feature feature, final IPropertyType property, final int kind )
  {
    if( m_rootDelta == null )
      throw new IllegalStateException( "Do not reuse this object. Create a new one after createDelta was called." );

    Assert.isNotNull( feature, "Feature must alwas be non null." );

    Assert.isTrue( m_workspace.contains( feature ), "Feature must be contained in the main workspace." );

    switch( kind )
    {
      case IGmlWorkspaceDelta.NO_CHANGE:
        throw new IllegalArgumentException( "Kind NO_CHANGE is only intended to be used internally." );

      case IGmlWorkspaceDelta.ADDED:
      case IGmlWorkspaceDelta.REMOVED:
        Assert.isTrue( property == null, "If kind ADDED or REMOVED is specified, property must be null." );
        break;

      case IGmlWorkspaceDelta.CHANGED:
        Assert.isNotNull( property, "If kind CHANGED is specified, property must be non null." );
        break;

      default:
        throw new IllegalArgumentException( "Only the pure kinds ADDED, REMOVED and CHANGED are accepted." );
    }

    addDeltaInternal( feature, property, kind );
  }

  private GmlWorkspaceDelta addDeltaInternal( final Feature feature, final IPropertyType property, final int kind )
  {
    // do we have this delta already? if yes, just add kind to it
    if( !m_deltaMap.containsKey( feature ) )
      m_deltaMap.put( feature, new HashMap<IPropertyType, GmlWorkspaceDelta>() );

    final Map<IPropertyType, GmlWorkspaceDelta> propertyMap = m_deltaMap.get( feature );
    if( propertyMap.containsKey( property ) )
    {
      /* the delta is already sorted into the hirarchy, so we only may have to tweak the kind */
      final GmlWorkspaceDelta delta = propertyMap.get( property );

      /* The next one is a sanity check. Maybe it is possible to add/remove a feature at the same time? */
      if( property == null && delta.getKind() != kind )
        throw new IllegalArgumentException( "A feature cannot be added and removed at the same time." );

      /* All other cases do not need something to do. */
      return delta;
    }
    else
    {
      final GmlWorkspaceDelta newDelta = new GmlWorkspaceDelta( feature, property, kind );
      propertyMap.put( property, newDelta );

      final Feature parent = feature.getParent();
      final IPropertyType parentProperty = feature.getParentRelation();
      final GmlWorkspaceDelta parentDelta = findParentDelta( parent, parentProperty );
      parentDelta.addChild( newDelta );

      return newDelta;
    }
  }

  private GmlWorkspaceDelta findParentDelta( final Feature parent, final IPropertyType parentProperty )
  {
    if( parent == null )
      return m_rootDelta;

    return addDeltaInternal( parent, parentProperty, IGmlWorkspaceDelta.NO_CHANGE );
  }

  /**
   * Returns the tree of deltas which was created by calls to addDelta. Do not add further deltas after calling this
   * method.
   */
  public IGmlWorkspaceDelta createDelta( )
  {
    final GmlWorkspaceDelta gmlWorkspaceDelta = m_rootDelta;

    /* Clear state of this class */
    m_rootDelta = null;

    return gmlWorkspaceDelta;
  }

}
