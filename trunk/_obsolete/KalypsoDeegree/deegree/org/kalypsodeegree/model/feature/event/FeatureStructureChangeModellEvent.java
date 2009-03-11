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
package org.kalypsodeegree.model.feature.event;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class FeatureStructureChangeModellEvent extends ModellEvent implements IGMLWorkspaceModellEvent
{
  private final GMLWorkspace m_workspace;

  private final Feature[] m_parentFeature;

  private final Feature[] m_changedFeature;

  public static final int STRUCTURE_CHANGE_ADD = 1;

  public static final int STRUCTURE_CHANGE_DELETE = 2;

  public static final int STRUCTURE_CHANGE_MOVE = 3;

  private final int m_changeType;

  /**
   * deprecated because changed features should be handed over
   */
  @Deprecated
  public FeatureStructureChangeModellEvent( final GMLWorkspace workspace, final Feature parentFeature, final int changeType )
  {
    this( workspace, new Feature[] { parentFeature }, changeType );
  }

  public FeatureStructureChangeModellEvent( final GMLWorkspace workspace, final Feature parentFeature, final Feature[] changedFeature, final int changeType )
  {
    this( workspace, new Feature[] { parentFeature }, changedFeature, changeType );
  }

  public FeatureStructureChangeModellEvent( final GMLWorkspace workspace, final Feature parentFeature, final Feature changedFeature, final int changeType )
  {
    this( workspace, new Feature[] { parentFeature }, new Feature[] { changedFeature }, changeType );
  }

  /**
   * deprecated because changed features should be handed over
   */
  @Deprecated
  public FeatureStructureChangeModellEvent( final GMLWorkspace workspace, final Feature[] parentFeature, final int changeType )
  {
    this( workspace, parentFeature, null, changeType );

  }

  public FeatureStructureChangeModellEvent( final GMLWorkspace workspace, final Feature[] parentFeature, final Feature[] changedFeature, final int changeType )
  {
    super( workspace, FEATURE_CHANGE );
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_changedFeature = changedFeature;
    m_changeType = changeType;
  }

  public Feature[] getParentFeatures( )
  {
    return m_parentFeature;
  }

  /**
   * Even if feature links in lists are added real features are returned. This has to be differentiated while catching
   * events.
   */
  public Feature[] getChangedFeatures( )
  {
    return m_changedFeature;
  }

  public GMLWorkspace getGMLWorkspace( )
  {
    return m_workspace;
  }

  public int getChangeType( )
  {
    return m_changeType;
  }
}
