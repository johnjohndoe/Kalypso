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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;

/**
 * A feature delta represents changes in the state of a feature between two discrete points in time.
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * @see org.kalypsodeegree_impl.model.feature.GMLWorkspace
 * @see org.kalypsodeegree.model.feature.Feature
 * @author Gernot Belger
 */
public interface IFeatureDelta
{
  /**
   * Returns a the affected feature id.
   * 
   * @return the affected feature id
   */
  public String getId( );

  /** The feature type of the feature this delta cooresponds to. */
  public IFeatureType getFeatureType();
  
  /**
   * Returns the parent delta.
   * 
   * @return The feature dleta of the parent feature, null iff this is the root delta.
   */
  public IFeatureDelta getParentDelta( );

  /** The parent relation of the feature this delta corresponds to. */
  public IPropertyType getParentRelation( );

  /**
   * Returns a the affected properties.
   * 
   * @return the affected properties (may be empty)
   */
  public IPropertyDelta[] getPropertyDeltas( );

  /**
   * Accepts the given visitor. The visitor's <code>visit</code> method is called with this delta. If the visitor
   * returns <code>true</code>, the resource delta's children are also visited.
   * 
   * @param visitor
   *          the visitor
   * @exception CoreException
   *              if the visitor failed with this exception.
   * @see IGmlWorkspaceDeltaVisitor#visit(IGmlWorkspaceDelta)
   * @since 2.0
   */
  public void accept( final IGmlWorkspaceDeltaVisitor visitor ) throws CoreException;
}
