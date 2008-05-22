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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A workspace delta represents changes in the state of a gml workspace between two discrete points in time.
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * @see org.kalypsodeegree_impl.model.feature.GMLWorkspace
 * @see org.kalypsodeegree.model.feature.Feature
 * @author Gernot Belger
 */
public interface IGmlWorkspaceDelta
{
  /*
   * Constants defining feature delta kinds:
   */

  /**
   * Delta kind constant indicating that the feature has not been changed in any way.
   * 
   * @see IGmlWorkspaceDelta#getKind()
   */
  public static final int NO_CHANGE = 0x0;

  /**
   * Delta kind constant (bit mask) indicating that the feature has been added
   * 
   * @see IGmlWorkspaceDelta#getKind()
   */
  public static final int ADDED = 0x1;

  /**
   * Delta kind constant (bit mask) indicating that the (inlined) feature has been removed
   * 
   * @see IGmlWorkspaceDelta#getKind()
   */
  public static final int REMOVED = 0x2;

  /**
   * Delta kind constant (bit mask) indicating that the feature has been changed.
   * <p>This may be the case if
   * <ul>
   *    <li>a value of a property was set</li>
   *    <li>the list value of a property has changed (an item was added or removed)</li>
   * </ul>.
   * If this flag is set, {@link #getProperty()} returns the changed property.    
   * 
   * @see IResourceDelta#getKind()
   */
  public static final int CHANGED = 0x4;

  /**
   * Returns the kind of this resource delta. Normally, one of <code>ADDED</code>, <code>REMOVED</code>,
   * <code>CHANGED</code>.
   * 
   * @return the kind of this resource delta
   * @see IResourceDelta#ADDED
   * @see IResourceDelta#REMOVED
   * @see IResourceDelta#CHANGED
   */
  public int getKind( );

  /**
   * Returns a the affected feature.
   * <p>
   * For additions (<code>ADDED</code>), this is the newly-added feature.
   * <p>
   * For changes (<code>CHANGED</code>), this is the changed feature.
   * <p>
   * For removals (<code>REMOVED</code>), this is the deleted feature. Even though this resource would not normally
   * exist in the current workspace, the type and content of the feature can be determined from the handle.
   * <p>
   * 
   * @return the affected feature
   */
  public Feature getFeature( );

  /**
   * Returns a the affected property.
   * <p>
   * For additions (<code>ADDED</code>), this is always null.
   * <p>
   * For changes (<code>CHANGED</code>), this is the changed property.
   * <p>
   * For removals (<code>REMOVED</code>), this is always null.
   * <p>
   * 
   * @return the affected property or null
   */
  public IPropertyType getProperty();
  
  /**
   * Finds and returns the descendent delta of all features identified by the given qname in this delta, or
   * <code>null</code> if no such descendent exists.
   * <p>
   * This is a convenience method to avoid manual traversal of the delta tree in cases where the listener is only
   * interested in changes to particular features. Calling this method will generally be faster than manually traversing
   * the delta to a particular descendent.
   * </p>
   * 
   * @param qname
   *          the qname of the desired descendent delta
   * @return the descendent delta, or <code>null</code> if no such descendent exists in the delta
   */
  public IGmlWorkspaceDelta[] findMember( final QName qname );

  /**
   * Returns workspace deltas for all children of this delta which were added, removed, or changed. Returns an empty
   * array if there are no affected children.
   * 
   * @return the workspace deltas for all affected children
   * @see IGmlWorkspaceDelta#ADDED
   * @see IGmlWorkspaceDelta#REMOVED
   * @see IGmlWorkspaceDelta#CHANGED
   * @see #getAffectedChildren(int)
   */
  public IGmlWorkspaceDelta[] getAffectedChildren( );

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
