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

import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A workspace change listener is notified of changes to features in the workspace.
 * <p>
 * Clients may implement this interface.
 * </p>
 * 
 * @see IGmlWorkspaceDelta
 * @see GmlWorkspace#addChangeListener(IGmlWorkspaceChangeListener)
 * @author Gernot Belger
 */
public interface IGmlWorkspaceChangeListener
{
  /**
   * Notifies this listener that some workspace changes have happened.
   * <p>
   * The supplied delta gives details. This delta is valid only for the duration of the invocation of this method.
   * </p>
   * <p>
   * Note: This method is called by the platform; it is not intended to be called directly by clients.
   * <p>
   * Note that during resource change event notification, further changes to features may be disallowed.
   * </p>
   * 
   * @param source
   *          an object identifying the source of this event TODO: descripe who may this be
   * @param delta
   *          the workspace delta, always starting with a null feature
   * @see IGmlWorkspaceDelta
   */
  public void resourceChanged( final Object source, final GMLWorkspace workspace, final IGmlWorkspaceDelta delta );
}
