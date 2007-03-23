/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypsodeegree.model.feature.event;

import org.eclipse.core.runtime.CoreException;

/**
 * An objects that visits workspace deltas.
 * <p>
 * Usage:
 * 
 * <pre>
 *    class Visitor implements IGmlWorkspaceDeltaVisitor {
 *        public boolean visit(IGmlWorkspaceDelta delta) {
 *            switch (delta.getKind()) {
 *            case IGmlWorkspaceDelta.ADDED :
 *                // handle added feature
 *                break;
 *            case IGmlWorkspaceDelta.REMOVED :
 *                // handle removed feature
 *                break;
 *            case IGmlWorkspaceDelta.CHANGED :
 *                // handle changed feature
 *                break;
 *            }
 *        return true;
 *        }
 *    }
 *    IGmlWorkspaceDelta rootDelta = ...;
 *    rootDelta.accept(new Visitor());
 * </pre>
 * 
 * </p>
 * <p>
 * Clients may implement this interface.
 * </p>
 * 
 * @author Gernot Belger
 */
public interface IGmlWorkspaceDeltaVisitor
{
  /**
   * Visits the given workspace delta.
   * 
   * @return <code>true</code> if the workspace delta's children should be visited; <code>false</code> if they should
   *         be skipped.
   * @exception CoreException
   *              if the visit fails for some reason.
   */
  public boolean visit( final IGmlWorkspaceDelta delta ) throws CoreException;
}
