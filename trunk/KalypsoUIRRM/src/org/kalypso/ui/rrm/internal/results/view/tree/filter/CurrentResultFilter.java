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
package org.kalypso.ui.rrm.internal.results.view.tree.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class CurrentResultFilter extends AbstractResultViewerFilter
{
  private boolean m_doFilter;

  @Override
  protected boolean doSelect( final TreeNode node )
  {
    if( !m_doFilter )
      return true;

    final int level = getLevel( node );
    if( level != 2 )
      return true;

    final Object data = node.getData();
    if( data instanceof IFolder )
    {
      final IFolder folder = (IFolder) data;

      // FIXME english project template
      if( StringUtils.equalsIgnoreCase( folder.getName(), "berechnet" ) ) /*
                                                                           * current result folder name set by project
                                                                           * import
                                                                           */
        return true;
      else if( StringUtils.equalsIgnoreCase( folder.getName(), RrmSimulation.FOLDER_AKTUELL ) )
        return true;

      return false;

    }

    return false;
  }

  public void setSelection( final boolean doFilter )
  {
    m_doFilter = doFilter;
  }

}
