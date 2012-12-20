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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.widgets.TreeItem;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class HideEmptyNodeFilter extends ViewerFilter
{

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {

    if( element instanceof TreeNode )
    {
      final TreeNode node = (TreeNode) element;
      final ITreeNodeUiHandler handler = node.getUiHandler();
      if( handler instanceof EmptyNodeUiHandler )
      {
        return isVisible( (TreeViewer) viewer );
      }

    }

    return true;
  }

  private boolean isVisible( final TreeViewer viewer )
  {
    final TreeItem[] items = viewer.getTree().getItems();

    /* EmptyNodeUIHandler is always added and it's only visible when it's the only (visible (other filters)) tree item */
    if( ArrayUtils.getLength( items ) < 2 )
      return true;

    return false;
  }

}
