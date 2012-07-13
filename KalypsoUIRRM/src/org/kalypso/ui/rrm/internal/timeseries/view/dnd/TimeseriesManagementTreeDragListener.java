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
package org.kalypso.ui.rrm.internal.timeseries.view.dnd;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;

/**
 * @author Dirk Kuch
 */
public class TimeseriesManagementTreeDragListener implements DragSourceListener
{
  private final TreeViewer m_treeViewer;

  public TimeseriesManagementTreeDragListener( final TreeViewer treeViewer )
  {
    m_treeViewer = treeViewer;
  }

  @Override
  public void dragStart( final DragSourceEvent event )
  {
    // dragSetData( event );
  }

  @Override
  public void dragSetData( final DragSourceEvent event )
  {
    // resolve selected layer
    final IStructuredSelection selection = (IStructuredSelection) m_treeViewer.getSelection();
    final ITimeseries[] timeseries = TimeseriesManagementTreeDndSupport.doSelection( selection );

    event.data = timeseries;
  }

  @Override
  public void dragFinished( final DragSourceEvent event )
  {
  }
}