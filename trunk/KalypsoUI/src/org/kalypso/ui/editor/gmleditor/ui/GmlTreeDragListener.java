/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.awt.dnd.DragSourceAdapter;

import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class GmlTreeDragListener extends DragSourceAdapter implements DragSourceListener
{

  private GmlTreeView m_viewer;

  /**
   *  
   */

  public GmlTreeDragListener( GmlTreeView viewer )
  {
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.swt.dnd.DragSourceListener#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
   */
  public void dragStart( DragSourceEvent event )
  {
    System.out.println( "Drag-Start" );

  }

  /**
   * @see org.eclipse.swt.dnd.DragSourceListener#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
   */
  public void dragSetData( DragSourceEvent event )
  {
    Object source = event.getSource();
    System.out.println( "\nDrag-SetData: " + source );

  }

  /**
   * @see org.eclipse.swt.dnd.DragSourceListener#dragFinished(org.eclipse.swt.dnd.DragSourceEvent)
   */
  public void dragFinished( DragSourceEvent event )
  {
    System.out.println( "Drag-Finished" );

  }

}
