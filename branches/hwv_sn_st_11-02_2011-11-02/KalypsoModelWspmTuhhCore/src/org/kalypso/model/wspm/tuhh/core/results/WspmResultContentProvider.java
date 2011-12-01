/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.results;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;

/**
 * @author Gernot Belger
 */
public class WspmResultContentProvider implements ITreeContentProvider
{
  private static final String DATA_PROPERTY = "wspmResultTreeProperty"; //$NON-NLS-1$

  enum Property
  {
    LABEL;
  }

  public static void initTreeViewer( final TreeViewer viewer )
  {
    final Tree tree = viewer.getTree();
    viewer.setAutoExpandLevel( 3 );

    final TreeColumn labelColumn = new TreeColumn( tree, SWT.NONE );
    labelColumn.setData( DATA_PROPERTY, Property.LABEL );
    labelColumn.setWidth( tree.getSize().x - 5 );

    tree.addControlListener( new ControlAdapter()
    {
      /**
       * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
       */
      @Override
      public void controlResized( final ControlEvent e )
      {
        labelColumn.setWidth( tree.getSize().x - 5 );
      }
    } );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IWspmResultNode )
      return ((IWspmResultNode) parentElement).getChildResults();

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof IWspmResultNode )
      return ((IWspmResultNode) element).getParent();

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    if( element instanceof IWspmResultNode )
      return ((IWspmResultNode) element).getChildResults().length > 0;

    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement instanceof IWspmResultNode )
      return ((IWspmResultNode) inputElement).getChildResults();

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    final Item[] items = getItems( viewer );

    final String[] properties = new String[items.length];
    for( int i = 0; i < properties.length; i++ )
      properties[i] = ((Property) items[i].getData( DATA_PROPERTY )).name();

    final ColumnViewer columnViewer = (ColumnViewer) viewer;
    columnViewer.setColumnProperties( properties );
  }

  private Item[] getItems( final Viewer viewer )
  {
    if( viewer instanceof TreeViewer )
      return ((TreeViewer) viewer).getTree().getColumns();

    if( viewer instanceof TableViewer )
      return ((TableViewer) viewer).getTable().getColumns();

    return new Item[0];
  }
}
