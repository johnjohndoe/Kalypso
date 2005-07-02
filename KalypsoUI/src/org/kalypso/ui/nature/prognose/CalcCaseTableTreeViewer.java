/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.nature.prognose;

import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckable;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.custom.TableTreeItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.contribs.eclipse.jface.viewers.IViewerSorterFactory;
import org.kalypso.contribs.eclipse.swt.widgets.TableSorter;

/**
 * A Table tree viewer, showing calc cases in a tree with additional column 'modiifed since'.
 * 
 * @author belger
 */
public class CalcCaseTableTreeViewer extends TableTreeViewer implements ICheckable
{
  /**
   * List of check state listeners (element type: <code>ICheckStateListener</code>).
   */
  private ListenerList m_checkStateListeners = new ListenerList( 3 );

  private final TableSorter m_tableSorter = new TableSorter( new IViewerSorterFactory()
  {
    public ViewerSorter createSorter( final int columnIndex, final boolean inverse )
    {
      return new CalcCaseViewerSorter( columnIndex, inverse );
    }
  } );

  public CalcCaseTableTreeViewer( final IFolder markedCalcCase, final TableTree tree )
  {
    super( tree );

    init( markedCalcCase );
  }

  public CalcCaseTableTreeViewer( final IFolder markedCalcCase, final Composite parent )
  {
    super( parent );

    init( markedCalcCase );
  }

  public CalcCaseTableTreeViewer( final IFolder markedCalcCase, final Composite parent, int style )
  {
    super( parent, style );

    init( markedCalcCase );
  }

  /**
   * Post construction, used by all constutors. Initialized default label and content providers.
   */
  private void init( final IFolder markedCalcCase )
  {
    final TableTree tableTree = getTableTree();
    tableTree.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final Table table = tableTree.getTable();
    table.setHeaderVisible( true );

    final TableColumn column = new TableColumn( table, SWT.NONE );
    column.setText( "" );
    column.setWidth( 0 );
    m_tableSorter.createSortedColumn( table, "Vorhersage", this );
    m_tableSorter.createSortedColumn( table, "zuletzt geändert", this );

    final Color markedColor = tableTree.getDisplay().getSystemColor( SWT.COLOR_YELLOW );
    setLabelProvider( new CalcCaseTableLabelProvider( markedCalcCase, markedColor ) );
    setContentProvider( new CalcCaseTreeContentProvider() );
  }

  private static final class CalcCaseViewerSorter extends ViewerSorter
  {
    private final int m_columnIndex;

    private final int m_sign;

    public CalcCaseViewerSorter( final int columnIndex, final boolean inverse )
    {
      m_columnIndex = columnIndex;
      m_sign = inverse ? -1 : 1;
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerSorter#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
     *      java.lang.Object)
     */
    public int compare( Viewer viewer, Object e1, Object e2 )
    {
      if( e1 instanceof IFolder && e2 instanceof IFolder )
      {
        final IFolder f1 = (IFolder)e1;
        final IFolder f2 = (IFolder)e2;

        switch( m_columnIndex )
        {
        case 1:
          final String n1 = f1.getName();
          final String n2 = f2.getName();
          return n1.compareToIgnoreCase( n2 ) * m_sign;

        case 2:
          final Date m1 = CalcCaseTableLabelProvider.lastModifiedFromFolder( f1 );
          final Date m2 = CalcCaseTableLabelProvider.lastModifiedFromFolder( f2 );
          return m1.compareTo( m2 ) * m_sign;

        default:
          break;
        }
      }

      return super.compare( viewer, e1, e2 );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.AbstractTreeViewer#inputChanged(java.lang.Object, java.lang.Object)
   */
  protected void inputChanged( Object input, Object oldInput )
  {
    super.inputChanged( input, oldInput );

    final TableColumn[] columns = getTableTree().getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
      columns[i].pack();
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckable#addCheckStateListener(org.eclipse.jface.viewers.ICheckStateListener)
   */
  public void addCheckStateListener( final ICheckStateListener listener )
  {
    m_checkStateListeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckable#getChecked(java.lang.Object)
   */
  public boolean getChecked( Object element )
  {
    final Widget widget = findItem( element );
    if( widget instanceof TableTreeItem )
      return ( (TableTreeItem)widget ).getChecked();

    return false;
  }

  /**
   * Returns a list of elements corresponding to checked table items in this viewer.
   * <p>
   * This method is typically used when preserving the interesting state of a viewer; <code>setCheckedElements</code>
   * is used during the restore.
   * </p>
   * 
   * @return the array of checked elements
   * @see #setCheckedElements
   */
  public Object[] getCheckedElements()
  {
    final TableTreeItem[] children = getTableTree().getItems();
    ArrayList v = new ArrayList( children.length );
    for( int i = 0; i < children.length; i++ )
    {
      final TableTreeItem item = children[i];
      if( item.getChecked() )
        v.add( item.getData() );
    }
    return v.toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckable#setChecked(java.lang.Object, boolean)
   */
  public boolean setChecked( final Object element, final boolean state )
  {
    Assert.isNotNull( element );
    final Widget widget = findItem( element );
    if( widget instanceof TableTreeItem )
    {
      ( (TableTreeItem)widget ).setChecked( state );
      return true;
    }
    return false;
  }

  /**
   * Sets which nodes are checked in this viewer. The given list contains the elements that are to be checked; all other
   * nodes are to be unchecked.
   * <p>
   * This method is typically used when restoring the interesting state of a viewer captured by an earlier call to
   * <code>getCheckedElements</code>.
   * </p>
   * 
   * @param elements
   *          the list of checked elements (element type: <code>Object</code>)
   * @see #getCheckedElements
   */
  public void setCheckedElements( Object[] elements )
  {
    assertElementsNotNull( elements );
    final Hashtable set = new Hashtable( elements.length * 2 + 1 );
    for( int i = 0; i < elements.length; ++i )
    {
      set.put( elements[i], elements[i] );
    }
    final TableTreeItem[] items = getTableTree().getItems();
    for( int i = 0; i < items.length; ++i )
    {
      final TableTreeItem item = items[i];
      final Object element = item.getData();
      if( element != null )
      {
        final boolean check = set.containsKey( element );
        // only set if different, to avoid flicker
        if( item.getChecked() != check )
          item.setChecked( check );
      }
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckable#removeCheckStateListener(org.eclipse.jface.viewers.ICheckStateListener)
   */
  public void removeCheckStateListener( final ICheckStateListener listener )
  {
    m_checkStateListeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#handleSelect(org.eclipse.swt.events.SelectionEvent)
   */
  public void handleSelect( SelectionEvent event )
  {
    if( event.detail == SWT.CHECK )
    {
      super.handleSelect( event ); // this will change the current selection

      final TableTreeItem item = (TableTreeItem)event.item;
      final Object data = item.getData();
      if( data != null )
        fireCheckStateChanged( new CheckStateChangedEvent( this, data, item.getChecked() ) );
    }
    else
      super.handleSelect( event );
  }

  /**
   * Notifies any check state listeners that a check state changed has been received. Only listeners registered at the
   * time this method is called are notified.
   * 
   * @param event
   *          a check state changed event
   * 
   * @see ICheckStateListener#checkStateChanged
   */
  private void fireCheckStateChanged( final CheckStateChangedEvent event )
  {
    Object[] array = m_checkStateListeners.getListeners();
    for( int i = 0; i < array.length; i++ )
    {
      final ICheckStateListener l = (ICheckStateListener)array[i];
      Platform.run( new SafeRunnable()
      {
        public void run()
        {
          l.checkStateChanged( event );
        }
      } );
    }
  }

  /*
   * (non-Javadoc) Method declared on Viewer.
   */
  protected void preservingSelection( Runnable updateCode )
  {
    final TableTreeItem[] children = getTableTree().getItems();
    final Hashtable checked = new Hashtable( children.length * 2 + 1 );
    //  	Hashtable grayed = new Hashtable(children.length*2+1);

    for( int i = 0; i < children.length; i++ )
    {
      final TableTreeItem item = children[i];
      final Object data = item.getData();
      if( data != null )
      {
        if( item.getChecked() )
          checked.put( data, data );
        //  			if (item.getGrayed())
        //  				grayed.put(data, data);
      }
    }

    super.preservingSelection( updateCode );

    final TableTreeItem[] newChildren = getTableTree().getItems();
    for( int i = 0; i < newChildren.length; i++ )
    {
      final TableTreeItem item = newChildren[i];
      final Object data = item.getData();
      if( data != null )
      {
        item.setChecked( checked.containsKey( data ) );
        //        item.setGrayed( grayed.containsKey( data ) );
      }
    }
  }
}
