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
package org.kalypso.contribs.eclipse.ui.views.contentoutline;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * ContentOutlinePage that allows subclasses to specify which tree viewer to use
 * 
 * @author schlienger
 */
public abstract class ContentOutlinePage2 extends Page implements IContentOutlinePage, ISelectionChangedListener
{
  private ListenerList selectionChangedListeners = new ListenerList();

  protected TreeViewer treeViewer;

  /**
   * Create a new content outline page.
   */
  protected ContentOutlinePage2()
  {
    super();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    selectionChangedListeners.add( listener );
  }

  /**
   * The <code>ContentOutlinePage</code> implementation of this <code>IContentOutlinePage</code> method creates a
   * tree viewer using createTreeViewer( Composite ). Subclasses may override createTreeViewer to provide another
   * TreeViewer than the default one.
   * <p>
   * Subclasses must extend this method to configure the tree viewer with a proper content provider, label provider, and
   * input element.
   * 
   * @param parent
   */
  public void createControl( final Composite parent )
  {
    treeViewer = createTreeViewer( parent );
    treeViewer.addSelectionChangedListener( this );
  }

  /**
   * Creates an instance of TreeViewer. This default implementation creates a <code>TreeViewer</code>. You may choose
   * to override it and create an instance of another subclass of TreeViewer.
   * 
   * @param parent
   * @return new instance of TreeViewer
   */
  protected TreeViewer createTreeViewer( final Composite parent )
  {
    return new TreeViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
  }

  /**
   * Fires a selection changed event.
   * 
   * @param selection
   *          the new selection
   */
  protected void fireSelectionChanged( ISelection selection )
  {
    // create an event
    final SelectionChangedEvent event = new SelectionChangedEvent( this, selection );

    // fire the event
    Object[] listeners = selectionChangedListeners.getListeners();
    for( int i = 0; i < listeners.length; ++i )
    {
      final ISelectionChangedListener l = (ISelectionChangedListener)listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run()
        {
          l.selectionChanged( event );
        }
      } );
    }
  }

  /**
   * @see org.eclipse.ui.part.IPage#getControl()
   */
  public Control getControl()
  {
    if( treeViewer == null )
      return null;
    return treeViewer.getControl();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    if( treeViewer == null )
      return StructuredSelection.EMPTY;
    return treeViewer.getSelection();
  }

  /**
   * Returns this page's tree viewer.
   * 
   * @return this page's tree viewer, or <code>null</code> if <code>createControl</code> has not been called yet
   */
  protected TreeViewer getTreeViewer()
  {
    return treeViewer;
  }

  /**
   * @see org.eclipse.ui.part.IPageBookViewPage#init(org.eclipse.ui.part.IPageSite)
   */
  public void init( IPageSite pageSite )
  {
    super.init( pageSite );
    pageSite.setSelectionProvider( this );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    selectionChangedListeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    fireSelectionChanged( event.getSelection() );
  }

  /**
   * Sets focus to a part in the page.
   */
  public void setFocus()
  {
    treeViewer.getControl().setFocus();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( treeViewer != null )
      treeViewer.setSelection( selection );
  }
}