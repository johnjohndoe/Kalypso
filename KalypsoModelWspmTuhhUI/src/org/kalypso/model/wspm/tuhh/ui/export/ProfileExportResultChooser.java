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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLabelProvider;

/**
 * Shows the currently available WSPM results in a tree view and let the user check them.
 * 
 * @author Gernot Belger
 */
public class ProfileExportResultChooser
{
  private final Collection<ICheckStateListener> m_listeners = new HashSet<ICheckStateListener>();

  private final Set<IWspmResultNode> m_results = new HashSet<IWspmResultNode>();

  private final IWspmResultNode m_rootNode;

  public ProfileExportResultChooser( final IWspmResultNode rootNode )
  {
    m_rootNode = rootNode;
  }

  public Control createControl( final Composite parent )
  {
    // add a tree-table that shows the fetcher data
    final CheckboxTreeViewer treeViewer = new CheckboxTreeViewer( parent, SWT.SINGLE | SWT.FULL_SELECTION | SWT.CHECK | SWT.BORDER );
    WspmResultContentProvider.initTreeViewer( treeViewer );

    treeViewer.setContentProvider( new WspmResultContentProvider() );
    treeViewer.setLabelProvider( new WspmResultLabelProvider( treeViewer ) );
    treeViewer.setCheckStateProvider( new ICheckStateProvider()
    {
      @Override
      public boolean isGrayed( final Object element )
      {
        return !(element instanceof IWspmResult);
      }

      @Override
      public boolean isChecked( final Object element )
      {
        if( element instanceof IWspmResult )
          return hasResult( (IWspmResult) element );
        else
          return true;
      }
    } );
    treeViewer.setInput( m_rootNode );

    treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object element = event.getElement();
        if( element instanceof IWspmResultNode )
        {
          if( event.getChecked() )
            addResultNode( (IWspmResultNode) element );
          else
            removeResultNode( (IWspmResultNode) element );
        }
        else
          treeViewer.setChecked( element, true );

        fireCheckStateChanged( event );
      }
    } );

    return treeViewer.getControl();
  }

  protected boolean hasResult( final IWspmResult element )
  {
    return m_results.contains( element );
  }

  protected void addResultNode( final IWspmResultNode node )
  {
    m_results.add( node );
  }

  protected void removeResultNode( final IWspmResultNode node )
  {
    m_results.remove( node );
  }

  public IWspmResultNode[] getSelectedResults( )
  {
    return m_results.toArray( new IWspmResultNode[m_results.size()] );
  }

  public void addCheckStateListener( final ICheckStateListener l )
  {
    m_listeners.add( l );
  }

  public void removeCheckStateListener( final ICheckStateListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireCheckStateChanged( final CheckStateChangedEvent event )
  {
    final ICheckStateListener[] listeners = m_listeners.toArray( new ICheckStateListener[m_listeners.size()] );
    for( final ICheckStateListener listener : listeners )
    {
      SafeRunner.run( new SafeRunnable()
      {
        @Override
        public void run( ) throws Exception
        {
          listener.checkStateChanged( event );
        }
      } );
    }
  }
}
