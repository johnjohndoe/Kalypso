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
package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.internal.util.Util;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.EnableThemeCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class GisMapOutlineViewer implements ISelectionProvider, ModellEventListener, ICommandTarget
{
  protected CheckboxTreeViewer m_viewer;

  private final ITreeContentProvider m_contentProvider = new WorkbenchContentProvider();

  private final ILabelProvider m_labelProvider = new WorkbenchLabelProvider();

  private MapPanel m_mapPanel;

  private ICommandTarget m_commandTarget;

  public GisMapOutlineViewer( final ICommandTarget commandTarget, final MapPanel mapPanel )
  {
    setMapPanel( mapPanel );
    m_commandTarget = commandTarget;
  }

  public void dispose( )
  {
    m_contentProvider.dispose();
    m_labelProvider.dispose();
  }

  public void createControl( final Composite parent )
  {
    m_viewer = new CheckboxTreeViewer( parent );
    m_viewer.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object data = event.getElement();
        if( data instanceof IKalypsoTheme )
        {
          final IKalypsoTheme theme = (IKalypsoTheme) data;
          final IMapModell mm = theme.getMapModell();
          final ICommand command = new EnableThemeCommand( mm, theme, event.getChecked() );
          postCommand( command, null );
        }
      }
    } );

    final Tree tree = m_viewer.getTree();
    tree.addMouseMoveListener( new MouseMoveListener()
    {
      public void mouseMove( final MouseEvent e )
      {
        final TreeItem item = tree.getItem( new Point( e.x, e.y ) );
        final String text;
        if( item == null )
          text = null; // remove tooltip
        else
        {
          final Object data = item.getData();
          final ITooltipProvider ttProvider = (ITooltipProvider) Util.getAdapter( data, ITooltipProvider.class );
          text = ttProvider == null ? null : ttProvider.getTooltip( data );
        }
        tree.setToolTipText( text );
      }
    } );

    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.setLabelProvider( m_labelProvider );

    m_viewer.setInput( m_mapPanel == null ? null : m_mapPanel.getMapModell() );
    m_viewer.refresh();

    // Refresh check state
    onModellChange( null );
  }

  /**
   * @return control
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  public Control getControl( )
  {
    return m_viewer.getControl();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapPanel( final MapPanel panel )
  {
    if( m_mapPanel != null )
      m_mapPanel.removeModellListener( this );

    m_mapPanel = panel;

    final IMapModell mapModell = panel == null ? null : panel.getMapModell();

    if( m_mapPanel != null )
      m_mapPanel.addModellListener( this );

    if( m_viewer != null && !m_viewer.getControl().isDisposed() )
    {
      m_viewer.getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( m_viewer.getContentProvider() != null )
            m_viewer.setInput( mapModell );
        }
      } );
    }

    onModellChange( null );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // den Checkstate setzen!
    if( m_viewer != null )
    {
      // final IMapModell mm = getMapModell();
      // if( mm == null )
      // return;

      final StructuredViewer viewer = m_viewer;
      final Tree tt = m_viewer.getTree();
      if( tt.isDisposed() )
        return;

      tt.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( tt.isDisposed() )
            return;
          try
          {
            final TreeItem[] items = tt.getItems();
            processItems( items );

            // und die ganze view refreshen!
            if( !viewer.getControl().isDisposed() )
              viewer.refresh();
          }
          catch( final RuntimeException e )
          {
            e.printStackTrace();
          }
        }

        private void processItems( final TreeItem[] items )
        {
          for( int i = 0; i < items.length; i++ )
          {
            final TreeItem item = items[i];

            if( !item.isDisposed() )
            {
              final Object data = item.getData();
              if( data instanceof IKalypsoTheme )
              {
                final IKalypsoTheme theme = (IKalypsoTheme) data;
                final IMapModell mm = theme.getMapModell();
                item.setChecked( mm.isThemeEnabled( theme ) );
              }
              processItems( item.getItems() );
            }
          }
        }
      } );
    }
  }

  /**
   * @return object
   * @see org.eclipse.jface.viewers.IInputProvider#getInput()
   */
  public Object getInput( )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_viewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection( )
  {
    if( m_viewer == null )
      return null;

    return m_viewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_viewer.setSelection( selection );
  }

  public IStructuredContentProvider getContentProvider( )
  {
    return (IStructuredContentProvider) m_viewer.getContentProvider();
  }

  /**
   * Adds a listener for double-clicks in this viewer. Has no effect if an identical listener is already registered.
   * 
   * @param listener
   *            a double-click listener
   */
  public void addDoubleClickListener( final IDoubleClickListener listener )
  {
    m_viewer.addDoubleClickListener( listener );
  }

  /**
   * Removes the given double-click listener from this viewer. Has no affect if an identical listener is not registered.
   * 
   * @param listener
   *            a double-click listener
   */
  public void removeDoubleClickListener( final IDoubleClickListener listener )
  {
    if( m_viewer != null )
      m_viewer.removeDoubleClickListener( listener );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    if( m_commandTarget != null )
    {
      m_commandTarget.postCommand( command, runnable );
    }
  }

  public void setCommandTarget( JobExclusiveCommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
  }

}