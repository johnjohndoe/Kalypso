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

import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.custom.TableTreeItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.EnableThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author belger
 */
public class GisMapOutlineViewer implements ISelectionProvider, IMapModellView, SelectionListener, ICommandTarget
{
  protected StructuredViewer m_viewer;

  private final MapModellTreeContentProvider m_contentProvider = new MapModellTreeContentProvider();

  private final MapModellLabelProvider m_labelProvider = new MapModellLabelProvider();

  private IMapModell m_mapModel;

  private final ICommandTarget m_commandTarget;

  public GisMapOutlineViewer( final ICommandTarget commandTarget, final IMapModell mapModel )
  {
    setMapModell( mapModel );
    m_commandTarget = commandTarget;
  }

  public void dispose( )
  {
    m_contentProvider.dispose();
    m_labelProvider.dispose();
  }

  public void createControl( final Composite parent )
  {
    final TableTree tree = new TableTree( parent, SWT.SINGLE | SWT.CHECK );
    tree.addSelectionListener( this );

    m_viewer = new TableTreeViewer( tree );
    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.setLabelProvider( m_labelProvider );

    m_viewer.setInput( m_mapModel );
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
  public IMapModell getMapModell( )
  {
    return m_mapModel;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    if( m_mapModel != null )
      m_mapModel.removeModellListener( this );

    m_mapModel = modell;

    m_labelProvider.setMapModell( modell );

    if( m_mapModel != null )
      m_mapModel.addModellListener( this );

    if( m_viewer != null && !m_viewer.getControl().isDisposed() )
    {
      m_viewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          if( m_viewer.getContentProvider() != null )
            m_viewer.setInput( modell );
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
      final IMapModell mm = getMapModell();
      if( mm == null )
        return;

      final StructuredViewer viewer = m_viewer;
      final TableTree tt = (TableTree) m_viewer.getControl();
      if( tt.isDisposed() )
        return;

      final TableTreeItem[] items = tt.getItems();

      tt.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          try
          {
            for( int i = 0; i < items.length; i++ )
            {
              final TableTreeItem item = items[i];

              if( !item.isDisposed() )
                item.setChecked( mm.isThemeEnabled( (IKalypsoTheme) item.getData() ) );
            }

            // und die ganze view refreshen!
            if( !viewer.getControl().isDisposed() )
              viewer.refresh();
          }
          catch( final RuntimeException e )
          {
            e.printStackTrace();
          }
        }
      } );
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    final TableTreeItem ti = (TableTreeItem) e.item;
    final Object data = ti.getData();
    // if( data instanceof IKalypsoTheme )
    // {
    // if( m_mapModel.getActiveTheme() != (IKalypsoTheme)data )
    // {
    // m_commandTarget.postCommand( new ActivateThemeCommand( m_mapModel, (IKalypsoTheme)data ),
    // null );
    // m_mapModel.activateTheme( (IKalypsoTheme)data );
    // // todo: maybe create MultiCommand (eg. activate and enable )
    // }
    // }

    if( (e.detail & SWT.CHECK) != 0 )
    {
      if( data instanceof IKalypsoTheme )
      {
        final ICommand command = new EnableThemeCommand( m_mapModel, (IKalypsoTheme) data, ti.getChecked() );
        m_commandTarget.postCommand( command, null );
      }
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    //
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
  public void addSelectionChangedListener( ISelectionChangedListener listener )
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
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
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
   *          a double-click listener
   */
  public void addDoubleClickListener( IDoubleClickListener listener )
  {
    m_viewer.addDoubleClickListener( listener );
  }

  /**
   * Removes the given double-click listener from this viewer. Has no affect if an identical listener is not registered.
   * 
   * @param listener
   *          a double-click listener
   */
  public void removeDoubleClickListener( IDoubleClickListener listener )
  {
    m_viewer.removeDoubleClickListener( listener );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

}