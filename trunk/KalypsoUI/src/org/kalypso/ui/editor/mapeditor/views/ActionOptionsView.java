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
package org.kalypso.ui.editor.mapeditor.views;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.IWidgetChangeListener;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * ActionOptionsView is a view on the selected widget of an active map view. It provides a panel where the selected
 * widgets can place GUI elements for their options.
 */
public class ActionOptionsView extends ViewPart implements IWindowListener, IPageListener, IPartListener, IWidgetChangeListener
{
  public static final String ID = "org.kalypso.ui.editor.mapeditor.views.ActionOptionsView";

  /**
   * top level composite of view
   */
  private Composite m_topLevel = null;

  /**
   * group that is contains always the active widget's controls
   */
  private Group m_group = null;

  /**
   * the actual active widget
   */
  private IWidget m_activeWidget = null;

  /**
   * list of registries where we are registered as listeners <br>
   * used for clean dispose
   */
  private final List<Object> m_registries = new ArrayList<Object>();

  /**
   * sets force mode for update of inner control
   */
  private boolean m_modeForce = false;

  /*
   * @author doemming TODO update view when model changes (on selected modellevents)
   */
  public ActionOptionsView( )
  {
    super();
    // register as windowlistener at workbench
    final IWorkbench workbench = PlatformUI.getWorkbench();
    workbench.addWindowListener( this );
    m_registries.add( workbench );
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    // set initial things
    windowOpened( activeWorkbenchWindow );

    final IWorkbenchPage[] pages = activeWorkbenchWindow.getPages();
    for( int i = 0; i < pages.length; i++ )
    {
      final IWorkbenchPage workbenchPage = pages[i];
      pageOpened( workbenchPage );
      final IWorkbenchPart activePart = workbenchPage.getActivePart();
      if( activePart != null )
      {
        partActivated( activePart );
        if( activePart instanceof AbstractMapPart )
        {
          final AbstractMapPart editor = (AbstractMapPart) activePart;
          m_activeWidget = editor.getMapPanel().getWidgetManager().getActualWidget();
        }
      }
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowActivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowActivated( IWorkbenchWindow window )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowDeactivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowDeactivated( IWorkbenchWindow window )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageActivated(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageActivated( IWorkbenchPage page )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowOpened(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowOpened( final IWorkbenchWindow window )
  {
    window.addPageListener( this );
    m_registries.add( window );
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowClosed(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowClosed( IWorkbenchWindow window )
  {
    window.removePageListener( this );
    m_registries.remove( window );
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageOpened(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageOpened( IWorkbenchPage page )
  {
    page.addPartListener( this );
    m_registries.add( page );
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageClosed(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageClosed( IWorkbenchPage page )
  {
    page.removePartListener( this );
    m_registries.remove( page );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partActivated( IWorkbenchPart part )
  {
    if( part instanceof AbstractMapPart )
    {
      AbstractMapPart editor = (AbstractMapPart) part;
      if( m_topLevel != null && !m_topLevel.isDisposed() )
        m_topLevel.setVisible( true );
      final WidgetManager widgetManager = editor.getMapPanel().getWidgetManager();
      widgetManager.addWidgetChangeListener( this );
      m_registries.add( widgetManager );
    }
    else if( part instanceof IEditorPart )
    {
      // if other editor than mapeditor disable view
      if( m_topLevel != null && !m_topLevel.isDisposed() )
        m_topLevel.setVisible( false );
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partDeactivated( IWorkbenchPart part )
  {
    if( part instanceof AbstractMapPart )
    {
      final AbstractMapPart editor = (AbstractMapPart) part;
      final WidgetManager widgetManager = editor.getMapPanel().getWidgetManager();
      widgetManager.removeWidgetChangeListener( this );
      m_registries.remove( widgetManager );
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart)
   */
  public void partBroughtToTop( IWorkbenchPart part )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
   */
  public void partClosed( IWorkbenchPart part )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
   */
  public void partOpened( IWorkbenchPart part )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    m_topLevel = new Composite( parent, SWT.NONE );

    Layout gridLayout = new FillLayout();
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    m_topLevel.setLayoutData( data );

    m_group = new Group( m_topLevel, SWT.NONE );
    GridData datag = new GridData();
    datag.horizontalAlignment = GridData.FILL;
    datag.verticalAlignment = GridData.FILL;
    datag.grabExcessHorizontalSpace = true;
    datag.grabExcessVerticalSpace = true;

    GridLayout layout = new GridLayout();
    m_group.setLayout( layout );
    m_group.setText( "Optionen" );
    m_group.setLayoutData( datag );
    m_topLevel.layout();
    // m_topLevel.pack();

    // update content
    m_modeForce = true;
    widgetChanged( m_activeWidget );
    m_modeForce = false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidgetChangeListener#widgetChanged(org.kalypso.ogc.gml.widgets.IWidget)
   */
  public void widgetChanged( final IWidget newWidget )
  {
    if( m_activeWidget == newWidget && !m_modeForce )
      return;

    // dispose old
    if( m_activeWidget != null && m_activeWidget instanceof IWidgetWithOptions )
    {
      final IWidgetWithOptions widget = (IWidgetWithOptions) m_activeWidget;
      widget.disposeControl();
    }

    // create new Content
    m_activeWidget = newWidget;
    if( newWidget == null && m_group != null && !m_group.isDisposed() )
      m_group.setText( "keine Aktion selektiert" );
    else
    {
      final String name = newWidget.getName();
      final String tooltip = newWidget.getToolTip();
      if( m_group != null && !m_group.isDisposed() )
      {
        m_group.setText( name );
        m_group.setToolTipText( tooltip );
        if( newWidget instanceof IWidgetWithOptions )
        {
          final IWidgetWithOptions widget = (IWidgetWithOptions) m_activeWidget;
          final Control control = widget.createControl( m_group );
          control.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

          m_group.layout();
        }
      }
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    // dispose inner controls
    if( m_activeWidget != null && m_activeWidget instanceof IWidgetWithOptions )
    {
      ((IWidgetWithOptions) m_activeWidget).disposeControl();
      m_activeWidget = null;
    }
    // dispose my controls
    if( m_topLevel != null && !m_topLevel.isDisposed() )
      m_topLevel.dispose();
    // remove all listeners
    for( Iterator iter = m_registries.iterator(); iter.hasNext(); )
    {
      final Object registry = iter.next();
      if( registry instanceof WidgetManager )
        ((WidgetManager) registry).removeWidgetChangeListener( this );
      if( registry instanceof IWorkbench )
        ((IWorkbench) registry).removeWindowListener( this );
      if( registry instanceof IWorkbenchWindow )
        ((IWorkbenchWindow) registry).removePageListener( this );
      if( registry instanceof IWorkbenchPage )
        ((IWorkbenchPage) registry).removePartListener( this );
    }
    super.dispose();
  }
}