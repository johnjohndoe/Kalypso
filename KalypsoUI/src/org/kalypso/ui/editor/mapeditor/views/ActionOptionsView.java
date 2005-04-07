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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
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
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * ActionOptionsView is a view on the selected widget of an active map view. It
 * provides a panel where the selected widgets can place GUI elements for their
 * options.
 */
public class ActionOptionsView extends ViewPart implements IWindowListener, IPageListener,
    IPartListener, IWidgetChangeListener
{
  private Composite m_topLevel;

  private Group m_group;

  private IWidget m_activeWidget;

  /*
   * 
   * @author doemming
   */
  public ActionOptionsView()
  {
    super();
    // register as windowlistener at workbench
    IWorkbench workbench = PlatformUI.getWorkbench();
    workbench.addWindowListener( this );
    IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    // initial settings
    windowOpened( activeWorkbenchWindow );
    IWorkbenchPage[] pages = activeWorkbenchWindow.getPages();
    for( int i = 0; i < pages.length; i++ )
    {
      IWorkbenchPage workbenchPage = pages[i];
      pageOpened( workbenchPage );
      IEditorPart activeEditor = workbenchPage.getActiveEditor();
      if( activeEditor != null )
        partActivated( activeEditor );
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    m_topLevel = new Composite( parent, SWT.NONE );

    GridLayout gridLayout = new GridLayout();
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    m_group = new Group( m_topLevel, SWT.NONE );
    gridLayout = new GridLayout();
    m_group.setLayout( gridLayout );
    m_group.setText( "Optionen" );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // nothing
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    if( m_topLevel != null && !m_topLevel.isDisposed() )
      m_topLevel.dispose();
    IWorkbench workbench = PlatformUI.getWorkbench();
    workbench.removeWindowListener( this );
    super.dispose();
  }

  private boolean isMapEditor( IWorkbenchPart part )
  {
    return ( part instanceof GisMapEditor );
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowOpened(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowOpened( IWorkbenchWindow window )
  {
    window.addPageListener( this );
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowClosed(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowClosed( IWorkbenchWindow window )
  {
    window.removePageListener( this );
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
  //    
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageOpened(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageOpened( IWorkbenchPage page )
  {
    page.addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageClosed(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageClosed( IWorkbenchPage page )
  {
    page.removePartListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partActivated( IWorkbenchPart part )
  {
    if( isMapEditor( part ) )
    {
      GisMapEditor editor = (GisMapEditor)part;
      editor.getMapPanel().getWidgetManager().add( this );
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partDeactivated( IWorkbenchPart part )
  {
    if( isMapEditor( part ) )
    {
      GisMapEditor editor = (GisMapEditor)part;
      editor.getMapPanel().getWidgetManager().remove( this );
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
   * @see org.kalypso.ogc.gml.widgets.IWidgetChangeListener#widgetChanged(org.kalypso.ogc.gml.widgets.IWidget)
   */
  public void widgetChanged( IWidget newWidget )
  {
    if( m_activeWidget == newWidget )
      return;
    // dispose old
    if( m_activeWidget == null && m_activeWidget instanceof IWidgetWithOptions )
    {
      IWidgetWithOptions widget = (IWidgetWithOptions)m_activeWidget;
      widget.disposeControl();
    }

    // create new Content
    m_activeWidget = newWidget;
    if( newWidget == null )
      m_group.setText( "keine Aktion selektiert" );
    else
    {
      String name = newWidget.getName();
      String tooltip = newWidget.getToolTip();
      m_group.setText( name );
      m_group.setToolTipText( tooltip );
      if( newWidget instanceof IWidgetWithOptions )
      {
        IWidgetWithOptions widget = (IWidgetWithOptions)m_activeWidget;
        widget.createControl( m_group );
      }
    }
  }
}