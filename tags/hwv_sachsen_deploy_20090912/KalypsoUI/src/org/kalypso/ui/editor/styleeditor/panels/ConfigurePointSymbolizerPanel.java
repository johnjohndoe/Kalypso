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
/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 *  
 */
public class ConfigurePointSymbolizerPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  public final static int ADD_MARK = 0;

  public final static int REM_MARK = 1;

  public final static int FOR_MARK = 2;

  public final static int BAK_MARK = 3;

  private int currentAction = -1;

  private int canDelete = -1;

  public ConfigurePointSymbolizerPanel( Composite parent, int m_size )
  {
    setCanDelete( m_size );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 195;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    Label addMarkButton = new Label( composite, SWT.PUSH );
    addMarkButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    FormData addMarkButtonData = new FormData();
    addMarkButtonData.height = 18;
    addMarkButtonData.width = 18;
    addMarkButtonData.left = new FormAttachment( 200, 1000, 0 );
    addMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    addMarkButton.setLayoutData( addMarkButtonData );
    addMarkButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD_MARK );
    addMarkButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( ADD_MARK );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
      }

    } );

    Label removeMarkButton = new Label( composite, SWT.PUSH );
    removeMarkButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    if( getCanDelete() == 1 )
      removeMarkButton.setVisible( false );
    FormData removeMarkButtonData = new FormData();
    removeMarkButtonData.height = 18;
    removeMarkButtonData.width = 18;
    removeMarkButtonData.left = new FormAttachment( 400, 1000, 0 );
    removeMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeMarkButton.setLayoutData( removeMarkButtonData );
    removeMarkButton.setToolTipText( MessageBundle.STYLE_EDITOR_REMOVE_MARK );
    removeMarkButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( REM_MARK );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
      }

    } );

    Label moveBackwardMarkButton = new Label( composite, SWT.PUSH );
    moveBackwardMarkButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    FormData moveBackwardMarkButtonData = new FormData();
    moveBackwardMarkButtonData.height = 18;
    moveBackwardMarkButtonData.width = 18;
    moveBackwardMarkButtonData.left = new FormAttachment( 600, 1000, 0 );
    moveBackwardMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardMarkButton.setLayoutData( moveBackwardMarkButtonData );
    moveBackwardMarkButton.setToolTipText( MessageBundle.STYLE_EDITOR_BACKWARD );
    moveBackwardMarkButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( BAK_MARK );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
      }

    } );

    Label moveForwardMarkButton = new Label( composite, SWT.PUSH );
    moveForwardMarkButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    FormData moveForwardMarkButtonData = new FormData();
    moveForwardMarkButtonData.height = 18;
    moveForwardMarkButtonData.width = 18;
    moveForwardMarkButtonData.left = new FormAttachment( 800, 1000, 0 );
    moveForwardMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardMarkButton.setLayoutData( moveForwardMarkButtonData );
    moveForwardMarkButton.setToolTipText( MessageBundle.STYLE_EDITOR_FORWARD );
    moveForwardMarkButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( FOR_MARK );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
      }

    } );
  }

  public int getAction()
  {
    return getCurrentAction();
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[i + 1] ).valueChanged( event );
      }
    }
  }

  public int getCanDelete()
  {
    return canDelete;
  }

  public void setCanDelete( int m_canDelete )
  {
    this.canDelete = m_canDelete;
  }

  public int getCurrentAction()
  {
    return currentAction;
  }

  public void setCurrentAction( int m_currentAction )
  {
    this.currentAction = m_currentAction;
  }
}