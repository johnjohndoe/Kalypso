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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 *
 */
public class EditSymbolizerPanel
{

  private Composite composite = null;

  private final EventListenerList listenerList = new EventListenerList();

  public final static int REM_SYMB = 0;

  public final static int FOR_SYMB = 1;

  public final static int BAK_SYMB = 2;

  private int currentAction = -1;

  private int canDelete = -1;

  private Label moveBackwardSymbolizerButton = null;

  private Label removeSymbolizerButton = null;

  private Label moveForwardSymbolizerButton = null;

  public EditSymbolizerPanel( final Composite parent, final int m_size )
  {
    setCanDelete( m_size );
    composite = new Composite( parent, SWT.NULL );
    final FormLayout compositeLayout = new FormLayout();
    final GridData compositeData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    compositeData.horizontalSpan = ((GridLayout) parent.getLayout()).numColumns;
    compositeData.widthHint = 230;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  public void addPanelListener( final PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    removeSymbolizerButton = new Label( composite, SWT.PUSH );
    removeSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    if( getCanDelete() == 0 )
      removeSymbolizerButton.setEnabled( false );
    final FormData removeSymbolizerButtonData = new FormData();
    removeSymbolizerButtonData.height = 18;
    removeSymbolizerButtonData.width = 18;
    removeSymbolizerButtonData.left = new FormAttachment( 250, 1000, 0 );
    removeSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeSymbolizerButton.setLayoutData( removeSymbolizerButtonData );
    removeSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_REMOVE_SYMBOLIZER );
    removeSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( final MouseEvent e )
      {
        setCurrentAction( REM_SYMB );
        fire();
      }

      public void mouseDown( final MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( final MouseEvent e )
      {
      // nothing
      }

    } );

    moveBackwardSymbolizerButton = new Label( composite, SWT.PUSH );
    moveBackwardSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveBackwardSymbolizerButton.setEnabled( false );
    final FormData moveBackwardSymbolizerButtonData = new FormData();
    moveBackwardSymbolizerButtonData.height = 18;
    moveBackwardSymbolizerButtonData.width = 18;
    moveBackwardSymbolizerButtonData.left = new FormAttachment( 500, 1000, 0 );
    moveBackwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardSymbolizerButton.setLayoutData( moveBackwardSymbolizerButtonData );
    moveBackwardSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_BACKWARD );
    moveBackwardSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( final MouseEvent e )
      {
        setCurrentAction( BAK_SYMB );
        fire();
      }

      public void mouseDown( final MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( final MouseEvent e )
      {
      // nothing
      }

    } );

    moveForwardSymbolizerButton = new Label( composite, SWT.PUSH );
    moveForwardSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveForwardSymbolizerButton.setEnabled( false );
    final FormData moveForwardSymbolizerButtonData = new FormData();
    moveForwardSymbolizerButtonData.height = 18;
    moveForwardSymbolizerButtonData.width = 18;
    moveForwardSymbolizerButtonData.left = new FormAttachment( 750, 1000, 0 );
    moveForwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardSymbolizerButton.setLayoutData( moveForwardSymbolizerButtonData );
    moveForwardSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_FORWARD );
    moveForwardSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( final MouseEvent e )
      {
        setCurrentAction( FOR_SYMB );
        fire();
      }

      public void mouseDown( final MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( final MouseEvent e )
      {
      // nothing
      }

    } );
  }

  public void update( final int symbolizerNumber )
  {
    setCanDelete( symbolizerNumber );
    if( getCanDelete() == 0 )
      removeSymbolizerButton.setEnabled( false );
    else
      removeSymbolizerButton.setEnabled( true );
    if( getCanDelete() <= 1 )
      moveBackwardSymbolizerButton.setEnabled( false );
    else
      moveBackwardSymbolizerButton.setEnabled( true );
    if( getCanDelete() <= 1 )
      moveForwardSymbolizerButton.setEnabled( false );
    else
      moveForwardSymbolizerButton.setEnabled( true );
  }

  public int getAction()
  {
    return getCurrentAction();
  }

  protected void fire()
  {
    final Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        final PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[i + 1] ).valueChanged( event );
      }
    }
  }

  public int getCanDelete()
  {
    return canDelete;
  }

  public void setCanDelete( final int m_canDelete )
  {
    this.canDelete = m_canDelete;
  }

  public int getCurrentAction()
  {
    return currentAction;
  }

  public void setCurrentAction( final int m_currentAction )
  {
    this.currentAction = m_currentAction;
  }
}