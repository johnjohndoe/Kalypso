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
public class EditSymbolizerPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  public final static int REM_SYMB = 0;

  public final static int FOR_SYMB = 1;

  public final static int BAK_SYMB = 2;

  private int currentAction = -1;

  private int canDelete = -1;

  private Label moveBackwardSymbolizerButton = null;

  private Label removeSymbolizerButton = null;

  private Label moveForwardSymbolizerButton = null;

  public EditSymbolizerPanel( Composite parent, int m_size )
  {
    setCanDelete( m_size );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 230;
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
    removeSymbolizerButton = new Label( composite, SWT.PUSH );
    removeSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    if( getCanDelete() == 0 )
      removeSymbolizerButton.setEnabled( false );
    FormData removeSymbolizerButtonData = new FormData();
    removeSymbolizerButtonData.height = 18;
    removeSymbolizerButtonData.width = 18;
    removeSymbolizerButtonData.left = new FormAttachment( 250, 1000, 0 );
    removeSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeSymbolizerButton.setLayoutData( removeSymbolizerButtonData );
    removeSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_REMOVE_SYMBOLIZER );
    removeSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( REM_SYMB );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );

    moveBackwardSymbolizerButton = new Label( composite, SWT.PUSH );
    moveBackwardSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveBackwardSymbolizerButton.setEnabled( false );
    FormData moveBackwardSymbolizerButtonData = new FormData();
    moveBackwardSymbolizerButtonData.height = 18;
    moveBackwardSymbolizerButtonData.width = 18;
    moveBackwardSymbolizerButtonData.left = new FormAttachment( 500, 1000, 0 );
    moveBackwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardSymbolizerButton.setLayoutData( moveBackwardSymbolizerButtonData );
    moveBackwardSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_BACKWARD );
    moveBackwardSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( BAK_SYMB );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );

    moveForwardSymbolizerButton = new Label( composite, SWT.PUSH );
    moveForwardSymbolizerButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveForwardSymbolizerButton.setEnabled( false );
    FormData moveForwardSymbolizerButtonData = new FormData();
    moveForwardSymbolizerButtonData.height = 18;
    moveForwardSymbolizerButtonData.width = 18;
    moveForwardSymbolizerButtonData.left = new FormAttachment( 750, 1000, 0 );
    moveForwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardSymbolizerButton.setLayoutData( moveForwardSymbolizerButtonData );
    moveForwardSymbolizerButton.setToolTipText( MessageBundle.STYLE_EDITOR_FORWARD );
    moveForwardSymbolizerButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( FOR_SYMB );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );
  }

  public void update( int symbolizerNumber )
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