/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

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

  private Button moveBackwardSymbolizerButton = null;

  private Button removeSymbolizerButton = null;

  private Button moveForwardSymbolizerButton = null;

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
    removeSymbolizerButton = new Button( composite, SWT.PUSH );
    if( getCanDelete() == 0 )
      removeSymbolizerButton.setEnabled( false );
    FormData removeSymbolizerButtonData = new FormData();
    removeSymbolizerButtonData.height = 18;
    removeSymbolizerButtonData.width = 18;
    removeSymbolizerButtonData.left = new FormAttachment( 250, 1000, 0 );
    removeSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeSymbolizerButton.setLayoutData( removeSymbolizerButtonData );
    removeSymbolizerButton.setText( "-" );
    removeSymbolizerButton.setToolTipText( "Remove Symbolizer" );
    removeSymbolizerButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( REM_SYMB );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    moveBackwardSymbolizerButton = new Button( composite, SWT.PUSH );
    if( getCanDelete() <= 1 )
      moveBackwardSymbolizerButton.setEnabled( false );
    FormData moveBackwardSymbolizerButtonData = new FormData();
    moveBackwardSymbolizerButtonData.height = 18;
    moveBackwardSymbolizerButtonData.width = 18;
    moveBackwardSymbolizerButtonData.left = new FormAttachment( 500, 1000, 0 );
    moveBackwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardSymbolizerButton.setLayoutData( moveBackwardSymbolizerButtonData );
    moveBackwardSymbolizerButton.setText( "<-" );
    moveBackwardSymbolizerButton.setToolTipText( "Move Backward" );
    moveBackwardSymbolizerButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( BAK_SYMB );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    moveForwardSymbolizerButton = new Button( composite, SWT.PUSH );
    if( getCanDelete() <= 1 )
      moveForwardSymbolizerButton.setEnabled( false );
    FormData moveForwardSymbolizerButtonData = new FormData();
    moveForwardSymbolizerButtonData.height = 18;
    moveForwardSymbolizerButtonData.width = 18;
    moveForwardSymbolizerButtonData.left = new FormAttachment( 750, 1000, 0 );
    moveForwardSymbolizerButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardSymbolizerButton.setLayoutData( moveForwardSymbolizerButtonData );
    moveForwardSymbolizerButton.setText( "->" );
    moveForwardSymbolizerButton.setToolTipText( "Move Forward" );
    moveForwardSymbolizerButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( FOR_SYMB );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
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