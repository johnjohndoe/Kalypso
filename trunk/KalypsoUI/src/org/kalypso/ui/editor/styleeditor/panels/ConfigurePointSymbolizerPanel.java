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
    Button addMarkButton = new Button( composite, SWT.PUSH );
    FormData addMarkButtonData = new FormData();
    addMarkButtonData.height = 18;
    addMarkButtonData.width = 18;
    addMarkButtonData.left = new FormAttachment( 200, 1000, 0 );
    addMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    addMarkButton.setLayoutData( addMarkButtonData );
    addMarkButton.setText( "+" );
    addMarkButton.setToolTipText( "Add mark" );
    addMarkButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( ADD_MARK );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Button removeMarkButton = new Button( composite, SWT.PUSH );
    if( getCanDelete() == 1 )
      removeMarkButton.setEnabled( false );
    FormData removeMarkButtonData = new FormData();
    removeMarkButtonData.height = 18;
    removeMarkButtonData.width = 18;
    removeMarkButtonData.left = new FormAttachment( 400, 1000, 0 );
    removeMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeMarkButton.setLayoutData( removeMarkButtonData );
    removeMarkButton.setText( "-" );
    removeMarkButton.setToolTipText( "Remove mark" );
    removeMarkButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( REM_MARK );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Button moveBackwardMarkButton = new Button( composite, SWT.PUSH );
    FormData moveBackwardMarkButtonData = new FormData();
    moveBackwardMarkButtonData.height = 18;
    moveBackwardMarkButtonData.width = 18;
    moveBackwardMarkButtonData.left = new FormAttachment( 600, 1000, 0 );
    moveBackwardMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardMarkButton.setLayoutData( moveBackwardMarkButtonData );
    moveBackwardMarkButton.setText( "<-" );
    moveBackwardMarkButton.setToolTipText( "Move Backward" );
    moveBackwardMarkButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( BAK_MARK );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Button moveForwardMarkButton = new Button( composite, SWT.PUSH );
    FormData moveForwardMarkButtonData = new FormData();
    moveForwardMarkButtonData.height = 18;
    moveForwardMarkButtonData.width = 18;
    moveForwardMarkButtonData.left = new FormAttachment( 800, 1000, 0 );
    moveForwardMarkButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardMarkButton.setLayoutData( moveForwardMarkButtonData );
    moveForwardMarkButton.setText( "->" );
    moveForwardMarkButton.setToolTipText( "Move Forward" );
    moveForwardMarkButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setCurrentAction( FOR_MARK );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
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