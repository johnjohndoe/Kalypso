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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * @author F.Lindemann
 *  
 */
public abstract class ComboPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  protected Combo comboBox = null;

  protected String items[];

  protected int selection_index = -1;

  protected ComboPanel( Composite parent, String m_label )
  {
    setLabel( m_label );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 180;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  public abstract void setSelection( int index );

  public abstract int getSelection();

  protected void init()
  {
    comboBox = new Combo( composite, SWT.NULL );
    FormData comboData = new FormData();
    comboData.width = 70;
    comboData.height = 10;
    comboData.left = new FormAttachment( 340, 1000, 0 );
    comboData.top = new FormAttachment( 0, 1000, 0 );
    comboBox.setLayoutData( comboData );
    comboBox.setItems( items );

    comboBox.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        selection_index = ( (Combo)e.getSource() ).getSelectionIndex();
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Label labelPanel = new Label( composite, SWT.NULL );
    FormData labelData = new FormData();
    labelData.height = 15;
    labelData.width = 242;
    labelData.left = new FormAttachment( 0, 1000, 0 );
    labelData.top = new FormAttachment( 150, 1000, 0 );
    labelPanel.setLayoutData( labelData );
    labelPanel.setText( label );
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

  public String[] getItems()
  {
    return items;
  }

  public void setItems( String[] m_items )
  {
    this.items = m_items;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public int getSelection_index()
  {
    return selection_index;
  }

  public void setSelection_index( int m_selection_index )
  {
    this.selection_index = m_selection_index;
  }
}