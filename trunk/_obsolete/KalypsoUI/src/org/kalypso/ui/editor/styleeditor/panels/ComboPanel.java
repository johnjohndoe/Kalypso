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

  protected int style = SWT.NULL;

  protected int comboBox_width = 70;

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
    comboBox = new Combo( composite, style );
    FormData comboData = new FormData();
    comboData.width = comboBox_width;
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