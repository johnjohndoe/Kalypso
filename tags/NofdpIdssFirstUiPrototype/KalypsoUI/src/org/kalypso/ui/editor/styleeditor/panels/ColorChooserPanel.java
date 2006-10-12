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
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * @author F.Lindemann
 *  
 */
public class ColorChooserPanel
{

  private Composite composite = null;

  private Color color = new Color( null, 255, 80, 80 );

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  public ColorChooserPanel( Composite parent, String m_label, java.awt.Color m_color )
  {
    setLabel( m_label );
    if( m_color != null )
      setColor( new Color( null, m_color.getRed(), m_color.getGreen(), m_color.getBlue() ) );
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
    init();
  }

  public void addColorChooserListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    // Color Image
    final Label fillColorImageInner = new Label( composite, SWT.NULL );
    FormData fillColorImageInnerData = new FormData();
    fillColorImageInnerData.height = 10;
    fillColorImageInnerData.width = 10;
    fillColorImageInnerData.left = new FormAttachment( 350, 1000, 0 );
    fillColorImageInnerData.top = new FormAttachment( 220, 1000, 0 );
    fillColorImageInner.setLayoutData( fillColorImageInnerData );
    fillColorImageInner.setBackground( color );

    Label fillColorImageOuter = new Label( composite, SWT.NULL );
    FormData fillColorImageOuterData = new FormData();
    fillColorImageOuterData.height = 14;
    fillColorImageOuterData.width = 14;
    fillColorImageOuterData.left = new FormAttachment( 340, 1000, 0 );
    fillColorImageOuterData.top = new FormAttachment( 120, 1000, 0 );
    fillColorImageOuter.setLayoutData( fillColorImageOuterData );
    fillColorImageOuter.setBackground( new Color( null, 0, 0, 0 ) );

    Button colorChooserButton = new Button( composite, SWT.PUSH );
    FormData colorChooserButtonData = new FormData();
    colorChooserButtonData.height = 15;
    colorChooserButtonData.width = 22;
    colorChooserButtonData.left = new FormAttachment( 540, 1000, 0 );
    colorChooserButtonData.top = new FormAttachment( 100, 1000, 0 );
    colorChooserButton.setLayoutData( colorChooserButtonData );
    colorChooserButton.setText( "..." );

    final ColorDialog dialog = new ColorDialog( composite.getShell() );
    dialog.setRGB( color.getRGB() );
    colorChooserButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        dialog.open();
        setColor( new Color( null, dialog.getRGB() ) );
        fillColorImageInner.setBackground( getColor() );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Label fillColorLabel = new Label( composite, SWT.NULL );
    FormData fillColorLabelLData = new FormData();
    fillColorLabelLData.height = 15;
    fillColorLabelLData.width = 242;
    fillColorLabelLData.left = new FormAttachment( 0, 1000, 0 );
    fillColorLabelLData.top = new FormAttachment( 100, 1000, 0 );
    fillColorLabel.setLayoutData( fillColorLabelLData );
    fillColorLabel.setText( label );
  }

  public Color getColor()
  {
    return color;
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

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public void setColor( Color m_color )
  {
    this.color = m_color;
  }
}