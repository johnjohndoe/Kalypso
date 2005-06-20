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
public class ControlRulePanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  public final static int ADD_RULE = 0;

  public final static int REM_RULE = 1;

  public final static int FOR_RULE = 2;

  public final static int BAK_RULE = 3;

  public final static int ADD_PATTERN_RULE = 4;

  private String labelText = null;

  private int currentAction = -1;

  private int canDelete = -1;

  private int possibleNumericFeatureTypeNumber = 0;

  public ControlRulePanel( Composite parent, String m_labelText, int m_size, int m_possibleNumericFeatureTypeNumber )
  {
    setCanDelete( m_size );
    setPossibleNumericFeatureTypeNumber( m_possibleNumericFeatureTypeNumber );
    setLabelText( m_labelText );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 240;
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
    Label label = new Label( composite, SWT.NULL );
    FormData labelData = new FormData();
    labelData.height = 15;
    labelData.width = 40;
    labelData.top = new FormAttachment( 100, 1000, 0 );
    labelData.left = new FormAttachment( 10, 1000, 0 );
    label.setLayoutData( labelData );
    label.setText( labelText );

    Label addRuleButton = new Label( composite, SWT.PUSH );
    addRuleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    FormData addRuleButtonData = new FormData();
    addRuleButtonData.height = 18;
    addRuleButtonData.width = 18;
    addRuleButtonData.left = new FormAttachment( 300, 1000, 0 );
    addRuleButtonData.top = new FormAttachment( 100, 1000, 0 );
    addRuleButton.setLayoutData( addRuleButtonData );
    addRuleButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD_RULE );
    addRuleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( ADD_RULE );
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

    Label addPatternRuleButton = new Label( composite, SWT.PUSH );
    addPatternRuleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE_PATTERN.createImage() );
    FormData addPatternRuleButtonData = new FormData();
    addPatternRuleButtonData.height = 18;
    addPatternRuleButtonData.width = 18;
    addPatternRuleButtonData.left = new FormAttachment( 400, 1000, 0 );
    addPatternRuleButtonData.top = new FormAttachment( 100, 1000, 0 );
    addPatternRuleButton.setLayoutData( addPatternRuleButtonData );
    if( getPossibleNumericFeatureTypeNumber() == 0 )
      addPatternRuleButton.setVisible( false );
    addPatternRuleButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD_RULE_PATTERN );
    addPatternRuleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( ADD_PATTERN_RULE );
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

    Label removeRuleButton = new Label( composite, SWT.PUSH );
    removeRuleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    if( getCanDelete() == 0 )
      removeRuleButton.setVisible( false );
    FormData removeRuleButtonData = new FormData();
    removeRuleButtonData.height = 18;
    removeRuleButtonData.width = 18;
    removeRuleButtonData.left = new FormAttachment( 500, 1000, 0 );
    removeRuleButtonData.top = new FormAttachment( 100, 1000, 0 );
    removeRuleButton.setLayoutData( removeRuleButtonData );
    removeRuleButton.setToolTipText( MessageBundle.STYLE_EDITOR_REMOVE_RULE );
    removeRuleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( REM_RULE );
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

    Label moveBackwardRuleButton = new Label( composite, SWT.PUSH );
    moveBackwardRuleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_BACKWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveBackwardRuleButton.setVisible( false );

    FormData moveBackwardRuleButtonData = new FormData();
    moveBackwardRuleButtonData.height = 18;
    moveBackwardRuleButtonData.width = 18;
    moveBackwardRuleButtonData.left = new FormAttachment( 750, 1000, 0 );
    moveBackwardRuleButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveBackwardRuleButton.setLayoutData( moveBackwardRuleButtonData );
    moveBackwardRuleButton.setToolTipText( MessageBundle.STYLE_EDITOR_BACKWARD );
    moveBackwardRuleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( BAK_RULE );
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

    Label moveForwardRuleButton = new Label( composite, SWT.PUSH );
    moveForwardRuleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_FORWARD.createImage() );
    if( getCanDelete() <= 1 )
      moveForwardRuleButton.setVisible( false );
    FormData moveForwardRuleButtonData = new FormData();
    moveForwardRuleButtonData.height = 18;
    moveForwardRuleButtonData.width = 18;
    moveForwardRuleButtonData.left = new FormAttachment( 850, 1000, 0 );
    moveForwardRuleButtonData.top = new FormAttachment( 100, 1000, 0 );
    moveForwardRuleButton.setLayoutData( moveForwardRuleButtonData );
    moveForwardRuleButton.setToolTipText( MessageBundle.STYLE_EDITOR_FORWARD );
    moveForwardRuleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setCurrentAction( FOR_RULE );
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

  public String getLabelText()
  {
    return labelText;
  }

  public void setLabelText( String m_labelText )
  {
    this.labelText = m_labelText;
  }

  public int getPossibleNumericFeatureTypeNumber()
  {
    return possibleNumericFeatureTypeNumber;
  }

  public void setPossibleNumericFeatureTypeNumber( int m_possibleNumericFeatureTypeNumber )
  {
    this.possibleNumericFeatureTypeNumber = m_possibleNumericFeatureTypeNumber;
  }
}