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
package org.kalypso.ui.editor.styleeditor.dialogs.filterpatterndialog;

import javax.swing.event.EventListenerList;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;
import org.kalypso.ui.editor.styleeditor.dialogs.filterdialog.FilterDialogEvent;
import org.kalypso.ui.editor.styleeditor.dialogs.filterdialog.FilterDialogListener;
import org.kalypso.ui.editor.styleeditor.panels.ColorBox;
import org.kalypso.ui.editor.styleeditor.panels.ColorPalettePanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;

public class FilterPatternDialog extends Dialog
{

  private EventListenerList listenerList = new EventListenerList();

  private PropertyIsBetweenOperation operation = null;

  private Color color = null;

  private Text upperBoundaryText = null;

  private Text lowerBoundaryText = null;

  public FilterPatternDialog( Shell parent, Operation m_operation )
  {
    super( parent );
    // Currently, pattern supports only PropertyIsBetweenOperation
    if( m_operation instanceof PropertyIsBetweenOperation )
      operation = (PropertyIsBetweenOperation)m_operation;
  }

  @Override
  protected void configureShell( Shell shell )
  {
    super.configureShell( shell );
    shell.setText( MessageBundle.STYLE_EDITOR_FILTER_CONFIG );
    shell.setSize( 200, 200 );
  }

  @Override
  protected void okPressed()
  {
    // check whether boundaries are set and lowerBoundary<upperBoundary
    if( lowerBoundaryText.getText() == null || lowerBoundaryText.getText().trim().length() == 0 )
    {
      new StyleEditorErrorDialog( getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
          MessageBundle.STYLE_EDITOR_PATTERN_LOWER_BOUNDARY ).showError();
      super.cancelPressed();
      return;
    }
    else if( upperBoundaryText.getText() == null || upperBoundaryText.getText().trim().length() == 0 )
    {
      new StyleEditorErrorDialog( getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
          MessageBundle.STYLE_EDITOR_PATTERN_UPPER_BOUNDARY ).showError();
      super.cancelPressed();
      return;
    }
    else if( !( isNumeric( lowerBoundaryText.getText() ) ) )
    {
      new StyleEditorErrorDialog( getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
          MessageBundle.STYLE_EDITOR_PATTERN_LOWER_ERROR ).showError();
      super.cancelPressed();
      return;
    }
    else if( !( isNumeric( upperBoundaryText.getText() ) ) )
    {
      new StyleEditorErrorDialog( getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
          MessageBundle.STYLE_EDITOR_PATTERN_UPPER_ERROR ).showError();
      super.cancelPressed();
      return;
    }
    else if( Double.parseDouble( lowerBoundaryText.getText() ) > Double.parseDouble( upperBoundaryText.getText() ) )
    {
      new StyleEditorErrorDialog( getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
          MessageBundle.STYLE_EDITOR_PATTERN_UPPER_LOWER_ERROR ).showError();
      super.cancelPressed();
      return;
    }

    ( (BoundaryExpression)operation.getLowerBoundary() ).setValue( lowerBoundaryText.getText() );
    ( (BoundaryExpression)operation.getUpperBoundary() ).setValue( upperBoundaryText.getText() );
    fire();
    super.okPressed();
  }

  @Override
  protected void cancelPressed()
  {
    fire();
    super.cancelPressed();
  }

  public void addFilterDialogListener( FilterDialogListener pl )
  {
    listenerList.add( FilterDialogListener.class, pl );
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == FilterDialogListener.class )
      {
        FilterDialogEvent event = new FilterDialogEvent( this );
        ( (FilterDialogListener)listeners[i + 1] ).filterUpdated( event );
      }
    }
  }

  @Override
  protected Control createDialogArea( Composite parent )
  {
    Composite composite = (Composite)super.createDialogArea( parent );
    composite.setSize( 200, 200 );
    composite.setLayout( new GridLayout( 1, true ) );
    composite.layout();
    applyDialogFont( composite );

    final ColorBox box = new ColorBox( composite, getColor(), ColorPalettePanel.COLOR_SIZE,
        ColorPalettePanel.COLOR_BORDER );
    final ColorDialog dialog = new ColorDialog( composite.getShell() );
    dialog.setRGB( getColor().getRGB() );
    box.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        dialog.open();
        Color newColor = new Color( null, dialog.getRGB() );
        setColor( newColor );
        box.setColor( newColor );
        fire();
      }
    } );

    Label lowerBoundaryLabel = new Label( composite, SWT.NULL );
    lowerBoundaryLabel.setText( MessageBundle.STYLE_EDITOR_LOWER_BOUNDARY );
    lowerBoundaryText = new Text( composite, SWT.BORDER );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    lowerBoundaryText.setLayoutData( textData );
    lowerBoundaryText.setText( ( (BoundaryExpression)operation.getLowerBoundary() ).getValue() );

    Label upperBoundaryLabel = new Label( composite, SWT.NULL );
    upperBoundaryLabel.setText( MessageBundle.STYLE_EDITOR_UPPER_BOUNDARY );
    upperBoundaryText = new Text( composite, SWT.BORDER );
    GridData textData2 = new GridData();
    textData2.widthHint = 90;
    textData2.heightHint = 10;
    upperBoundaryText.setLayoutData( textData2 );
    upperBoundaryText.setText( ( (BoundaryExpression)operation.getUpperBoundary() ).getValue() );
    return composite;
  }

  private boolean isNumeric( String value )
  {
    try
    {
      Double.parseDouble( value );
    }
    catch( NumberFormatException e )
    {
      return false;
    }
    return true;
  }

  public Color getColor()
  {
    return color;
  }

  public void setColor( Color m_color )
  {
    this.color = m_color;
  }

  public PropertyIsBetweenOperation getOperation()
  {
    return operation;
  }

  public void setOperation( PropertyIsBetweenOperation m_operation )
  {
    this.operation = m_operation;
  }
}