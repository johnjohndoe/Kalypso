/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editor.diagrameditor.actions;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Text;

/**
 * Edits the properties of a observation diagram curve.
 * 
 * @author Gernot Belger
 */
public class EditDiagCurveDialog extends TitleAreaDialog
{
  private final LineProperties m_lineProperties;

  public EditDiagCurveDialog( Shell parentShell, final LineProperties lineProperties )
  {
    super( parentShell );
    m_lineProperties = lineProperties;

    setTitle( "Kurveneigenschaften" );
    //    setMessage( "Ändern Sie die Eigenschaften einer oder mehrerer Kurven" );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( Composite parent )
  {
    Composite composite = new Composite( parent, SWT.BORDER );
    composite.setLayout( new GridLayout( 2, false ) );

    // Color
    final Label colorLabel = new Label( composite, SWT.NONE );
    colorLabel.setText( "Farbe" );

    final Button colorButton = new Button( composite, SWT.PUSH );
    colorButton.setText( "Farbe..." );

    // Name
    final Label nameLabel = new Label( composite, SWT.NONE );
    nameLabel.setText( "Name" );

    final Text nameText = new Text( composite, SWT.NONE );

    // Size
    final Label sizeLabel = new Label( composite, SWT.NONE );
    sizeLabel.setText( "Strichstärke" );

    final Slider sizeSlider = new Slider( composite, SWT.HORIZONTAL );
    sizeSlider.setValues( 1, 1, 20, 2, 1, 5 );

    // Type
    final Label typeLabel = new Label( composite, SWT.NONE );
    typeLabel.setText( " Strichart" );

    final ComboViewer typeCombo = new ComboViewer( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
    typeCombo.setLabelProvider( new LabelProvider() );
    typeCombo.setContentProvider( new ArrayContentProvider() );
    typeCombo.setInput( new String[]
    { "TODO" } );

    // hook-listeners
    nameText.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        handleNameTextModified();
      }
    } );

    colorButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        handleColorButtonSelected();
      }
    } );

    sizeSlider.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        handleSizeSliderSelected();
      }
    } );

    typeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        handleTypeSelectionChanged();
      }
    } );

    updateControl();

    return super.createDialogArea( parent );
  }

  protected void handleNameTextModified()
  {
  // TODO Auto-generated method stub

  }

  protected void handleTypeSelectionChanged()
  {
  // TODO Auto-generated method stub

  }

  protected void handleSizeSliderSelected()
  {
  // TODO Auto-generated method stub

  }

  protected void handleColorButtonSelected()
  {
  // TODO Auto-generated method stub

  }

  private void updateControl()
  {
  // fill values into controls

  // update message
  // TODO
  }

  /**
   *  
   */
  public LineProperties getLineProperties()
  {
    return m_lineProperties;
  }
}
