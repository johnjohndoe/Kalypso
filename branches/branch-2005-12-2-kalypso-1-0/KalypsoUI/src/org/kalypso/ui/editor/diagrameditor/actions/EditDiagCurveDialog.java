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

import java.awt.Color;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Edits the properties of a observation diagram curve.
 * 
 * @author Gernot Belger
 */
public class EditDiagCurveDialog extends TitleAreaDialog
{
  /** Indicates the iundefined state for the name property. */
  public static final String NAME_UNDEF = EditDiagCurveDialog.class.getName();

  private String m_name;

  private Color m_color;

  private Integer m_size;

  private DashType m_dash;

  private int m_alpha;

  public EditDiagCurveDialog( Shell parentShell, final LineProperties lineProperties )
  {
    super( parentShell );

    m_name = lineProperties.getName();
    m_color = lineProperties.getColor();
    m_alpha = m_color == LineProperties.COLOR_UNDEF ? 255 : m_color.getAlpha();
    m_size = lineProperties.getSize();
    m_dash = lineProperties.getDash();
  }

  /**
   * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
   */
  protected void configureShell( Shell newShell )
  {
    super.configureShell( newShell );

    newShell.setText( "Kurveneigenschaften" );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    setTitle( "Eigenschaften editieren" );
    setMessage( "Ändern Sie die Eigenschaften einer oder mehrerer Kurven" );

    final Group composite = new Group( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 3, false ) );
    composite.setText( "Eigenschaften" );
    // Also set layoutData as it is not set by parent. Maybe fixed in 3.3?
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // Name
    final Label nameLabel = new Label( composite, SWT.NONE );
    nameLabel.setText( "Name" );

    final Text nameText = new Text( composite, SWT.BORDER );
    final GridData nameData = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    final int CONTROL_WIDTH = 150;
    nameData.widthHint = CONTROL_WIDTH;
    nameData.horizontalSpan = 2;
    nameText.setLayoutData( nameData );

    // Color
    final Label colorLabel = new Label( composite, SWT.NONE );
    colorLabel.setText( "Farbe" );

    final Button colorButton = new Button( composite, SWT.PUSH );
    final GridData colorData = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    colorData.horizontalSpan = 1;
    colorData.widthHint = CONTROL_WIDTH;
    colorButton.setLayoutData( colorData );
    colorButton.setText( "Farbe..." );

    // TODO: replace by spinner in 3.3
    final Scale alphaSlider = new Scale( composite, SWT.HORIZONTAL );
    final GridData alphaData = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    alphaData.widthHint = CONTROL_WIDTH;
    alphaData.horizontalSpan = 1;
    alphaSlider.setLayoutData( alphaData );
    alphaSlider.setIncrement( 1 );
    alphaSlider.setMinimum( 0 );
    alphaSlider.setMaximum( 255 );
    alphaSlider.setPageIncrement( 20 );

    // Size
    final Label sizeLabel = new Label( composite, SWT.NONE );
    sizeLabel.setText( "Strichstärke" );

    // TODO: replace by spinner in 3.3
    final Scale sizeSlider = new Scale( composite, SWT.HORIZONTAL );
    final GridData sizeData = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    sizeData.widthHint = CONTROL_WIDTH;
    sizeData.horizontalSpan = 1;
    sizeSlider.setLayoutData( sizeData );
    sizeSlider.setIncrement( 1 );
    sizeSlider.setMinimum( 1 );
    sizeSlider.setMaximum( 20 );
    sizeSlider.setPageIncrement( 5 );
    final Label sizeValueLabel = new Label( composite, SWT.NONE );
    sizeValueLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    // Type
    final Label typeLabel = new Label( composite, SWT.NONE );
    typeLabel.setText( "Strichart" );

    final ComboViewer dashCombo = new ComboViewer( composite, SWT.DROP_DOWN | SWT.READ_ONLY );
    dashCombo.getControl().setFont( composite.getDisplay().getSystemFont() );
    final GridData dashData = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    dashData.widthHint = CONTROL_WIDTH;
    dashCombo.getCombo().setLayoutData( dashData );
    dashCombo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      public String getText( Object element )
      {
        return ( (DashType)element ).getComboLabel();
      }
    } );
    dashCombo.setContentProvider( new ArrayContentProvider() );
    dashCombo.setInput( DashType.KNOWN_DASHS );

    // hook-listeners
    nameText.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        handleNameTextModified( nameText );
        updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );
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
        updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );
      }
    } );

    colorButton.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( DisposeEvent e )
      {
        if( colorButton.getImage() != null )
          colorButton.getImage().dispose();
      }
    } );

    alphaSlider.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        handleAlphaSliderSelected( alphaSlider );
        updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );
      }
    } );

    sizeSlider.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        handleSizeSliderSelected( sizeSlider );
        updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );

      }
    } );

    dashCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        handleTypeSelectionChanged( (IStructuredSelection)event.getSelection() );
        updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );
      }
    } );

    // Initalize controls
    if( m_name == NAME_UNDEF )
      nameText.setEditable( false );
    else
      nameText.setText( m_name );

    if( m_color == LineProperties.COLOR_UNDEF )
    {}
    else
      alphaSlider.setSelection( m_alpha );

    if( m_size != LineProperties.SIZE_UNDEF )
      sizeSlider.setSelection( m_size.intValue() );

    dashCombo.setSelection( new StructuredSelection( m_dash ) );

    updateControl( nameText, colorButton, sizeSlider, sizeValueLabel, dashCombo );

    return super.createDialogArea( parent );
  }

  protected void handleAlphaSliderSelected( Scale alphaSlider )
  {
    m_alpha = alphaSlider.getSelection();
  }

  protected void handleNameTextModified( Text nameText )
  {
    m_name = nameText.getText();
  }

  protected void handleTypeSelectionChanged( IStructuredSelection selection )
  {
    if( !selection.isEmpty() )
      m_dash = (DashType)selection.getFirstElement();
  }

  protected void handleSizeSliderSelected( final Scale sizeSlider )
  {
    m_size = new Integer( sizeSlider.getSelection() );
  }

  protected void handleColorButtonSelected()
  {
    final ColorDialog dialog = new ColorDialog( getShell() );
    final RGB rgb = dialog.open();
    if( rgb != null )
      m_color = new Color( rgb.red, rgb.green, rgb.blue );
  }

  protected void updateControl( final Text nameText, final Button colorButton, final Scale sizeSlider,
      final Label sizeValueLabel, final ComboViewer dashCombo )
  {
    final Display display = getShell().getDisplay();

    if( m_name == NAME_UNDEF )
      nameText.setBackground( display.getSystemColor( SWT.COLOR_GRAY ) );
    else
      nameText.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    if( colorButton.getImage() != null )
      colorButton.getImage().dispose();
    if( m_color == LineProperties.COLOR_UNDEF )
    {
      colorButton.setImage( null );
      colorButton.setText( "Undefiniert" );
      colorButton.setForeground( display.getSystemColor( SWT.COLOR_GRAY ) );
    }
    else
    {
      final Image colorImage = new Image( display, 32, 16 );
      GC gc = new GC( colorImage );
      org.eclipse.swt.graphics.Color buttonColor = new org.eclipse.swt.graphics.Color( display, m_color.getRed(),
          m_color.getGreen(), m_color.getBlue() );
      gc.setBackground( buttonColor );
      gc.fillRectangle( 0, 0, 32, 16 );
      buttonColor.dispose();
      gc.dispose();

      colorButton.setText( "" );
      colorButton.setImage( colorImage );

      colorButton.setForeground( display.getSystemColor( SWT.COLOR_BLACK ) );
    }

    if( m_size == LineProperties.SIZE_UNDEF )
    {
      sizeSlider.setForeground( display.getSystemColor( SWT.COLOR_GRAY ) );
      sizeValueLabel.setText( "Undefiniert" );
    }
    else
    {
      sizeSlider.setForeground( display.getSystemColor( SWT.COLOR_BLACK ) );
      sizeSlider.setSelection( m_size.intValue() );
      sizeValueLabel.setText( "" + m_size );
    }

    if( m_dash == LineProperties.DASH_UNDEF )
      dashCombo.getCombo().setBackground( display.getSystemColor( SWT.COLOR_GRAY ) );
    else
      dashCombo.getCombo().setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    propertiesChanged();
  }

  /**
   * Empty method that is called every time something changed. <br>
   * May be implemented by inheritence in order to react to changes.
   */
  protected void propertiesChanged()
  {}

  public LineProperties getLineProperties()
  {
    final Color color = m_color == LineProperties.COLOR_UNDEF ? LineProperties.COLOR_UNDEF : new Color( m_color
        .getRed(), m_color.getGreen(), m_color.getBlue(), m_alpha );

    return new LineProperties( m_name, color, m_size, m_dash );
  }
}
