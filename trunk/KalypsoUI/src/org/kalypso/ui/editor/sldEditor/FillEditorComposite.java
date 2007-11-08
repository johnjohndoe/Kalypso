/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.editor.sldEditor;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.contribs.eclipse.swt.awt.ImageConverter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;

/**
 * @author Thomas Jung
 */
public class FillEditorComposite extends Composite
{
  private final Set<IFillModifyListener> m_listeners = new HashSet<IFillModifyListener>();

  private final Fill m_fill;

  private Color m_color;

  private Label m_colorLabel;

  private Image m_preview;

  private Composite m_previewComp;

  private final Boolean m_previewVisible;

  public FillEditorComposite( final Composite parent, final int style, final Fill fill, final Boolean previewVisible )
  {
    super( parent, style );
    m_fill = fill;
    m_previewVisible = previewVisible;

    try
    {
      createControl();
      if( m_previewVisible == true )
        updatePreview();

    }
    catch( FilterEvaluationException e )
    {
      e.printStackTrace();
    }
  }

  private void createControl( ) throws FilterEvaluationException
  {
    setLayout( new GridLayout( 2, false ) );

    createColorControl();

    createOpacityControl();

    createTypeControl();

    if( m_previewVisible == true )
      createPreviewControl();

  }

  private void createColorControl( ) throws FilterEvaluationException
  {
    /* Color */
    final Label colorTextLabel = new Label( this, SWT.NONE );
    colorTextLabel.setText( "F¸llfarbe" );

    m_colorLabel = new Label( this, SWT.BORDER );
    m_colorLabel.setText( "     " );
    GridData gridData = new GridData( SWT.END, SWT.CENTER, true, false );
    gridData.widthHint = 16;
    gridData.heightHint = 16;

    m_colorLabel.setLayoutData( gridData );

    java.awt.Color fillColor = m_fill.getFill( null );
    m_color = new Color( m_colorLabel.getDisplay(), fillColor.getRed(), fillColor.getGreen(), fillColor.getBlue() );
    m_colorLabel.setBackground( m_color );

    /* mouse listeners */
    m_colorLabel.addMouseListener( new MouseAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void mouseDown( MouseEvent e )
      {
        final ColorDialog colorDialog = new ColorDialog( FillEditorComposite.this.getShell() );
        final RGB chosenColor = colorDialog.open();
        if( chosenColor != null )
        {
          m_color.dispose();
          m_color = new Color( m_colorLabel.getDisplay(), chosenColor.red, chosenColor.green, chosenColor.blue );
          m_fill.setFill( new java.awt.Color( chosenColor.red, chosenColor.green, chosenColor.blue ) );
        }
        m_colorLabel.setBackground( m_color );
        contentChanged();
      }
    } );

    m_colorLabel.addMouseTrackListener( new MouseTrackAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseTrackAdapter#mouseEnter(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseEnter( MouseEvent e )
      {
        setCursor( new Cursor( null, SWT.CURSOR_HAND ) );
      }

      /**
       * @see org.eclipse.swt.events.MouseTrackAdapter#mouseExit(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseExit( MouseEvent e )
      {
        setCursor( new Cursor( null, SWT.CURSOR_ARROW ) );
      }
    } );
  }

  private void createTypeControl( )
  {
    /* fill type combo */
    // combo text
    final Label comboTextLabel = new Label( this, SWT.NONE );
    comboTextLabel.setText( "F¸llart" );

    final ComboViewer fillTypeCombo = new ComboViewer( this, SWT.READ_ONLY );
    GridData comboGridData = new GridData( SWT.END, SWT.CENTER, false, false );

    fillTypeCombo.getControl().setLayoutData( comboGridData );
    fillTypeCombo.setContentProvider( new ArrayContentProvider() );

    String[] types = new String[4];
    types[0] = "voll";
    types[1] = "gepunktet";
    types[2] = "gestrichelt";
    types[3] = "Strich-Punkt";
    fillTypeCombo.setInput( types );
    fillTypeCombo.setSelection( new StructuredSelection( fillTypeCombo.getElementAt( 0 ) ) );
    fillTypeCombo.getControl().setEnabled( false );

    fillTypeCombo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {

        return super.getText( element );
      }
    } );

    // selection listener
    fillTypeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {

      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object element = selection.getFirstElement();

        final String string = (String) element;

        // TODO: get the GraphicFill from a GraphicFill editor.
        // right now, there is just possible a plain fill .

        GraphicFill graphicFill = null;

        if( string == "einfarbig" )
        {
          m_fill.setGraphicFill( null );
        }
        else if( string == "gestrichelt" )
        {
        }
        else if( string == "gepunktet" )
        {
        }
        else if( string == "Strich-Punkt" )
        {
        }
        m_fill.setGraphicFill( graphicFill );

        contentChanged();
      }
    } );

    final Label addGraphicLabel = new Label( this, SWT.NONE );
    addGraphicLabel.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    addGraphicLabel.setText( "F¸llung bearbeiten" );

    final Button addGraphicButton = new Button( this, SWT.NONE );
    GridData addGraphicData = new GridData( SWT.END, SWT.CENTER, true, false );
    addGraphicData.widthHint = 20;

    addGraphicButton.setLayoutData( addGraphicData );

// final Image editImage = Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor(
// DESCRIPTORS.RESULT_VIEWER_EDIT ).createImage();
// addGraphicButton.setImage( editImage );
  }

  private void createOpacityControl( ) throws FilterEvaluationException
  {
    /* color opacity */
    // spinner text
    final Label opacityTextLabel = new Label( this, SWT.NONE );
    opacityTextLabel.setText( "Deckkraft [%]" );

    final Spinner opacitySpinner = new Spinner( this, SWT.NONE );
    opacitySpinner.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    opacitySpinner.setBackground( this.getBackground() );
    double opacity = m_fill.getOpacity( null );
    if( Double.isNaN( opacity ) || opacity > 1.0 || opacity < 0.0 )
      opacity = 1.0;
    final BigDecimal selectionValue = new BigDecimal( opacity * 100 ).setScale( 0, BigDecimal.ROUND_HALF_UP );
    opacitySpinner.setValues( selectionValue.intValue(), 1, 100, 0, 1, 10 );

    opacitySpinner.addSelectionListener( new SelectionAdapter()
    {

      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        double opac = new BigDecimal( opacitySpinner.getSelection() ).setScale( 2, BigDecimal.ROUND_HALF_UP ).divide( new BigDecimal( 100 ), BigDecimal.ROUND_HALF_UP ).doubleValue();
        m_fill.setOpacity( opac );
        contentChanged();
      }
    } );
  }

  private void createPreviewControl( )
  {
    final Group previewGroup = new Group( this, SWT.NONE );
    previewGroup.setLayout( new GridLayout() );
    GridData previewGridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    previewGridData.horizontalSpan = 2;
    previewGridData.heightHint = 30;
    previewGroup.setLayoutData( previewGridData );
    previewGroup.setText( "Vorschau" );

    /* preview */
    m_previewComp = new Composite( previewGroup, SWT.NONE );
    GridData previewCompData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    previewCompData.heightHint = 22;
    m_previewComp.setLayoutData( previewCompData );

    this.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( DisposeEvent e )
      {
        disposeControl();
      }

    } );

    m_previewComp.addControlListener( new ControlAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void controlResized( ControlEvent e )
      {
        if( m_previewVisible == true )
          updatePreview();
      }
    } );
  }

  @SuppressWarnings("static-access")
  private void updatePreview( )
  {
    Point point = m_previewComp.getSize();
    final BigDecimal width = new BigDecimal( point.x ).setScale( 0 );
    final BigDecimal height = new BigDecimal( point.y ).setScale( 0 );

    if( width.intValue() == 0 || height.intValue() == 0 )
      return;

    BufferedImage bufferedImage = new BufferedImage( width.intValue(), height.intValue(), BufferedImage.TYPE_INT_RGB );

    Graphics2D g2D = bufferedImage.createGraphics();
    g2D.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

    g2D.setPaintMode();

    g2D.setColor( java.awt.Color.WHITE );
    g2D.fillRect( 0, 0, width.intValue(), height.intValue() );

    g2D.setColor( java.awt.Color.BLACK );
    Font font = new Font( "SansSerif", Font.BOLD, height.intValue() );
    g2D.setFont( font );

    /* demo text */
    final String title = "demo";
    g2D.drawString( title, width.divide( new BigDecimal( 2 ), 0, BigDecimal.ROUND_HALF_UP ).intValue() - 30, height.divide( new BigDecimal( 1.2 ), 0, BigDecimal.ROUND_HALF_UP ).intValue() );

    FillPainter painter;

    /* fill preview painting */
    try
    {
      painter = new FillPainter( m_fill, null, null, null );
      painter.prepareGraphics( g2D );
      g2D.fillRect( 0, 0, width.intValue(), height.intValue() );
    }
    catch( FilterEvaluationException e )
    {
      e.printStackTrace();
    }

    ImageConverter converter = new ImageConverter();
    final ImageData convertToSWT = converter.convertToSWT( bufferedImage );

    Image preview = new Image( getDisplay(), convertToSWT );
    if( preview != null )
    {
      m_preview = preview;
    }
    m_previewComp.setBackgroundImage( m_preview );
  }

  protected void disposeControl( )
  {
    m_color.dispose();
    m_preview.dispose();

    m_listeners.clear();
  }

  /**
   * Add the listener to the list of listeners. If an identical listeners has already been registered, this has no
   * effect.
   */
  public void addModifyListener( final IFillModifyListener l )
  {
    m_listeners.add( l );
  }

  public void removeModifyListener( final IFillModifyListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireModified( )
  {
    final IFillModifyListener[] ls = m_listeners.toArray( new IFillModifyListener[m_listeners.size()] );
    for( final IFillModifyListener fillModifyListener : ls )
      fillModifyListener.onFillChanged( this, m_fill );
  }

  protected void contentChanged( )
  {
    if( m_previewVisible == true )
      updatePreview();
    fireModified();
  }
}
