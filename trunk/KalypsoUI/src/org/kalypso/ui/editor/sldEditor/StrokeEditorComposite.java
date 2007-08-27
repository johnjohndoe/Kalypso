/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.math.BigDecimal;
import java.util.HashMap;

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
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.contribs.eclipse.swt.awt.ImageConverter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;

/**
 * @author Thomas Jung
 */
public class StrokeEditorComposite extends Composite
{
  private final Stroke m_stroke;

  private Color m_color;

  private Label m_colorLabel;

  private Image m_preview;

  private Composite m_previewComp;

  public StrokeEditorComposite( final Composite parent, final int style, final Stroke stroke )
  {
    super( parent, style );

    m_stroke = stroke;

    try
    {
      createControl();
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

    /* Color */
    final Label colorTextLabel = new Label( this, SWT.NONE );
    colorTextLabel.setText( "Linienfarbe" );

    m_colorLabel = new Label( this, SWT.BORDER );
    m_colorLabel.setText( "     " );
    GridData gridData = new GridData( SWT.END, SWT.CENTER, true, false );
    gridData.widthHint = 16;
    gridData.heightHint = 16;

    m_colorLabel.setLayoutData( gridData );

    java.awt.Color strokeColor;

    strokeColor = m_stroke.getStroke( null );
    m_color = new Color( m_colorLabel.getDisplay(), strokeColor.getRed(), strokeColor.getGreen(), strokeColor.getBlue() );

    m_colorLabel.setBackground( m_color );

    /* mouse listeners */
    m_colorLabel.addMouseListener( new MouseAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void mouseDown( MouseEvent e )
      {
        final ColorDialog colorDialog = new ColorDialog( StrokeEditorComposite.this.getShell() );
        final RGB chosenColor = colorDialog.open();
        if( chosenColor != null )
        {
          m_color.dispose();
          m_color = new Color( m_colorLabel.getDisplay(), chosenColor.red, chosenColor.green, chosenColor.blue );
          m_stroke.setStroke( new java.awt.Color( chosenColor.red, chosenColor.green, chosenColor.blue ) );
        }
        m_colorLabel.setBackground( m_color );
        updatePreview();
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

    /* color width */
    // spinner text
    final Label widthTextLabel = new Label( this, SWT.NONE );
    widthTextLabel.setText( "Linienstärke" );

    final Spinner widthSpinner = new Spinner( this, SWT.NONE );
    widthSpinner.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    widthSpinner.setBackground( this.getBackground() );
    double width = m_stroke.getWidth( null );
    BigDecimal selectionValue = new BigDecimal( width ).setScale( 0, BigDecimal.ROUND_CEILING );
    widthSpinner.setValues( selectionValue.intValue(), 1, 99, 0, 1, 10 );

    widthSpinner.addSelectionListener( new SelectionAdapter()
    {

      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        m_stroke.setWidth( widthSpinner.getSelection() );
        updatePreview();
      }
    } );

    /* line type combo */
    // combo text
    final Label comboTextLabel = new Label( this, SWT.NONE );
    comboTextLabel.setText( "Strichart" );

    final ComboViewer lineTypeCombo = new ComboViewer( this, SWT.READ_ONLY );
    GridData comboGridData = new GridData( SWT.END, SWT.CENTER, false, false );

    lineTypeCombo.getControl().setLayoutData( comboGridData );
    lineTypeCombo.setContentProvider( new ArrayContentProvider() );

    String[] types = new String[4];
    types[0] = "durchgezogen";
    types[1] = "gepunktet";
    types[2] = "gestrichelt";
    types[3] = "Strich-Punkt";
    lineTypeCombo.setInput( types );
    lineTypeCombo.setSelection( new StructuredSelection( lineTypeCombo.getElementAt( 0 ) ) );

    lineTypeCombo.setLabelProvider( new LabelProvider()
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
    lineTypeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {

      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object element = selection.getFirstElement();

        String string = (String) element;

        if( string == "durchgezogen" )
        {
          float[] dashArray = new float[2];
          dashArray[0] = 1;
          dashArray[1] = 0;
          m_stroke.setDashArray( dashArray );
        }
        else if( string == "gestrichelt" )
        {
          float[] dashArray = new float[2];
          dashArray[0] = 10f;
          dashArray[1] = 5f;
          m_stroke.setDashArray( dashArray );
        }
        else if( string == "gepunktet" )
        {
          float[] dashArray = new float[2];
          dashArray[0] = 2f;
          dashArray[1] = 1.9f;
          m_stroke.setDashArray( dashArray );
        }
        else if( string == "Strich-Punkt" )
        {
          float[] dashArray = new float[4];
          dashArray[0] = 10f;
          dashArray[1] = 5f;
          dashArray[2] = 2f;
          dashArray[3] = 1.9f;
          m_stroke.setDashArray( dashArray );
        }
        updatePreview();
      }
    } );

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
      @Override
      public void controlResized( ControlEvent e )
      {
        updatePreview();
      }
    } );

  }

  private void updatePreview( )
  {
    Point point = m_previewComp.getSize();
    final BigDecimal width = new BigDecimal( point.x ).setScale( 0 );
    final BigDecimal height = new BigDecimal( point.y ).setScale( 0 );

    if( width.intValue() == 0 || height.intValue() == 0 )
      return;

    BufferedImage bufferedImage = new BufferedImage( width.intValue(), height.intValue(), BufferedImage.TYPE_INT_RGB );

    Graphics2D graphics2D = bufferedImage.createGraphics();
    graphics2D.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    graphics2D.fillRect( 0, 0, width.intValue(), height.intValue() );
    StrokePainter painter;
    try
    {
      // PreviewPainting
      painter = new StrokePainter( m_stroke, null, null, null );
      int[][] pos = new int[3][4];
      pos[0][0] = 4;
      pos[1][0] = 4;
      pos[0][1] = width.divide( new BigDecimal( 3 ), 0, BigDecimal.ROUND_CEILING ).intValue();
      pos[1][1] = height.divide( new BigDecimal( 1 ), 0, BigDecimal.ROUND_CEILING ).intValue() - 4;
      pos[0][2] = width.divide( new BigDecimal( 1.5 ), 0, BigDecimal.ROUND_CEILING ).intValue();
      pos[1][2] = 4;
      pos[0][3] = width.divide( new BigDecimal( 1 ), 0, BigDecimal.ROUND_CEILING ).intValue() - 4;
      pos[1][3] = height.divide( new BigDecimal( 1 ), 0, BigDecimal.ROUND_CEILING ).intValue() - 4;
      pos[2][0] = 4;

      painter.paintPoses( graphics2D, pos );
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
  }

  public static void main( String[] args )
  {
    Display display = new Display();
    Shell shell = new Shell( display );

    final Stroke stroke = new Stroke_Impl( new HashMap<Object, Object>(), null, null );
    stroke.setWidth( 3.45 );
    stroke.setStroke( new java.awt.Color( shell.getBackground().getRed(), shell.getBackground().getRed(), shell.getBackground().getBlue() ) );

    shell.setLayout( new GridLayout() );

    final Group group = new Group( shell, SWT.BORDER );
    group.setLayout( new FillLayout() );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( "Stroke" );
    StrokeEditorComposite strokeEditorComposite = new StrokeEditorComposite( group, SWT.NONE, stroke );

    shell.pack();
    shell.open();
    while( !shell.isDisposed() )
    {
      if( !display.readAndDispatch() )
        display.sleep();
    }
    display.dispose();
  }
}
