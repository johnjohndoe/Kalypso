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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.awt.Color;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
// import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorControl;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.IElevationColorModel;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SimpleElevationColorModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author congo
 */
public class ColorModelChangeComponent
{
  private PainterElevationColorModel paintModel = new PainterElevationColorModel();

  private Section elevationColorSection;

  private PaintListener drawListener;

  private Canvas windowCanvas;

  private int selectedRects = 1;

  private GC gc;

  private Label minLabel;

  // private RGB rgbChoice;

  // private Color colorChoice;
  private Color colorAWTChoice;

  private FormToolkit toolkit;

  private Button checkBtnOptionMinMax;

  private Display disp;

  private Composite parent;

  private Color noColorAWTChoice;

  private IElevationColorModel colorModel;

  private double MINI_ELEVATION = 0;

  private double MAXI_ELEVATION = 0;
  


  private Color gotColor;

  private ApplyElevationWidgetDataModel dataModel;

  private ColorSelector noColorSelector;

  private Label noElevationColorLabel;

  public void createControl( ApplyElevationWidgetDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    createSelectColor( parent );
  }

  private void createSelectColor( Composite clientComposite )
  {

    
    FormLayout elevationColorGrid = new FormLayout();
    clientComposite.setLayout( elevationColorGrid );

    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    clientComposite.setLayoutData( formData );

    // Min Max Grouping
    final Group minMaxGroup = new Group( clientComposite, SWT.NULL );
    minMaxGroup.setLayoutData( formData );

    final Group optionsColorGroup = new Group( clientComposite, SWT.NULL );
    formData = new FormData();
    formData.left = new FormAttachment( minMaxGroup, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    formData.right = new FormAttachment( 100, 0 );
    optionsColorGroup.setLayoutData( formData );
    firstGroup( minMaxGroup );
    secondGroup( optionsColorGroup );
  }

  private void firstGroup( Group minMaxGroup )
  {

    FormLayout minMaxLayout = new FormLayout();
    minMaxGroup.setText( "Color Space" );
    minMaxGroup.setLayout( minMaxLayout );

    FormData formData;
    Label maxLabel = new Label( minMaxGroup, SWT.NONE );
    maxLabel.setText( "Max" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    // formData.right = new FormAttachment( MIN_MAX_WIDTH, 0 );
    maxLabel.setLayoutData( formData );
    this.disp = minMaxGroup.getDisplay();

    windowCanvas = createCanvas( minMaxGroup, new PaintListener()
    {
      public void paintControl( PaintEvent e )
      {
        gc = new GC( windowCanvas );
        paintElevationColorSelection( gc );
      }

    } );

    //windowCanvas.setBackground( minMaxGroup.getDisplay().getSystemColor( SWT.COLOR_GRAY ) );

    formData = new FormData();
    formData.width = 20;// 22;
    formData.height = 100;// 106;
    // windowCanvasFormData.top = new FormAttachment(0,1,0);
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    formData.left = new FormAttachment( maxLabel, 5 );
    windowCanvas.setLayoutData( formData );

    minLabel = new Label( minMaxGroup, SWT.BOTTOM );
    minLabel.setText( "Min" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    // formData.top = new FormAttachment(maxLabel,100);
    formData.bottom = new FormAttachment( 100, -5 );
    // formData.right = new FormAttachment( MIN_MAX_WIDTH, 0 );
    minLabel.setLayoutData( formData );
  }

  private void paintElevationColorSelection( GC graphicCanvas )
  {
    int coord = (((int) (Math.ceil( 100D / selectedRects ))));
    colorAWTChoice = ElevationColorControl.getBaseColor();
    System.out.println(colorAWTChoice);
    
    float[] colorsHSB = colorAWTChoice.RGBtoHSB( colorAWTChoice.getRed(), colorAWTChoice.getGreen(), colorAWTChoice.getBlue(), null );
    
    MAXI_ELEVATION = dataModel.getElevationModel().getMaxElevation();
    MINI_ELEVATION = dataModel.getElevationModel().getMinElevation();
    System.out.println(MAXI_ELEVATION+","+MINI_ELEVATION);
    
    float brightnessCount = colorsHSB[0];//2
    double selectElevation = MINI_ELEVATION;
    double part1;
    float[] ex = {23,20,18,16,14,11,9,7,4,2};

    if( !checkBtnOptionMinMax.getSelection() )
    {
      colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );

    //  System.out.println( "NoColorElevation : " + ElevationColorControl.getNoElevationColor() + ", ColorAWTChoice :" + colorAWTChoice );

      part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
      selectElevation = MINI_ELEVATION; 
      

      for( int i = 0; i < selectedRects; i++ )
      {
        System.out.println("1 :"+selectElevation);
        if( colorAWTChoice != null )
        {
          gotColor = colorModel.getColor( selectElevation );
          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue())) ) );
          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
        }
        else
        {
          System.out.println( "Out of Range" );
        }
          selectElevation = selectElevation + part1;
      }  
      
    }
    else
    {
      colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION,MAXI_ELEVATION);
      part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
      selectElevation = (float) MINI_ELEVATION;     
      
      for( int i = selectedRects - 1; i >= 0; i-- )
      {
        System.out.println("2 : "+selectElevation);
        
        if( colorAWTChoice != null )
        {
          gotColor = colorModel.getColor( selectElevation );
          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
        }
        else
        {
          System.out.println( "Out Of Range" );
        }
        selectElevation = selectElevation + part1;
      }      
    }
  }

  private void secondGroup( Group optionsColorGroup )
  {
    FormData optionsColorFormData;
    FormLayout optionsColorGrpLayout = new FormLayout();
    optionsColorGroup.setText( "Further Options" );
    optionsColorGroup.setLayout( optionsColorGrpLayout );

    Label maximumColor = new Label( optionsColorGroup, SWT.FLAT );
    maximumColor.setText( "Choose Color" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    // formData.right = new FormAttachment()
    maximumColor.setLayoutData( optionsColorFormData );

    final ColorSelector colorSelector = new ColorSelector( optionsColorGroup );

    Button maxColorBtn = colorSelector.getButton();// new Button(optionsColorGroup,SWT.None);

    maxColorBtn.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {

        colorAWTChoice = new Color( colorSelector.getColorValue().red, colorSelector.getColorValue().green, colorSelector.getColorValue().blue );
        ElevationColorControl.setBaseColor( colorAWTChoice );
        // System.out.println(colorAWTChoice+"'s choice - RGB :" +colorAWTChoice.getRed()+","
        // +colorAWTChoice.getGreen()+"," +colorAWTChoice.getBlue());
        // paintModel.setBaseColor( colorAWTChoice );
        windowCanvas.redraw();
      }
    } );
    colorSelector.setColorValue( new RGB(ElevationColorControl.getBaseColor().getRed(),
        ElevationColorControl.getBaseColor().getGreen(),
        ElevationColorControl.getBaseColor().getBlue()));    
    maxColorBtn.setText( "SELECT" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment(maximumColor, 28 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    maxColorBtn.setLayoutData( optionsColorFormData );
    
    noElevationColorLabel = new Label( optionsColorGroup, SWT.NONE );
    noElevationColorLabel.setText( "No Elevation Color" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorLabel.setLayoutData( optionsColorFormData );

    noColorSelector = new ColorSelector( optionsColorGroup );
    Button noElevationColorBtn = noColorSelector.getButton();
    noElevationColorBtn.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        noColorAWTChoice = new Color( noColorSelector.getColorValue().red, noColorSelector.getColorValue().green, noColorSelector.getColorValue().blue );
        ElevationColorControl.setNoElevationColor( noColorAWTChoice );
      }
    } );
    noColorSelector.setColorValue( new RGB(ElevationColorControl.getNoElevationColor().getRed(),
        ElevationColorControl.getNoElevationColor().getGreen(),
        ElevationColorControl.getNoElevationColor().getBlue()));
    optionsColorFormData = new FormData();
    noElevationColorBtn.setText( "SELECT" );
    optionsColorFormData.left = new FormAttachment( noElevationColorLabel, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorBtn.setLayoutData( optionsColorFormData );

    Label colorNumberCells = new Label( optionsColorGroup, SWT.NONE );
    colorNumberCells.setText( "Discrete Color Number" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    colorNumberCells.setLayoutData( optionsColorFormData );

    final Spinner stepper = new Spinner( optionsColorGroup, SWT.BORDER );
    stepper.setMinimum( 0 );
    stepper.setIncrement( 10 );
    stepper.setMaximum( 50 );

    stepper.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        selectedRects = stepper.getSelection();
        if( selectedRects == 0 )
          selectedRects = 1;
        ElevationColorControl.setColorIndex( selectedRects );
        
        System.out.println( "Selected" + selectedRects );
        windowCanvas.redraw();
      }
    } );  
    
    stepper.setSelection( ElevationColorControl.getColorIndex());
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( colorNumberCells, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    stepper.setLayoutData( optionsColorFormData );

    Label optionMinMax = new Label( optionsColorGroup, SWT.NONE );
    optionMinMax.setText( "Go Darker from Max to Min Elevation" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    optionMinMax.setLayoutData( optionsColorFormData );

    checkBtnOptionMinMax = new Button( optionsColorGroup, SWT.CHECK );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( optionMinMax, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    checkBtnOptionMinMax.setLayoutData( optionsColorFormData );
    checkBtnOptionMinMax.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        System.out.println( "Val :" + checkBtnOptionMinMax.getSelection() );
        windowCanvas.redraw();
      }
    } );

    Button applyColors = new Button( optionsColorGroup, SWT.NONE );
    applyColors.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        redrawElevationLayer();
        windowCanvas.redraw();
      }
    } );
    optionsColorFormData = new FormData();
    applyColors.setText( "Apply Colors" );
    optionsColorFormData.bottom = new FormAttachment( 100, -1 );
    optionsColorFormData.left = new FormAttachment( checkBtnOptionMinMax, 5 );
    applyColors.setLayoutData( optionsColorFormData );
    windowCanvas.redraw();

  }

  private Canvas createCanvas( Composite parent, int style, PaintListener pl )
  {
    Canvas c = new Canvas( parent, style );
    if( pl != null )
    {
      c.addPaintListener( pl );
    }
    return c;
  }

  private Canvas createCanvas( Composite parent, PaintListener pl )
  {
    return createCanvas( parent, SWT.NONE, pl );
  }

  private final void redrawElevationLayer( )
  {
    IKalypsoFeatureTheme elevationTheme = dataModel.getElevationTheme();
    if( elevationTheme == null )
    {
      System.out.println( "Elevation theme model is null" );
      return;
    }
    else
    {
      CommandableWorkspace workspace = elevationTheme.getWorkspace();
      ModellEvent event = new FeatureStructureChangeModellEvent( workspace, elevationTheme.getFeatureList().getParentFeature(), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
      workspace.fireModellEvent( event );
    }
  }
  public float[] getRealHSB(Color color){    
  
    float[] val = Color.RGBtoHSB( color.getRed(),color.getGreen(),color.getBlue(),null);
    
    return val;
    }

}
