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
//import org.eclipse.swt.graphics.Color;
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

/**
 * @author congo
 *
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

  private RGB rgbChoice;

  //private Color colorChoice;
  private Color colorAWTChoice;
  
  private FormToolkit toolkit;
  private Button checkBtnOptionMinMax;
  private Display disp;
  private Composite parent;
  private Color noColorAWTChoice;
  private IElevationColorModel colorModel;
  private double MINI_ELEVATION = 0;
  private double MAXI_ELEVATION = 100;
  private Color gotColor;
  private ApplyElevationWidgetDataModel dataModel;
  
  public void createControl(
      ApplyElevationWidgetDataModel dataModel,
      FormToolkit toolkit,
      Composite parent)
  {
   this.toolkit=toolkit;
   this.parent = parent;
   this.dataModel=dataModel;
   createSelectColor(parent );
  }
  
  private void createSelectColor( Composite  clientComposite)
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
    //formData.right = new FormAttachment( MIN_MAX_WIDTH, 0 );
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

    windowCanvas.setBackground( minMaxGroup.getDisplay().getSystemColor( SWT.COLOR_GRAY ) );

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

    
   // float part = ((float) (0.917 - rgbChoice.getHSB()[2])) / selectedRects;
    //System.out.println("Part :"+part+", HSB Brightness :"+rgbChoice.getHSB()[2]);
    
    float brightnessCount = rgbChoice.getHSB()[2];
    double selectElevation = MINI_ELEVATION;
    //update the color control
    ElevationColorControl.setBaseColor( colorAWTChoice );
    ElevationColorControl.setNoElevatonColor( noColorAWTChoice);
    colorModel = 
        ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
//        new SimpleElevationColorModel(
//                          MINI_ELEVATION ,
//                          MAXI_ELEVATION,
//                          colorAWTChoice,
//                          noColorAWTChoice
//                          );
    
    double part1 = ((MAXI_ELEVATION - MINI_ELEVATION)) /selectedRects;
    
    System.out.println("part1 :"+part1+", Select Elevation :"+selectElevation);
    
   if( !checkBtnOptionMinMax.getSelection() )
    {      
      for( int i = 0; i < selectedRects; i++ )
      {
        if( colorAWTChoice != null )
        {
          gotColor = colorModel.getColor( selectElevation );
          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color(disp,(new RGB
              (
              gotColor.getRed(),
              gotColor.getGreen(),
              gotColor.getBlue()
              )
              )));
          System.out.println("RGB: "+gotColor.getRed()+","+gotColor.getGreen()+","+gotColor.getBlue());
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
      selectElevation = (float) MINI_ELEVATION;

      for( int i = selectedRects - 1; i >= 0; i-- )
      {
        if( colorAWTChoice != null )
        {
          gotColor = colorModel.getColor( selectElevation );
          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color(disp,(new RGB
              (
              gotColor.getRed(),
              gotColor.getGreen(),
              gotColor.getBlue()
              )
              )));
         
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
  
  private final Color interpolateColor(double elevation, double minElevation, double maxElevation, Color baseColor)
  {

    
    float[] hsb1;
    
    
    
    hsb1 = Color.RGBtoHSB(baseColor.getRed(),baseColor.getGreen(),baseColor.getBlue(), null);
    double minBrightness = hsb1[2];
    double maxBrightness = 0.917;

    //    if(Double.isNaN( elevation ))
//    {
//      return noElevationColor;
//    }
     if(elevation>=minElevation && elevation<=maxElevation)
    {

//      double brightness = minBrightness+elevation*(maxBrightness-minBrightness)/(maxElevation-minElevation);
//      
//      System.out.println("This Color HS Brightness :"+hsb[0]+","+hsb[1]+","+brightness);
      Color color1 = Color.getHSBColor( hsb1[0], hsb1[1], hsb1[2] );
      System.out.println("This Color - HSB+++++++++++:"+hsb1[0]+","+hsb1[1]+","+hsb1[2]+", Cololr:"+baseColor+ " color="+color1 +"Converted :" +
          new Color( Color.HSBtoRGB( hsb1[0], hsb1[1], hsb1[2] )));
      //return Color.getHSBColor( hsb[0], hsb[1], (float)brightness );
      return color1;
    }
    else
    {
      throw new IllegalArgumentException(
          "Elevation is out of range:"+
          "\n\tminElevation="+minElevation+
          "\n\tmaxElevation="+maxElevation+
          "\n\tcurrentElevation="+elevation);
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
        
        rgbChoice = colorSelector.getColorValue();
        //java.awt.Color awtColor = Color.;
        colorAWTChoice = new Color(rgbChoice.red, rgbChoice.green, rgbChoice.blue);
//        colorAWTChoice = Color.getHSBColor( rgbChoice.getHSB()[0], rgbChoice.getHSB()[1], rgbChoice.getHSB()[2] );
        System.out.println("color choice - HSB :" +rgbChoice.getHSB()[0]+"," +rgbChoice.getHSB()[1]+"," +rgbChoice.getHSB()[2] );
        System.out.println("color choice - RGB :" +colorAWTChoice.getRed()+"," +colorAWTChoice.getGreen()+"," +colorAWTChoice.getBlue());
        //colorChoice = new Color( disp, rgbChoice );
        paintModel.setBaseColor( colorAWTChoice );
        //selectedRects = 1;
        windowCanvas.redraw();

      }
    } );
    maxColorBtn.setText( "SELECT" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( maximumColor, 5 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );

    maxColorBtn.setLayoutData( optionsColorFormData );

    Label noElevationColorLabel = new Label( optionsColorGroup, SWT.NONE );
    noElevationColorLabel.setText( "No Elevation Color" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorLabel.setLayoutData( optionsColorFormData );

//    final ColorSelector noColorSelector = new ColorSelector( optionsColorGroup );
//    Button noElevationColorBtn = noColorSelector.getButton();
//    noElevationColorBtn.addSelectionListener( new SelectionAdapter()
//    {
//      
//
//      public void widgetSelected( SelectionEvent e )
//      {
//        rgbChoice = colorSelector.getColorValue();
//        noColorAWTChoice = Color.getHSBColor( rgbChoice.getHSB()[0], rgbChoice.getHSB()[1], rgbChoice.getHSB()[2] );
//        paintModel.setNoElevationColor( noColorAWTChoice );
//      }
//    } );
    final ColorSelector noColorSelector = new ColorSelector( optionsColorGroup );
    Button noElevationColorBtn = noColorSelector.getButton();
    noElevationColorBtn.addSelectionListener( new SelectionAdapter()
    {
      

      public void widgetSelected( SelectionEvent e )
      {
          redrawElevationLayer();
//        rgbChoice = colorSelector.getColorValue();
//        noColorAWTChoice = Color.getHSBColor( rgbChoice.getHSB()[0], rgbChoice.getHSB()[1], rgbChoice.getHSB()[2] );
//        paintModel.setNoElevationColor( noColorAWTChoice );
      }
    } );

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
        System.out.println( "Selected" + selectedRects );
        windowCanvas.redraw();
      }
    } );

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

    this.checkBtnOptionMinMax = new Button( optionsColorGroup, SWT.CHECK );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( optionMinMax, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    // optionsColorFormData.bottom = new FormAttachment( 100, -5 );
    checkBtnOptionMinMax.setLayoutData( optionsColorFormData );
    checkBtnOptionMinMax.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        System.out.println( "Val :" + checkBtnOptionMinMax.getSelection() );
        windowCanvas.redraw();
      }
    } );
    // checkBtnOptionMinMax.getEnabled()= false;
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
  
  private final void redrawElevationLayer()
  {
    ElevationTheme elevationTheme = dataModel.getElevationTheme();
    if(elevationTheme==null)
    {
      System.out.println("Elevation theme model is null");
      return;
    }
    else
    {
      elevationTheme.fireKalypsoThemeEvent( null );
    }
  }
  
}
