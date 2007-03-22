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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorControl;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.IElevationColorModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
public class ColorModelChangeComponent
{
  private PainterElevationColorModel paintModel = new PainterElevationColorModel();
  private Section elevationColorSection;
  private PaintListener drawListener;
  Canvas windowCanvas;
  private int selectedRects = 0;
  GC gc;
  private Label minLabel;
  Color colorAWTChoice;
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
  private Image image_Apply;
  private Spinner stepper;
  private Spinner transparencyStepper;
  private Label settLabel;

  public void createControl( ApplyElevationWidgetDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    createSelectColor( parent );
//    windowCanvas.redraw();
    
    
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
    //stepper.setSelection( ElevationColorControl.getColorIndex());
    
  }

  private void firstGroup( Group minMaxGroup )
  {
    FormLayout minMaxLayout = new FormLayout();
    minMaxGroup.setText( "Farbbereich" );
    minMaxGroup.setLayout( minMaxLayout );
    FormData formData;
    Label maxLabel = new Label( minMaxGroup, SWT.NONE );
    maxLabel.setText( "Max" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    maxLabel.setLayoutData( formData );
    this.disp = minMaxGroup.getDisplay();
    
    PaintListener paintLis = new PaintListener()
    {
    public void paintControl( PaintEvent e )
    {
      gc = new GC( windowCanvas );
      paintElevationColorSelection( gc );      
    }
    };
    
    windowCanvas = createCanvas( minMaxGroup, paintLis);
//    
    if(ElevationColorControl.getBaseColor()!= null)
      windowCanvas.setBackground(
          getSWTColor( minMaxGroup.getDisplay(),
          ElevationColorControl.getBaseColor() ));
    else    
      windowCanvas.setBackground( 
          minMaxGroup.getDisplay().
          getSystemColor( SWT.COLOR_GRAY ) );
    
    windowCanvas.redraw();
    
    formData = new FormData();
    formData.width = 20;
    //formData.height = 200;
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    formData.left = new FormAttachment( maxLabel, 5 );
    windowCanvas.setLayoutData( formData );

//    settLabel = new Label( minMaxGroup, SWT.BOTTOM );
//    settLabel.setText( "Settles" );
//    formData = new FormData();
//    formData.left = new FormAttachment( 0, 5 );
//    formData.bottom = new FormAttachment( 100, -5 );
//    settLabel.setLayoutData( formData );

    minLabel = new Label( minMaxGroup, SWT.BOTTOM );
    minLabel.setText( "Min" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    minLabel.setLayoutData( formData );
    
    
  }

  void paintElevationColorSelection( GC graphicCanvas )
  {
    selectedRects = ElevationColorControl.getColorIndex();
    int coord = (((int) (Math.ceil( 140D / selectedRects ))));//140
    colorAWTChoice = ElevationColorControl.getBaseColor();
    MAXI_ELEVATION = 100;//dataModel.getElevationModel().getMaxElevation();
    MINI_ELEVATION = 0;//dataModel.getElevationModel().getMinElevation();
    
    double selectElevation = MINI_ELEVATION;
    double part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
    
    colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
    part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
    selectElevation = MINI_ELEVATION; 
    for( int i = 0; i < selectedRects; i++ )
    {
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
    
//    if( !ElevationColorControl.getMinMaxStatus() )
//    {
//      colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
//      part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
//      selectElevation = MINI_ELEVATION; 
//      for( int i = 0; i < selectedRects; i++ )
//      {
//        if( colorAWTChoice != null )
//        {
//          gotColor = colorModel.getColor( selectElevation );
//          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue())) ) );
//          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
//        }
//        else
//        {
//          System.out.println( "Out of Range" );
//        }
//          selectElevation = selectElevation + part1;
//      }  
//      
//    }
//    else
//    {
//      colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
//      part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
//      selectElevation = (float) MINI_ELEVATION;
//      
//      for( int i = 0 ; i < selectedRects; i++ )
//      {
//        
//        if( colorAWTChoice != null )
//        {
//          gotColor = colorModel.getColor( selectElevation );
//         
//          graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
//          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
//        }
//        else
//        {
//          System.out.println( "Out Of Range" );
//        }
//        selectElevation = selectElevation + part1;
//      }
//     }
   }

  private void secondGroup( Group optionsColorGroup )
  {
    FormData optionsColorFormData;
    FormLayout optionsColorGrpLayout = new FormLayout();
    optionsColorGroup.setText( "Optionen" );
    optionsColorGroup.setLayout( optionsColorGrpLayout );

    final Label maxColor = new Label( optionsColorGroup, SWT.FLAT );
    maxColor.setText( "Farbe 1" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    maxColor.setLayoutData( optionsColorFormData );
    final ColorSelector maxColorSelector = new ColorSelector( optionsColorGroup );
    
    final Button maxColorBtn = maxColorSelector.getButton();
    maxColorBtn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e)
      {
        colorAWTChoice = new Color( maxColorSelector.getColorValue().red, maxColorSelector.getColorValue().green, maxColorSelector.getColorValue().blue );
        ElevationColorControl.setMaxColor( colorAWTChoice );
        windowCanvas.redraw();
      }
    } );
     maxColorSelector.setColorValue( new RGB(ElevationColorControl.getBaseColor().getRed(),
        ElevationColorControl.getBaseColor().getGreen(),
        ElevationColorControl.getBaseColor().getBlue()));     
    maxColorBtn.setText( "Auswahl" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment(maxColor, 18 );
//    optionsColorFormData.top = new FormAttachment( 0, 5 );
    maxColorBtn.setLayoutData( optionsColorFormData );
    

    final Label minColor = new Label( optionsColorGroup, SWT.FLAT );
    minColor.setText( "Farbe 2" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 105 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    minColor.setLayoutData( optionsColorFormData );
    final ColorSelector minColorSelector = new ColorSelector( optionsColorGroup );
    
    
    final Button minColorBtn = minColorSelector.getButton();
    minColorBtn.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e)
      {
        colorAWTChoice = new Color( minColorSelector.getColorValue().red, minColorSelector.getColorValue().green, minColorSelector.getColorValue().blue );
        ElevationColorControl.setMinColor( colorAWTChoice );
        windowCanvas.redraw();
      }
    } );
     minColorSelector.setColorValue( new RGB(ElevationColorControl.getMinColor().getRed(),
        ElevationColorControl.getMinColor().getGreen(),
        ElevationColorControl.getMinColor().getBlue()));     
     minColorBtn.setText( "Auswahl" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment(minColor, 18 );
//    optionsColorFormData.top = new FormAttachment( 0, 5 );
    minColorBtn.setLayoutData( optionsColorFormData );
    
    //final TEXT txtMinElevation =toolkit.createText(, SWT.FLAT);
    
    
    noElevationColorLabel = new Label( optionsColorGroup, SWT.NONE );
    noElevationColorLabel.setText( "Fehlfarbe" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorLabel.setLayoutData( optionsColorFormData );

    noColorSelector = new ColorSelector( optionsColorGroup );
    Button noElevationColorBtn = noColorSelector.getButton();
    noElevationColorBtn.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
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
    noElevationColorBtn.setText( "Auswahl" );
    optionsColorFormData.left = new FormAttachment( maxColor, 18 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorBtn.setLayoutData( optionsColorFormData );

    Label colorNumberCells = new Label( optionsColorGroup, SWT.NONE );
    colorNumberCells.setText( "Anzahl Farbklassen" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    colorNumberCells.setLayoutData( optionsColorFormData );

    stepper = new Spinner( optionsColorGroup, SWT.BORDER );
    stepper.setMinimum( 0 );
    stepper.setIncrement( 10 );
    stepper.setMaximum( 50 );
    stepper.setSelection( ElevationColorControl.getColorIndex());
    stepper.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        
        selectedRects = stepper.getSelection();
        if( selectedRects == 0 )
          selectedRects = 1;
        ElevationColorControl.setColorIndex( selectedRects );
        
        System.out.println( "Auswahl" + selectedRects );
        windowCanvas.redraw();
      }
    } );      
    
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( colorNumberCells, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    stepper.setLayoutData( optionsColorFormData );
/**Transparency */
    
    Label transparencyLabel = new Label( optionsColorGroup, SWT.NONE );
    transparencyLabel.setText( "Transparency" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    transparencyLabel.setLayoutData( optionsColorFormData );

    transparencyStepper = new Spinner( optionsColorGroup, SWT.BORDER );
    transparencyStepper.setMinimum( 0 );
    transparencyStepper.setIncrement( 10 );
    transparencyStepper.setMaximum( 100 );
    transparencyStepper.setSelection( ElevationColorControl.getTransparencyIndex());
    transparencyStepper.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        ElevationColorControl.setTransparencyIndex( transparencyStepper.getSelection() );
        windowCanvas.redraw();
      }
    } );  
    
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( colorNumberCells, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    transparencyStepper.setLayoutData( optionsColorFormData );    
    
    Label optionMinMax = new Label( optionsColorGroup, SWT.NONE );
    optionMinMax.setText( "Farbskala umdrehen" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( transparencyStepper, 8 );
    optionMinMax.setLayoutData( optionsColorFormData );

    checkBtnOptionMinMax = new Button( optionsColorGroup, SWT.CHECK );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( optionMinMax, 5 );
    optionsColorFormData.top = new FormAttachment( transparencyStepper, 8 );
    checkBtnOptionMinMax.setLayoutData( optionsColorFormData );
    checkBtnOptionMinMax.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        System.out.println( "Val :" + checkBtnOptionMinMax.getSelection() );
        ElevationColorControl.setMinMaxStatus( checkBtnOptionMinMax.getSelection() );
        windowCanvas.redraw();
      }
    } );
    checkBtnOptionMinMax.setSelection( ElevationColorControl.getMinMaxStatus());
    
    Button applyColors = new Button( optionsColorGroup, SWT.NONE );
    applyColors.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        redrawElevationLayer();
        windowCanvas.redraw();
      }
    } );
    
    optionsColorFormData = new FormData();
    applyColors.setToolTipText( "Farben ¸bernehmen" );
    image_Apply = new Image( 
        optionsColorGroup.getDisplay(),
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/ok.gif" ).getImageData() );
    applyColors.setImage( image_Apply );
    optionsColorFormData.right = new FormAttachment( 100, -2 );
    optionsColorFormData.bottom = new FormAttachment(100,-2);
    
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
  
  public org.eclipse.swt.graphics.Color getSWTColor(Display dis,Color color){
    org.eclipse.swt.graphics.Color swtColor = new org.eclipse.swt.graphics.Color(dis,
                                                                      color.getRed(),
                                                                      color.getGreen(),
                                                                      color.getBlue());
    return swtColor;
  }

}
