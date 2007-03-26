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

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
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
public class ColorModelChangeComponent implements IColorModelPreferenceConstants
{

  static private IPreferenceStore preferenceStore_ = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private IPropertyChangeListener storePropertyChangeListener_ = createPropertyChangeLis();

  // private GridPointCollector gridPointCollector_;
  // private PainterElevationColorModel paintModel = new PainterElevationColorModel();

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

  private ColorFieldEditor noColorSelector;

  private Label noElevationColorLabel;

  private Image image_Apply;

  private Spinner stepper;

  private Spinner transparencyStepper;

  private Label settLabel;

  public void createControl( ApplyElevationWidgetDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    preferenceStore_.addPropertyChangeListener( storePropertyChangeListener_ );
    initStoreDefaults();

    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    createSelectColor( parent );
  }

  public void dispose( )
  {
    preferenceStore_.removePropertyChangeListener( storePropertyChangeListener_ );
  }

  /**
   * Initializes The Preference Store from the Elevation Color Model
   */
  private void initStoreDefaults( )
  {
    try
    {
      if( !preferenceStore_.contains( LINE_MAX_COLOR ) )
      {
        PreferenceConverter.setValue( preferenceStore_, LINE_MAX_COLOR, makeRGB( ElevationColorControl.getMaxColor() ) );
      }
      else
      {
        ElevationColorControl.setMaxColor( getThisColor( LINE_MAX_COLOR ) );
      }

      if( !preferenceStore_.contains( LINE_MIN_COLOR ) )
      {
        PreferenceConverter.setValue( preferenceStore_, LINE_MIN_COLOR, makeRGB( ElevationColorControl.getMinColor() ) );
      }
      else
      {
        ElevationColorControl.setMinColor( getThisColor( LINE_MIN_COLOR ) );
      }

      if( !preferenceStore_.contains( LINE_NO_COLOR ) )
      {
        PreferenceConverter.setValue( preferenceStore_, LINE_NO_COLOR, makeRGB( ElevationColorControl.getNoElevationColor() ) );
      }
      else
      {
        ElevationColorControl.setNoElevationColor( getThisColor( LINE_NO_COLOR ) );
      }

      if( !preferenceStore_.contains( LINE_COLOR_INDEX ) )
      {
        preferenceStore_.setValue( LINE_COLOR_INDEX, ElevationColorControl.getColorIndex() );
      }
      else
      {
        ElevationColorControl.setColorIndex( preferenceStore_.getInt( LINE_COLOR_INDEX ) );
      }

      if( !preferenceStore_.contains( LINE_TRANSPARENCY ) )
      {
        preferenceStore_.setValue( LINE_TRANSPARENCY, ElevationColorControl.getTransparencyIndex() );
      }
      else
      {
        ElevationColorControl.setTransparencyIndex( preferenceStore_.getInt( LINE_TRANSPARENCY ) );
      }

      if( !preferenceStore_.contains( LINE_MIN_MAX ) )
      {
        preferenceStore_.setDefault( LINE_MIN_MAX, ElevationColorControl.getMinMaxStatus() );
        preferenceStore_.setValue( LINE_MIN_MAX, ElevationColorControl.getMinMaxStatus() );
      }
      else
      {
        ElevationColorControl.setMinMaxStatus( preferenceStore_.getBoolean( LINE_MIN_MAX ) );
      }
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );

    }

  }
  
  /** 
   * IPropertyChangeListener for Preference Store
   *  
   */
  private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {
      public void propertyChange( PropertyChangeEvent event )
      {
        try
        {
          Object source = event.getSource();
          String property = event.getProperty();

          if( source instanceof FieldEditor )
          {
            ((FieldEditor) source).store();
          }
          else if( source instanceof ColorSelector )
          {
            // ColorFieldEditor edi=null;
            //            
            // ((ColorSelector)source).
          }
          else if( LINE_MAX_COLOR.equals( property ) )
          {
            ElevationColorControl.setMaxColor( makeAWTColor( (RGB) event.getNewValue() ) );
          }
          else if( LINE_MIN_COLOR.equals( property ) )
          {
            ElevationColorControl.setMinColor( makeAWTColor( (RGB) event.getNewValue() ) );
          }
          else if( LINE_NO_COLOR.equals( property ) )
          {
            ElevationColorControl.setNoElevationColor( makeAWTColor( (RGB) event.getNewValue() ) );
          }
          else
          {
            System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() );
          }
          if( windowCanvas != null )
            windowCanvas.redraw();
        }
        catch( Throwable th )
        {
          th.printStackTrace();
          throw new RuntimeException( th );
        }
      }
    };
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

    windowCanvas = createCanvas( minMaxGroup, paintLis );
    windowCanvas.redraw();

    formData = new FormData();
    formData.width = 20;
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    formData.left = new FormAttachment( maxLabel, 5 );
    windowCanvas.setLayoutData( formData );

    minLabel = new Label( minMaxGroup, SWT.BOTTOM );
    minLabel.setText( "Min" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    minLabel.setLayoutData( formData );

  }

  /**    
   * Creates the preview of the number of classes selected along
   * with selected MAX Color and
   *      selected MIN Color.
   * @param GraphicCanvas  
   */
  void paintElevationColorSelection( GC graphicCanvas )
  {
    selectedRects = ElevationColorControl.getColorIndex();
    int coord = (((int) (Math.ceil( 140D / selectedRects ))));
    MAXI_ELEVATION = 100;
    MINI_ELEVATION = 0;

    double selectElevation = MAXI_ELEVATION;
    double part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;

    colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
    part1 = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / selectedRects;
    for( int i = 0; i < selectedRects; i++ )
    {
      gotColor = colorModel.getColor( selectElevation );
      graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
      graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
      selectElevation = selectElevation - part1;
    }
  }

  /**
   * GUI Part of Second Grouping
   * @param Group, acts a container to draw req. components 
   */
  private void secondGroup( Group optionsColorGroup )
  {
    FormData optionsColorFormData;
    FormLayout optionsColorGrpLayout = new FormLayout();
    optionsColorGroup.setText( "Optionen" );
    optionsColorGroup.setLayout( optionsColorGrpLayout );
    Composite smallComposite_1 = new Composite( optionsColorGroup, SWT.FLAT );
    smallComposite_1.setLayout( new GridLayout( 1, false ) );

    final ColorFieldEditor maxColorSelector = new ColorFieldEditor( LINE_MAX_COLOR, "Max Color", smallComposite_1 );
    maxColorSelector.setPreferenceStore( preferenceStore_ );
    maxColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    maxColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    maxColorSelector.load();
    optionsColorFormData = new FormData();
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    smallComposite_1.setLayoutData( optionsColorFormData );
    smallComposite_1.pack();

    Composite smallComposite_2 = new Composite( optionsColorGroup, SWT.FLAT );
    smallComposite_2.setLayout( new GridLayout( 1, false ) );
    final ColorFieldEditor minColorSelector = new ColorFieldEditor( LINE_MIN_COLOR, "Min Color", smallComposite_2 );
    minColorSelector.setPreferenceStore( preferenceStore_ );
    minColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    minColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    minColorSelector.load();
    optionsColorFormData = new FormData();
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    optionsColorFormData.left = new FormAttachment( smallComposite_1, 5 );
    smallComposite_2.setLayoutData( optionsColorFormData );
    smallComposite_2.pack();

    Composite smallComposite_3 = new Composite( optionsColorGroup, SWT.FLAT );
    smallComposite_3.setLayout( new GridLayout( 1, false ) );
    noColorSelector = new ColorFieldEditor( LINE_NO_COLOR, "Aushwahl", smallComposite_3 );
    noColorSelector.setPreferenceStore( preferenceStore_ );
    noColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    noColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    noColorSelector.load();
    optionsColorFormData = new FormData();
    optionsColorFormData.top = new FormAttachment( smallComposite_1, 5 );
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    smallComposite_3.setLayoutData( optionsColorFormData );
    smallComposite_3.pack();

    Label colorNumberCells = new Label( optionsColorGroup, SWT.NONE );
    colorNumberCells.setText( "Anzahl Farbklassen" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( smallComposite_3, 8 );
    colorNumberCells.setLayoutData( optionsColorFormData );

    stepper = new Spinner( optionsColorGroup, SWT.BORDER );
    stepper.setMinimum( 0 );
    stepper.setIncrement( 10 );
    stepper.setMaximum( 50 );
    stepper.setSelection( ElevationColorControl.getColorIndex() );
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
        preferenceStore_.setValue( LINE_COLOR_INDEX, ElevationColorControl.getColorIndex() );
        System.out.println( "Auswahl" + selectedRects );
        windowCanvas.redraw();
      }
    } );

    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( colorNumberCells, 5 );
    optionsColorFormData.top = new FormAttachment( smallComposite_3, 8 );
    stepper.setLayoutData( optionsColorFormData );

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
    transparencyStepper.setSelection( ElevationColorControl.getTransparencyIndex() );
    transparencyStepper.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        ElevationColorControl.setTransparencyIndex( transparencyStepper.getSelection() );
        preferenceStore_.setValue( LINE_TRANSPARENCY, transparencyStepper.getSelection() );
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
        preferenceStore_.setValue( LINE_MIN_MAX, checkBtnOptionMinMax.getSelection() );
        windowCanvas.redraw();
      }
    } );
    checkBtnOptionMinMax.setSelection( ElevationColorControl.getMinMaxStatus() );

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
    image_Apply = new Image( optionsColorGroup.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/ok.gif" ).getImageData() );
    applyColors.setImage( image_Apply );
    optionsColorFormData.right = new FormAttachment( 100, -2 );
    optionsColorFormData.bottom = new FormAttachment( 100, -2 );

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

  public float[] getRealHSB( Color color )
  {

    float[] val = Color.RGBtoHSB( color.getRed(), color.getGreen(), color.getBlue(), null );

    return val;
  }

  public org.eclipse.swt.graphics.Color getSWTColor( Display dis, Color color )
  {
    org.eclipse.swt.graphics.Color swtColor = new org.eclipse.swt.graphics.Color( dis, color.getRed(), color.getGreen(), color.getBlue() );
    return swtColor;
  }

  /**
   * Returns java.awt Color for RGB
   * @param RGB
   * @return java.awt.Color
   */
  static private final java.awt.Color makeAWTColor( RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }
  
  /**
   * Returns RGB for java.awt Color
   * @param java.awt.Color
   * @return RGB  
   */
  static public final RGB makeRGB( java.awt.Color color )
  {
    RGB rgb_ = new RGB( color.getRed(), color.getGreen(), color.getBlue() );
    return rgb_;
  }

  /**
   * Returns Color from Preference Store of this Plugin with Key.
   * @param RGB
   * @return java.awt.Color  
   */
  static public final java.awt.Color getThisColor( String key )
  {
    if( !preferenceStore_.contains( key ) )
    {
      PreferenceConverter.setDefault( preferenceStore_, key, makeRGB( ElevationColorControl.getMaxColor() ) );
      PreferenceConverter.setValue( preferenceStore_, key, makeRGB( ElevationColorControl.getMaxColor() ) );
    }
    return makeAWTColor( (PreferenceConverter.getColor( preferenceStore_, key )) );
  }

}
