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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.awt.Color;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
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

  private int numOfClasses = 0;

  GC gc;

  Color colorAWTChoice;

  private FormToolkit toolkit;

  private Display disp;

  private Composite parent;

  private IElevationColorModel m_colorModel;

  private double MINI_ELEVATION = 0;

  private double MAXI_ELEVATION = 0;

  private ApplyElevationWidgetDataModel dataModel;

  private ColorFieldEditor noColorSelector;

  private Image image_Apply;
  
  private Button m_applyColors;

  double m_maxElevationBorder;

  double m_minElevationBorder;

  private Label m_minTextLabel;

  private Label m_maxTextLabel;

  public void createControl( ApplyElevationWidgetDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    preferenceStore_.addPropertyChangeListener( storePropertyChangeListener_ );
    initStoreDefaults();

    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;

    /* check, if there is an user defined border */
    m_maxElevationBorder = ElevationColorControl.getMaxElevation();
    m_minElevationBorder = ElevationColorControl.getMinElevation();
    /* if not, take the min max values from the elevation model */
    if( m_maxElevationBorder == 0 && m_minElevationBorder == 0 )
    {
      m_maxElevationBorder = dataModel.getElevationModelSystem().getMaxElevation();
      m_minElevationBorder = dataModel.getElevationModelSystem().getMinElevation();
    }
    if( m_maxElevationBorder > dataModel.getElevationModelSystem().getMaxElevation() )
    {
      m_maxElevationBorder = dataModel.getElevationModelSystem().getMaxElevation();
      ElevationColorControl.setMaxElevation( m_maxElevationBorder );
    }
    if( m_minElevationBorder < dataModel.getElevationModelSystem().getMinElevation() )
    {
      m_minElevationBorder = dataModel.getElevationModelSystem().getMinElevation();
      ElevationColorControl.setMinElevation( m_minElevationBorder );
    }

    m_colorModel = ElevationColorControl.getColorModel();
    m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );

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

      if( !preferenceStore_.contains( ELEV_MAX ) )
      {
        preferenceStore_.setValue( ELEV_MAX, m_maxElevationBorder );
      }
      else
      {
        ElevationColorControl.setMaxElevation( preferenceStore_.getDouble( ELEV_MAX ) );
      }

      if( !preferenceStore_.contains( ELEV_MIN ) )
      {
        preferenceStore_.setValue( ELEV_MIN, m_minElevationBorder );
      }
      else
      {
        ElevationColorControl.setMinElevation( preferenceStore_.getDouble( ELEV_MIN ) );
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
   */
  private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {
      @SuppressWarnings("synthetic-access")
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
          else if( ELEV_MAX.equals( property ) )
          {
            ElevationColorControl.setMaxElevation( m_maxElevationBorder );
          }
          else if( ELEV_MIN.equals( property ) )
          {
            ElevationColorControl.setMinElevation( m_minElevationBorder );
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
    clientComposite.setLayout( new GridLayout( 2, false ) );

    final Group minMaxGroup = new Group( clientComposite, SWT.NONE );
    GridData gridDataminMaxGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    minMaxGroup.setLayoutData( gridDataminMaxGroup );
    minMaxGroup.setText( "Farbbereich" );

    final Group optionsColorGroup = new Group( clientComposite, SWT.NONE );
    GridData gridDataoptionsColorGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    optionsColorGroup.setLayoutData( gridDataoptionsColorGroup );

    firstGroup( minMaxGroup );
    secondGroup( optionsColorGroup );
  }

 
  private void firstGroup( Group minMaxGroup )
  {
    minMaxGroup.setLayout( new GridLayout( 1, false ) );

    final Composite compositeMinMax = new Composite( minMaxGroup, SWT.NONE );
    compositeMinMax.setLayout( new GridLayout( 3, false ) );

    GridData gridDataCompositeMinMax = new GridData( SWT.CENTER, SWT.UP, true, true );
    compositeMinMax.setLayoutData( gridDataCompositeMinMax );
    // compositeMinMax.setBackground( compositeMinMax.getDisplay().getSystemColor( SWT.COLOR_GREEN ) );

    final Label maxLabel = new Label( compositeMinMax, SWT.NONE );
    maxLabel.setText( "Max" );
    GridData gridDataMaxLabel = new GridData( SWT.CENTER, SWT.UP, false, false );
    maxLabel.setLayoutData( gridDataMaxLabel );

    this.disp = minMaxGroup.getDisplay();

    PaintListener paintLis = new PaintListener()
    {
      public void paintControl( PaintEvent e )
      {
        gc = new GC( windowCanvas );
        paintElevationColorSelection( gc );
      }
    };

    windowCanvas = createCanvas( compositeMinMax, SWT.NONE, paintLis );

    GridData gridDataCanvas = new GridData( SWT.CENTER, SWT.CENTER, false, false );

    gridDataCanvas.heightHint = 170;
    gridDataCanvas.widthHint = 24;

    gridDataCanvas.verticalSpan = 2;

    windowCanvas.setLayoutData( gridDataCanvas );
    windowCanvas.redraw();

    m_maxTextLabel = new Label( compositeMinMax, SWT.NONE );
    
    final String stringMax = String.format( "%.3f", m_maxElevationBorder );
    m_maxTextLabel.setText( stringMax );
    GridData gridDataMaxText = new GridData( SWT.CENTER, SWT.UP, false, false );
    m_maxTextLabel.setLayoutData( gridDataMaxText );

    final Label minLabel = new Label( compositeMinMax, SWT.NONE );
    minLabel.setText( "Min" );

    GridData gridDataMinLabel = new GridData( SWT.CENTER, SWT.DOWN, false, false );
    minLabel.setLayoutData( gridDataMinLabel );

    m_minTextLabel = new Label( compositeMinMax, SWT.NONE );

    final String stringMin = String.format( "%.3f", m_minElevationBorder );
    m_minTextLabel.setText( stringMin );

    GridData gridDataMinText = new GridData( SWT.LEFT, SWT.DOWN, true, true );
    m_minTextLabel.setLayoutData( gridDataMinText );

    final Composite compositeSwitchSchema = new Composite( minMaxGroup, SWT.NONE );
    compositeSwitchSchema.setLayout( new GridLayout( 1, false ) );

    GridData gridDataCompositeSwitchSchema = new GridData( SWT.LEFT, SWT.CENTER, false, false );
    gridDataCompositeSwitchSchema.horizontalSpan = 2;
    compositeSwitchSchema.setLayoutData( gridDataCompositeSwitchSchema );

    final Button checkBtnOptionMinMax1 = new Button( compositeSwitchSchema, SWT.CHECK );
    checkBtnOptionMinMax1.setText( "Farbskala umdrehen" );
    checkBtnOptionMinMax1.setSelection( false );
    checkBtnOptionMinMax1.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        System.out.println( "Val :" + checkBtnOptionMinMax1.getSelection() );
        ElevationColorControl.setMinMaxStatus( checkBtnOptionMinMax1.getSelection() );
        preferenceStore_.setValue( LINE_MIN_MAX, checkBtnOptionMinMax1.getSelection() );
        windowCanvas.redraw();
      }
    } );
    checkBtnOptionMinMax1.setSelection( ElevationColorControl.getMinMaxStatus() );

  }

  /**
   * Creates the preview of the number of classes selected along with selected MAX Color and selected MIN Color.
   * 
   * @param GraphicCanvas
   */
  void paintElevationColorSelection( GC graphicCanvas )
  {
    final int legendHeight = 168;
    numOfClasses = ElevationColorControl.getColorIndex();
//    int coord = (int) (Math.ceil( 140D / numOfClasses ));
    int coord = (int) (170 / numOfClasses );
    MAXI_ELEVATION = 100;
    MINI_ELEVATION = 0;

    double selectElevation = MAXI_ELEVATION;

    m_colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );
    // colorModel.setElevationMinMax( coord, selectElevation );
    final double step = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / (numOfClasses - 1);

    int restHeigth = legendHeight;

    /* first class */
    int coordStart = 0;
    int coordEnd = coord;
    int classHeigth = coordEnd - coordStart;

    // fill
    Color gotColor = m_colorModel.getColor( MAXI_ELEVATION );
    graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );

    // border
    graphicCanvas.setForeground( new org.eclipse.swt.graphics.Color( disp, new RGB( 0, 0, 0 ) ) );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );

    restHeigth = restHeigth - 2 * classHeigth; // substract the heigth for the first and last class

    for( int i = 1; i < numOfClasses - 1; i++ )
    {
      selectElevation = MAXI_ELEVATION - i * step;

      coordStart = coordEnd;
      coordEnd = coordStart + (int) (Math.ceil( restHeigth / (numOfClasses - (i + 1)) ));
      classHeigth = coordEnd - coordStart;
      restHeigth = restHeigth - classHeigth;

      gotColor = m_colorModel.getColor( selectElevation );
      graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
      graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
      graphicCanvas.setForeground( new org.eclipse.swt.graphics.Color( disp, new RGB( 0, 0, 0 ) ) );
      graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );
    }

    coordStart = coordEnd;
    coordEnd = legendHeight;
    classHeigth = coordEnd - coordStart;

    gotColor = m_colorModel.getColor( MINI_ELEVATION );
    graphicCanvas.setBackground( new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) ) );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
    graphicCanvas.setForeground( new org.eclipse.swt.graphics.Color( disp, new RGB( 0, 0, 0 ) ) );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );
  }

  /**
   * GUI Part of Second Grouping
   * 
   * @param Group,
   *          acts a container to draw req. components
   */
  private void secondGroup( Group optionsColorGroup )
  {
    optionsColorGroup.setText( "Optionen" );
    optionsColorGroup.setLayout( new GridLayout( 2, false ) );

    GridData gridDataoptionsColorGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    optionsColorGroup.setLayoutData( gridDataoptionsColorGroup );
    

    final Composite elevationChooseComposite = toolkit.createComposite( optionsColorGroup, SWT.NONE );
    elevationChooseComposite.setLayout( new GridLayout( 2, false ) );
    final Label maxElevationLabel = toolkit.createLabel( elevationChooseComposite, "obere Grenze",  SWT.NONE );
    final Label minElevationLabel = toolkit.createLabel( elevationChooseComposite, "untere Grenze",  SWT.NONE );

    final Text maxText = new Text( elevationChooseComposite, SWT.BORDER );
    double maxElevation = ElevationColorControl.getMaxElevation();
    
    String stringMax = String.valueOf( maxElevation );
    maxText.setText( stringMax );

    maxText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            System.out.println( "Enter" );

            checkMaxTextValue( elevationChooseComposite, maxText );
        }
      }
    } );


    maxText.addFocusListener( new FocusListener ()
    {

      public void focusGained( FocusEvent e )
      {
        // TODO Auto-generated method stub
        
      }

      @SuppressWarnings("synthetic-access")
      public void focusLost( FocusEvent e )
      {
        checkMaxTextValue( elevationChooseComposite, maxText );      
      }
  } );
    
    maxText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        String tempText = maxText.getText();

        Pattern p = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]+?" );
        Matcher m = p.matcher( tempText );

        if( !m.matches() )
        {
          maxText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          maxText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." );
        }
      }
    } );
 
    final Text minText = new Text( elevationChooseComposite, SWT.BORDER );
    double minElevation = ElevationColorControl.getMinElevation();
    String stringMin = String.valueOf( minElevation );
    minText.setText( stringMin );

    minText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            checkMinTextValue( elevationChooseComposite, minText );
        }
      }  
    } );
    
    minText.addFocusListener( new FocusListener ()
    {

      public void focusGained( FocusEvent e )
      {
        // TODO Auto-generated method stub       
      }

      @SuppressWarnings("synthetic-access")
      public void focusLost( FocusEvent e )
      {
        checkMinTextValue( elevationChooseComposite, minText );      
      }  
  } );

    minText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        String tempText = minText.getText();

        Pattern p = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]+?" );
        Matcher m = p.matcher( tempText );

        if( !m.matches() )
        {
          minText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          minText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." );
        }
      }
    } );
    
    final Composite colorChooseComposite = new Composite( optionsColorGroup, SWT.NONE );
    colorChooseComposite.setLayout( new GridLayout( 1, false ) );

    GridData gridDataColorChooseComposite = new GridData( SWT.FILL, SWT.UP, false, false );
    gridDataColorChooseComposite.horizontalSpan = 2;
    colorChooseComposite.setLayoutData( gridDataColorChooseComposite );

    final ColorFieldEditor maxColorSelector = new ColorFieldEditor( LINE_MAX_COLOR, "Max Farbe", colorChooseComposite );
    maxColorSelector.setPreferenceStore( preferenceStore_ );
    maxColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    maxColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    maxColorSelector.load();
    Button buttonMax = maxColorSelector.getColorSelector().getButton();
    buttonMax.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );

    final ColorFieldEditor minColorSelector = new ColorFieldEditor( LINE_MIN_COLOR, "Min Farbe", colorChooseComposite );
    minColorSelector.setPreferenceStore( preferenceStore_ );
    minColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    minColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    minColorSelector.load();
    Button buttonMin = minColorSelector.getColorSelector().getButton();
    buttonMin.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );

    noColorSelector = new ColorFieldEditor( LINE_NO_COLOR, "Fehlfarbe", colorChooseComposite );
    noColorSelector.setPreferenceStore( preferenceStore_ );
    noColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    noColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    noColorSelector.load();
    Button buttonNo = noColorSelector.getColorSelector().getButton();
    buttonNo.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );
    

    final Composite spinnerComposite = new Composite( optionsColorGroup, SWT.NONE );
    spinnerComposite.setLayout( new GridLayout( 2, false ) );

    GridData gridDataSpinnerComposite = new GridData( SWT.FILL, SWT.UP, false, false );
    gridDataSpinnerComposite.horizontalSpan = 2;
    spinnerComposite.setLayoutData( gridDataSpinnerComposite );
    
    final Label colorNumberCells = new Label( spinnerComposite, SWT.NONE );
    colorNumberCells.setText( "Anzahl Farbklassen" );
    
    final Spinner spinNumColorClasses = new Spinner( spinnerComposite, SWT.NONE );
    toolkit.adapt( spinNumColorClasses );
    spinNumColorClasses.setMinimum( 0 );
    spinNumColorClasses.setIncrement( 1 );
    spinNumColorClasses.setMaximum( 20 );
    spinNumColorClasses.setSelection( ElevationColorControl.getColorIndex() );
    spinNumColorClasses.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        numOfClasses = spinNumColorClasses.getSelection();
        if( numOfClasses == 0 )
          numOfClasses = 1;
        ElevationColorControl.setColorIndex( numOfClasses );
        preferenceStore_.setValue( LINE_COLOR_INDEX, ElevationColorControl.getColorIndex() );
        // System.out.println( "Auswahl" + numOfClasses );
        windowCanvas.redraw();
      }
    } );
    
    final Label transparencyLabel = new Label( spinnerComposite, SWT.NONE );
    transparencyLabel.setText( "Transparenz" );
    
    final Spinner spinTransparency = new Spinner( spinnerComposite, SWT.NONE );
    toolkit.adapt( spinTransparency );
    spinTransparency.setMinimum( 0 );
    spinTransparency.setIncrement( 10 );
    spinTransparency.setMaximum( 99 );
    spinTransparency.setSelection( ElevationColorControl.getTransparencyIndex() );
    spinTransparency.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        ElevationColorControl.setTransparencyIndex( spinTransparency.getSelection() );
        preferenceStore_.setValue( LINE_TRANSPARENCY, spinTransparency.getSelection() );
        windowCanvas.redraw();
      }
    } );

    m_applyColors = toolkit.createButton( spinnerComposite, "", SWT.NONE );
    m_applyColors.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {      
        redrawElevationLayer();
        windowCanvas.redraw();
      }
    } );
    m_applyColors.setToolTipText( "Farben übernehmen" );
    image_Apply = new Image( optionsColorGroup.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/ok.gif" ).getImageData() );
    m_applyColors.setImage( image_Apply );
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
   * 
   * @param RGB
   * @return java.awt.Color
   */
  static private final java.awt.Color makeAWTColor( RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

  /**
   * Returns RGB for java.awt Color
   * 
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
   * 
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

  private void checkMinTextValue( final Composite elevationChooseComposite, final Text minText )
  {
    String tempText = minText.getText();

    Pattern p = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]+?" );
    Matcher m = p.matcher( tempText );

    if( !m.matches() )
    {
      minText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }
    else
    {
      minText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      tempText.replaceAll( ",", "." );

      Double db = new Double( tempText );
      if( db < dataModel.getElevationModelSystem().getMinElevation() )
      {
        db = dataModel.getElevationModelSystem().getMinElevation() ;
        minText.setText( db.toString());
      }
      else if( db >= dataModel.getElevationModelSystem().getMaxElevation() )
      {
        db = dataModel.getElevationModelSystem().getMinElevation() ;
        minText.setText( db.toString());
      }
      m_minElevationBorder = db;
      ElevationColorControl.setMinElevation( m_minElevationBorder );
      preferenceStore_.setValue( ELEV_MIN, m_minElevationBorder );
      m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
      windowCanvas.redraw();
      m_applyColors.setSelection( true );
      m_minTextLabel.setText( String.format( "%.3f",db));
    }
  }

  private void checkMaxTextValue( final Composite elevationChooseComposite, final Text maxText )
  {
    String tempText = maxText.getText();

    Pattern p = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]+?" );
    Matcher m = p.matcher( tempText );

    if( !m.matches() )
    {
      maxText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }
    else
    {
      maxText.setBackground( elevationChooseComposite.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      tempText = tempText.replaceAll( ",", "." );

      Double db = new Double( tempText );
      if( db > dataModel.getElevationModelSystem().getMaxElevation() )
      {
        db = dataModel.getElevationModelSystem().getMaxElevation() ;
        maxText.setText( db.toString());
      }
      else if( db <= dataModel.getElevationModelSystem().getMinElevation() )
      {
        db = dataModel.getElevationModelSystem().getMaxElevation() ;
        maxText.setText( db.toString());
      }
      m_maxElevationBorder = db;
      ElevationColorControl.setMaxElevation( m_maxElevationBorder );
      preferenceStore_.setValue( ELEV_MAX, m_maxElevationBorder );
      m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
      windowCanvas.redraw();
      m_maxTextLabel.setText( String.format( "%.3f",db));
    }
  }
  
}
