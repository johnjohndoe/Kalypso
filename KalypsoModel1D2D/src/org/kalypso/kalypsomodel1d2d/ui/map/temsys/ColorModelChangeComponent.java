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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorControl;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
public class ColorModelChangeComponent implements IColorModelPreferenceConstants
{

  static private IPreferenceStore preferenceStore_ = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final IPropertyChangeListener storePropertyChangeListener_ = createPropertyChangeLis();

  protected Canvas windowCanvas;

  private int numOfClasses = 0;

  protected GC gc;

  private Display disp;

  private final List<org.eclipse.swt.graphics.Color> m_colorList = new ArrayList<org.eclipse.swt.graphics.Color>();

  private IElevationColorModel m_colorModel;

  private double MINI_ELEVATION = 0;

  private double MAXI_ELEVATION = 0;

  private ApplyElevationWidgetDataModel m_dataModel;

  private ColorFieldEditor noColorSelector;

  private Image image_Apply;

  private Button m_applyColors;

  double m_maxElevationBorder;

  double m_minElevationBorder;

  private Label m_minTextLabel;

  private Label m_maxTextLabel;

  private FormToolkit m_toolkit;

  public void createControl( final ApplyElevationWidgetDataModel dataModel, final FormToolkit toolkit, final Composite parent )
  {
    preferenceStore_.addPropertyChangeListener( storePropertyChangeListener_ );
    initStoreDefaults();

    m_toolkit = toolkit;
    m_dataModel = dataModel;

    /* check, if there is an user defined border in the color control */
    m_maxElevationBorder = ElevationColorControl.getMaxElevation();
    m_minElevationBorder = ElevationColorControl.getMinElevation();

    /* if not, take the min max values from the elevation model */
    final ITerrainElevationModelSystem elevationModelSystem = dataModel.getElevationModelSystem();
    if( elevationModelSystem != null )
    {

      if( m_maxElevationBorder == 0 && m_minElevationBorder == 0 )
      {
        m_maxElevationBorder = elevationModelSystem.getMaxElevation();
        m_minElevationBorder = elevationModelSystem.getMinElevation();
      }

      /* do some checks */
      if( m_maxElevationBorder > elevationModelSystem.getMaxElevation() )
      {
        m_maxElevationBorder = elevationModelSystem.getMaxElevation();
        ElevationColorControl.setMaxElevation( m_maxElevationBorder );
      }
      else if( m_maxElevationBorder < elevationModelSystem.getMinElevation() )
      {
        m_maxElevationBorder = elevationModelSystem.getMaxElevation();
        ElevationColorControl.setMaxElevation( m_maxElevationBorder );
      }

      if( m_minElevationBorder < elevationModelSystem.getMinElevation() )
      {
        m_minElevationBorder = elevationModelSystem.getMinElevation();
        ElevationColorControl.setMinElevation( m_minElevationBorder );
      }
      else if( m_minElevationBorder > elevationModelSystem.getMaxElevation() )
      {
        m_minElevationBorder = elevationModelSystem.getMinElevation();
        ElevationColorControl.setMinElevation( m_minElevationBorder );
      }
    }
    m_colorModel = ElevationColorControl.getColorModel();
    m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );

    createSelectColor( parent );
  }

  public void dispose( )
  {
    preferenceStore_.removePropertyChangeListener( storePropertyChangeListener_ );

    for( final org.eclipse.swt.graphics.Color color : m_colorList )
      color.dispose();
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
    catch( final Throwable th )
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
      @Override
      @SuppressWarnings("synthetic-access")
      public void propertyChange( final PropertyChangeEvent event )
      {
        try
        {
          final Object source = event.getSource();
          final String property = event.getProperty();

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
            System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }
          if( windowCanvas != null )
            windowCanvas.redraw();
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
          throw new RuntimeException( th );
        }
      }

    };
  }

  private void createSelectColor( final Composite clientComposite )
  {
    clientComposite.setLayout( new GridLayout( 2, false ) );

    final Group minMaxGroup = new Group( clientComposite, SWT.NONE );
    final GridData gridDataminMaxGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    minMaxGroup.setLayoutData( gridDataminMaxGroup );
    minMaxGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.4" ) ); //$NON-NLS-1$

    final Group optionsColorGroup = new Group( clientComposite, SWT.NONE );
    final GridData gridDataoptionsColorGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    optionsColorGroup.setLayoutData( gridDataoptionsColorGroup );

    firstGroup( minMaxGroup );
    secondGroup( optionsColorGroup );
  }

  /**
   * creates the color range display
   */
  private void firstGroup( final Group minMaxGroup )
  {
    minMaxGroup.setLayout( new GridLayout( 1, false ) );

    final Composite compositeMinMax = new Composite( minMaxGroup, SWT.NONE );
    compositeMinMax.setLayout( new GridLayout( 3, false ) );

    final GridData gridDataCompositeMinMax = new GridData( SWT.CENTER, SWT.UP, true, true );
    compositeMinMax.setLayoutData( gridDataCompositeMinMax );

    /* max label */
    final Label maxLabel = new Label( compositeMinMax, SWT.NONE );
    maxLabel.setText( "Max" ); //$NON-NLS-1$
    final GridData gridDataMaxLabel = new GridData( SWT.CENTER, SWT.UP, false, false );
    maxLabel.setLayoutData( gridDataMaxLabel );

    this.disp = minMaxGroup.getDisplay();

    /* color range */
    final PaintListener paintLis = new PaintListener()
    {
      @Override
      public void paintControl( PaintEvent e )
      {
        gc = new GC( windowCanvas );
        paintElevationColorSelection( gc );
        gc.dispose();
      }
    };

    windowCanvas = createCanvas( compositeMinMax, SWT.NONE, paintLis );

    final GridData gridDataCanvas = new GridData( SWT.CENTER, SWT.CENTER, false, false );

    gridDataCanvas.heightHint = 170;
    gridDataCanvas.widthHint = 24;

    gridDataCanvas.verticalSpan = 2;

    windowCanvas.setLayoutData( gridDataCanvas );
    windowCanvas.redraw();

    /* label for the max displayed elevation value */
    m_maxTextLabel = new Label( compositeMinMax, SWT.NONE );

    final String stringMax = String.format( "%.3f", m_maxElevationBorder ); //$NON-NLS-1$
    m_maxTextLabel.setText( stringMax );
    final GridData gridDataMaxText = new GridData( SWT.CENTER, SWT.UP, false, false );
    m_maxTextLabel.setLayoutData( gridDataMaxText );

    /* min label */
    final Label minLabel = new Label( compositeMinMax, SWT.NONE );
    minLabel.setText( "Min" ); //$NON-NLS-1$

    final GridData gridDataMinLabel = new GridData( SWT.CENTER, SWT.DOWN, false, false );
    minLabel.setLayoutData( gridDataMinLabel );

    /* label for the min displayed elevation value */
    m_minTextLabel = new Label( compositeMinMax, SWT.NONE );

    final String stringMin = String.format( "%.3f", m_minElevationBorder ); //$NON-NLS-1$
    m_minTextLabel.setText( stringMin );

    final GridData gridDataMinText = new GridData( SWT.LEFT, SWT.DOWN, true, true );
    m_minTextLabel.setLayoutData( gridDataMinText );

    /* switch color range check button */
    final Composite compositeSwitchSchema = new Composite( minMaxGroup, SWT.NONE );
    compositeSwitchSchema.setLayout( new GridLayout( 1, false ) );

    final GridData gridDataCompositeSwitchSchema = new GridData( SWT.LEFT, SWT.CENTER, false, false );
    gridDataCompositeSwitchSchema.horizontalSpan = 2;
    compositeSwitchSchema.setLayoutData( gridDataCompositeSwitchSchema );

    final Button checkBtnOptionMinMax1 = new Button( compositeSwitchSchema, SWT.CHECK );
    checkBtnOptionMinMax1.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.9" ) ); //$NON-NLS-1$
    checkBtnOptionMinMax1.setSelection( false );
    checkBtnOptionMinMax1.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.11" ) + checkBtnOptionMinMax1.getSelection() ); //$NON-NLS-1$
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
  void paintElevationColorSelection( final GC graphicCanvas )
  {
    final int legendHeight = 168;
    numOfClasses = ElevationColorControl.getColorIndex();
    final int coord = (170 / numOfClasses);

    // get the min/max values for the color range from the color model
    final double[] values = m_colorModel.getElevationMinMax();
    MINI_ELEVATION = values[0];
    MAXI_ELEVATION = values[1];

    m_colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );

    // start with the max value
    double selectElevation = MAXI_ELEVATION;

    // calculate the color step range
    final double step = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / (numOfClasses - 1);

    int restHeigth = legendHeight;

    /* first class */
    int coordStart = 0;
    int coordEnd = coord;
    int classHeigth = coordEnd - coordStart;

    // fill
    Color gotColor = m_colorModel.getColor( MAXI_ELEVATION );
    org.eclipse.swt.graphics.Color backgroundColor = new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
    graphicCanvas.setBackground( backgroundColor );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
    backgroundColor.dispose();

    // border
    final org.eclipse.swt.graphics.Color foregroundColor = new org.eclipse.swt.graphics.Color( disp, new RGB( 0, 0, 0 ) );
    graphicCanvas.setForeground( foregroundColor );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );

    restHeigth = restHeigth - 2 * classHeigth; // substract the heigth for the first and last class

    /* inbetween */
    for( int i = 1; i < numOfClasses - 1; i++ )
    {
      selectElevation = MAXI_ELEVATION - i * step;

      coordStart = coordEnd;
      coordEnd = coordStart + (int) (Math.ceil( restHeigth / (numOfClasses - (i + 1)) ));
      classHeigth = coordEnd - coordStart;
      restHeigth = restHeigth - classHeigth;

      gotColor = m_colorModel.getColor( selectElevation );
      backgroundColor = new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
      graphicCanvas.setBackground( backgroundColor );
      graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
      backgroundColor.dispose();

      graphicCanvas.setForeground( foregroundColor );
      graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );
    }

    /* last class */
    coordStart = coordEnd;
    coordEnd = legendHeight;
    classHeigth = coordEnd - coordStart;

    gotColor = m_colorModel.getColor( MINI_ELEVATION );
    backgroundColor = new org.eclipse.swt.graphics.Color( disp, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
    graphicCanvas.setBackground( backgroundColor );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
    backgroundColor.dispose();

    graphicCanvas.setForeground( foregroundColor );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );
    foregroundColor.dispose();
  }

  /**
   * GUI Part of Second Grouping
   * 
   * @param Group,
   *            acts a container to draw req. components
   */
  private void secondGroup( final Group optionsColorGroup )
  {
    optionsColorGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.12" ) ); //$NON-NLS-1$
    optionsColorGroup.setLayout( new GridLayout( 2, false ) );

    final GridData gridDataoptionsColorGroup = new GridData( SWT.CENTER, SWT.UP, false, true );
    optionsColorGroup.setLayoutData( gridDataoptionsColorGroup );

    final Composite elevationChooseComposite = m_toolkit.createComposite( optionsColorGroup, SWT.NONE );
    elevationChooseComposite.setLayout( new GridLayout( 2, false ) );
    @SuppressWarnings("unused")
    final Label maxElevationLabel = m_toolkit.createLabel( elevationChooseComposite, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.14" ), SWT.FILL ); //$NON-NLS-1$
    @SuppressWarnings("unused")
    final Label minElevationLabel = m_toolkit.createLabel( elevationChooseComposite, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.16" ), SWT.FILL ); //$NON-NLS-1$

    final Text maxText = new Text( elevationChooseComposite, SWT.BORDER );
    final double maxElevation = ElevationColorControl.getMaxElevation();

    final String stringMax = String.valueOf( maxElevation );
    maxText.setText( stringMax );

    maxText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            checkMaxTextValue( maxText );
        }
      }
    } );

    maxText.addFocusListener( new FocusListener()
    {

      @Override
      public void focusGained( final FocusEvent e )
      {

      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        checkMaxTextValue( maxText );
      }
    } );

    final Display display = elevationChooseComposite.getDisplay();
    final org.eclipse.swt.graphics.Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final org.eclipse.swt.graphics.Color badColor = display.getSystemColor( SWT.COLOR_RED );
    maxText.addModifyListener( new DoubleModifyListener( goodColor, badColor ) );

    final Text minText = new Text( elevationChooseComposite, SWT.BORDER );
    final double minElevation = ElevationColorControl.getMinElevation();
    final String stringMin = String.valueOf( minElevation );
    minText.setText( stringMin );

    minText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            checkMinTextValue( minText );
        }
      }
    } );

    minText.addFocusListener( new FocusListener()
    {

      @Override
      public void focusGained( final FocusEvent e )
      {

      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        checkMinTextValue( minText );
      }
    } );

    minText.addModifyListener( new DoubleModifyListener( goodColor, badColor ) );

    final Composite colorChooseComposite = new Composite( optionsColorGroup, SWT.NONE );
    colorChooseComposite.setLayout( new GridLayout( 1, false ) );

    final GridData gridDataColorChooseComposite = new GridData( SWT.FILL, SWT.UP, false, false );
    gridDataColorChooseComposite.horizontalSpan = 2;
    colorChooseComposite.setLayoutData( gridDataColorChooseComposite );

    final ColorFieldEditor maxColorSelector = new ColorFieldEditor( LINE_MAX_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.28" ), colorChooseComposite ); //$NON-NLS-1$
    maxColorSelector.setPreferenceStore( preferenceStore_ );
    maxColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    maxColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    maxColorSelector.load();
    final Button buttonMax = maxColorSelector.getColorSelector().getButton();
    buttonMax.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );

    final ColorFieldEditor minColorSelector = new ColorFieldEditor( LINE_MIN_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.29" ), colorChooseComposite ); //$NON-NLS-1$
    minColorSelector.setPreferenceStore( preferenceStore_ );
    minColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    minColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    minColorSelector.load();
    final Button buttonMin = minColorSelector.getColorSelector().getButton();
    buttonMin.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );

    noColorSelector = new ColorFieldEditor( LINE_NO_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.30" ), colorChooseComposite ); //$NON-NLS-1$
    noColorSelector.setPreferenceStore( preferenceStore_ );
    noColorSelector.setPropertyChangeListener( storePropertyChangeListener_ );
    noColorSelector.getColorSelector().addListener( storePropertyChangeListener_ );
    noColorSelector.load();
    final Button buttonNo = noColorSelector.getColorSelector().getButton();
    buttonNo.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );

    final Composite spinnerComposite = new Composite( optionsColorGroup, SWT.NONE );
    spinnerComposite.setLayout( new GridLayout( 2, false ) );

    final GridData gridDataSpinnerComposite = new GridData( SWT.FILL, SWT.UP, false, false );
    gridDataSpinnerComposite.horizontalSpan = 2;
    spinnerComposite.setLayoutData( gridDataSpinnerComposite );

    final Label colorNumberCells = new Label( spinnerComposite, SWT.NONE );
    colorNumberCells.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.31" ) ); //$NON-NLS-1$

    final Spinner spinNumColorClasses = new Spinner( spinnerComposite, SWT.NONE );
    m_toolkit.adapt( spinNumColorClasses );
    spinNumColorClasses.setMinimum( 0 );
    spinNumColorClasses.setIncrement( 1 );
    spinNumColorClasses.setMaximum( 20 );
    spinNumColorClasses.setSelection( ElevationColorControl.getColorIndex() );
    spinNumColorClasses.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
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
    transparencyLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.33" ) ); //$NON-NLS-1$

    final Spinner spinTransparency = new Spinner( spinnerComposite, SWT.NONE );
    m_toolkit.adapt( spinTransparency );
    spinTransparency.setMinimum( 0 );
    spinTransparency.setIncrement( 10 );
    spinTransparency.setMaximum( 99 );
    spinTransparency.setSelection( ElevationColorControl.getTransparencyIndex() );
    spinTransparency.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        ElevationColorControl.setTransparencyIndex( spinTransparency.getSelection() );
        preferenceStore_.setValue( LINE_TRANSPARENCY, spinTransparency.getSelection() );
        windowCanvas.redraw();
      }
    } );

    m_applyColors = m_toolkit.createButton( spinnerComposite, "", SWT.NONE ); //$NON-NLS-1$
    m_applyColors.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        redrawElevationLayer();
        windowCanvas.redraw();
      }
    } );
    m_applyColors.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.37" ) ); //$NON-NLS-1$
    image_Apply = new Image( optionsColorGroup.getDisplay(), AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/ok.gif" ).getImageData() ); //$NON-NLS-1$
    m_applyColors.setImage( image_Apply );
    windowCanvas.redraw();
  }

  private Canvas createCanvas( final Composite parent, final int style, final PaintListener pl )
  {
    final Canvas c = new Canvas( parent, style );
    if( pl != null )
    {
      c.addPaintListener( pl );
    }
    return c;
  }

  private final void redrawElevationLayer( )
  {
    final IKalypsoFeatureTheme elevationTheme = m_dataModel.getElevationTheme();
    if( elevationTheme == null )
    {
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.39" ) ); //$NON-NLS-1$
      return;
    }
    else
    {
      final CommandableWorkspace workspace = elevationTheme.getWorkspace();
      final ModellEvent event = new FeatureStructureChangeModellEvent( workspace, elevationTheme.getFeatureList().getParentFeature(), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
      workspace.fireModellEvent( event );
    }
  }

  public float[] getRealHSB( final Color color )
  {

    final float[] val = Color.RGBtoHSB( color.getRed(), color.getGreen(), color.getBlue(), null );

    return val;
  }

  /**
   * Returns java.awt Color for RGB
   * 
   * @param RGB
   * @return java.awt.Color
   */
  static private final java.awt.Color makeAWTColor( final RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

  /**
   * Returns RGB for java.awt Color
   * 
   * @param java.awt.Color
   * @return RGB
   */
  static public final RGB makeRGB( final java.awt.Color color )
  {
    final RGB rgb_ = new RGB( color.getRed(), color.getGreen(), color.getBlue() );
    return rgb_;
  }

  /**
   * Returns Color from Preference Store of this Plugin with Key.
   * 
   * @param RGB
   * @return java.awt.Color
   */
  static public final java.awt.Color getThisColor( final String key )
  {
    if( !preferenceStore_.contains( key ) )
    {
      PreferenceConverter.setDefault( preferenceStore_, key, makeRGB( ElevationColorControl.getMaxColor() ) );
      PreferenceConverter.setValue( preferenceStore_, key, makeRGB( ElevationColorControl.getMaxColor() ) );
    }
    return makeAWTColor( (PreferenceConverter.getColor( preferenceStore_, key )) );
  }

  /**
   * checks the user typed string for the min elevation value
   * 
   * @param elevationChooseComposite
   *            composite of the text field
   * @param minText
   *            the text field
   */
  private void checkMinTextValue( final Text minText )
  {
    final String tempText = minText.getText();

    Double db = NumberUtils.parseQuietDouble( tempText );

    if( db < m_dataModel.getElevationModelSystem().getMinElevation() )
    {
      db = m_dataModel.getElevationModelSystem().getMinElevation();
      minText.setText( db.toString() );
    }
    else if( db >= m_dataModel.getElevationModelSystem().getMaxElevation() )
    {
      db = m_dataModel.getElevationModelSystem().getMinElevation();
      minText.setText( db.toString() );
    }

    if( m_maxTextLabel.getText() != null )
    {
      String textMax = m_maxTextLabel.getText();
      textMax = textMax.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      final Double maxValue = new Double( textMax );
      if( db >= maxValue )
      {
        db = maxValue - 0.01;
        minText.setText( db.toString() );
      }
    }

    m_minElevationBorder = db;
    ElevationColorControl.setMinElevation( m_minElevationBorder );
    preferenceStore_.setValue( ELEV_MIN, m_minElevationBorder );
    m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
    windowCanvas.redraw();
    m_applyColors.setSelection( true );
    m_minTextLabel.setText( String.format( "%.3f", db ) ); //$NON-NLS-1$
  }

  /**
   * checks the user typed string for the max elevation value
   * 
   * @param elevationChooseComposite
   *            composite of the text field
   * @param maxText
   *            the text field
   */
  private void checkMaxTextValue( final Text maxText )
  {
    final String tempText = maxText.getText();

    Double db = NumberUtils.parseQuietDouble( tempText );

    if( db > m_dataModel.getElevationModelSystem().getMaxElevation() )
    {
      db = m_dataModel.getElevationModelSystem().getMaxElevation();
      maxText.setText( db.toString() );
    }
    else if( db <= m_dataModel.getElevationModelSystem().getMinElevation() )
    {
      db = m_dataModel.getElevationModelSystem().getMaxElevation();
      maxText.setText( db.toString() );
    }
    if( m_minTextLabel.getText() != null )
    {
      String textMin = m_minTextLabel.getText();
      textMin = textMin.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      final Double minValue = new Double( textMin );
      if( db <= minValue )
      {
        db = minValue + 0.01;
        maxText.setText( db.toString() );
      }
    }
    m_maxElevationBorder = db;
    ElevationColorControl.setMaxElevation( m_maxElevationBorder );
    preferenceStore_.setValue( ELEV_MAX, m_maxElevationBorder );
    m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
    windowCanvas.redraw();
    m_maxTextLabel.setText( String.format( "%.3f", db ) ); //$NON-NLS-1$
  }

}
