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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorControl;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
public class ColorModelChangeComponent extends Composite implements IColorModelPreferenceConstants
{
  static private IPreferenceStore m_preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final IPropertyChangeListener m_storePropertyChangeListener = new IPropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent event )
    {
      handlePropertyChange( event );
    }
  };

  // TODO: move into separate class
  private Canvas m_legendCanvas;

  private int m_numOfClasses = 0;

  private final List<org.eclipse.swt.graphics.Color> m_colorList = new ArrayList<>();

  private IElevationColorModel m_colorModel;

  private double MINI_ELEVATION = 0;

  private double MAXI_ELEVATION = 0;

  private final ApplyElevationWidgetDataModel m_dataModel;

  double m_maxElevationBorder;

  double m_minElevationBorder;

  private Label m_minTextLabel;

  private Label m_maxTextLabel;

  public ColorModelChangeComponent( final FormToolkit toolkit, final Composite parent, final ApplyElevationWidgetDataModel dataModel )
  {
    super( parent, SWT.NONE );

    m_dataModel = dataModel;

    toolkit.adapt( this );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).equalWidth( false ).applyTo( this );

    m_preferenceStore.addPropertyChangeListener( m_storePropertyChangeListener );
    initStoreDefaults();

    try
    {
      initData();
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
    }

    createControls( toolkit, this );

    ControlUtils.addDisposeListener( this );
  }

  private void initData( ) throws ElevationException
  {
    /* check, if there is an user defined border in the color control */
    m_maxElevationBorder = ElevationColorControl.getMaxElevation();
    m_minElevationBorder = ElevationColorControl.getMinElevation();

    /* if not, take the min max values from the elevation model */
    final ITerrainElevationModelSystem elevationModelSystem = m_dataModel.getElevationModelSystem();
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
  }

  @Override
  public void dispose( )
  {
    m_preferenceStore.removePropertyChangeListener( m_storePropertyChangeListener );

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
      if( !m_preferenceStore.contains( LINE_MAX_COLOR ) )
      {
        PreferenceConverter.setValue( m_preferenceStore, LINE_MAX_COLOR, makeRGB( ElevationColorControl.getMaxColor() ) );
      }
      else
      {
        ElevationColorControl.setMaxColor( getThisColor( LINE_MAX_COLOR ) );
      }

      if( !m_preferenceStore.contains( LINE_MIN_COLOR ) )
      {
        PreferenceConverter.setValue( m_preferenceStore, LINE_MIN_COLOR, makeRGB( ElevationColorControl.getMinColor() ) );
      }
      else
      {
        ElevationColorControl.setMinColor( getThisColor( LINE_MIN_COLOR ) );
      }

      if( !m_preferenceStore.contains( LINE_NO_COLOR ) )
      {
        PreferenceConverter.setValue( m_preferenceStore, LINE_NO_COLOR, makeRGB( ElevationColorControl.getNoElevationColor() ) );
      }
      else
      {
        ElevationColorControl.setNoElevationColor( getThisColor( LINE_NO_COLOR ) );
      }

      if( !m_preferenceStore.contains( LINE_COLOR_INDEX ) )
      {
        m_preferenceStore.setValue( LINE_COLOR_INDEX, ElevationColorControl.getColorIndex() );
      }
      else
      {
        ElevationColorControl.setColorIndex( m_preferenceStore.getInt( LINE_COLOR_INDEX ) );
      }

      if( !m_preferenceStore.contains( LINE_TRANSPARENCY ) )
      {
        m_preferenceStore.setValue( LINE_TRANSPARENCY, ElevationColorControl.getTransparencyIndex() );
      }
      else
      {
        ElevationColorControl.setTransparencyIndex( m_preferenceStore.getInt( LINE_TRANSPARENCY ) );
      }

      if( !m_preferenceStore.contains( LINE_MIN_MAX ) )
      {
        m_preferenceStore.setDefault( LINE_MIN_MAX, ElevationColorControl.getMinMaxStatus() );
        m_preferenceStore.setValue( LINE_MIN_MAX, ElevationColorControl.getMinMaxStatus() );
      }
      else
      {
        ElevationColorControl.setMinMaxStatus( m_preferenceStore.getBoolean( LINE_MIN_MAX ) );
      }

      if( !m_preferenceStore.contains( ELEV_MAX ) )
      {
        m_preferenceStore.setValue( ELEV_MAX, m_maxElevationBorder );
      }
      else
      {
        ElevationColorControl.setMaxElevation( m_preferenceStore.getDouble( ELEV_MAX ) );
      }

      if( !m_preferenceStore.contains( ELEV_MIN ) )
      {
        m_preferenceStore.setValue( ELEV_MIN, m_minElevationBorder );
      }
      else
      {
        ElevationColorControl.setMinElevation( m_preferenceStore.getDouble( ELEV_MIN ) );
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  private void createControls( final FormToolkit toolkit, final Composite parent )
  {
    createFirstGroup( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, true ) );
    createSecondGroup( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createApplyButton( toolkit, parent ).setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false, 2, 1 ) );

    m_legendCanvas.redraw();
  }

  private Control createApplyButton( final FormToolkit toolkit, final Composite parent )
  {
    final Action applyAction = new ColorModelApplyAction( m_legendCanvas, m_dataModel );

    return ActionButton.createButton( toolkit, parent, applyAction );
  }

  /**
   * creates the color range display
   */
  private Control createFirstGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Group minMaxGroup = new Group( parent, SWT.NONE );
    toolkit.adapt( minMaxGroup );
    minMaxGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.4" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( minMaxGroup );

    /* max label */
    final Label maxLabel = toolkit.createLabel( minMaxGroup, Messages.getString( "ColorModelChangeComponent.0" ), SWT.NONE ); //$NON-NLS-1$
    final GridData gridDataMaxLabel = new GridData( SWT.CENTER, SWT.UP, false, false );
    maxLabel.setLayoutData( gridDataMaxLabel );

    // TODO: move into separate class
    m_legendCanvas = new Canvas( minMaxGroup, SWT.NONE );
    final GridData gridDataCanvas = new GridData( SWT.CENTER, SWT.FILL, false, true, 1, 2 );
    gridDataCanvas.widthHint = 24;
    m_legendCanvas.setLayoutData( gridDataCanvas );
    m_legendCanvas.addPaintListener( new PaintListener()
    {
      @Override
      public void paintControl( final PaintEvent e )
      {
        paintElevationColorSelection( e.gc );
      }
    } );

    /* label for the max displayed elevation value */
    final String stringMax = String.format( "%.3f", m_maxElevationBorder ); //$NON-NLS-1$
    m_maxTextLabel = toolkit.createLabel( minMaxGroup, stringMax );
    m_maxTextLabel.setLayoutData( new GridData( SWT.CENTER, SWT.UP, false, false ) );

    /* min label */
    final Label minLabel = toolkit.createLabel( minMaxGroup, Messages.getString( "ColorModelChangeComponent.1" ) ); //$NON-NLS-1$
    minLabel.setLayoutData( new GridData( SWT.CENTER, SWT.DOWN, false, false ) );

    /* label for the min displayed elevation value */
    final String stringMin = String.format( "%.3f", m_minElevationBorder ); //$NON-NLS-1$
    m_minTextLabel = toolkit.createLabel( minMaxGroup, stringMin );
    m_minTextLabel.setLayoutData( new GridData( SWT.LEFT, SWT.DOWN, true, true ) );

    /* switch color range check button */
    final Button checkBtnOptionMinMax1 = new Button( minMaxGroup, SWT.CHECK );
    checkBtnOptionMinMax1.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false, 2, 1 ) );
    checkBtnOptionMinMax1.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.9" ) ); //$NON-NLS-1$
    checkBtnOptionMinMax1.setSelection( false );
    checkBtnOptionMinMax1.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleSwitchPressed( checkBtnOptionMinMax1.getSelection() );
      }
    } );
    checkBtnOptionMinMax1.setSelection( ElevationColorControl.getMinMaxStatus() );

    return minMaxGroup;
  }

  protected void handleSwitchPressed( final boolean selected )
  {
    // fixes ticket #644.
    // according to implementation of ElevationColorControl here needs the manual replacement of min-max values.
    // only setting of invert flag is not enough here
    final Color maxColor = ElevationColorControl.getMaxColor();
    ElevationColorControl.setMaxColor( ElevationColorControl.getMinColor() );
    ElevationColorControl.setMinColor( maxColor );
    m_preferenceStore.setValue( LINE_MIN_MAX, selected );
    m_legendCanvas.redraw();
  }

  /**
   * Creates the preview of the number of classes selected along with selected MAX Color and selected MIN Color.
   * 
   * @param GraphicCanvas
   */
  void paintElevationColorSelection( final GC graphicCanvas )
  {
    m_numOfClasses = ElevationColorControl.getColorIndex();

    final Point size = m_legendCanvas.getSize();
    final int legendHeight = Math.max( size.y - 5, 0 );

    final int coord = (int)Math.floor( legendHeight / m_numOfClasses );
    if( coord == 0 )
      return;

    // get the min/max values for the color range from the color model
    final double[] values = m_colorModel.getElevationMinMax();
    MINI_ELEVATION = values[0];
    MAXI_ELEVATION = values[1];

    m_colorModel = ElevationColorControl.getColorModel( MINI_ELEVATION, MAXI_ELEVATION );

    // start with the max value
    double selectElevation = MAXI_ELEVATION;

    // calculate the color step range
    final double step = (Math.abs( MAXI_ELEVATION - MINI_ELEVATION )) / (m_numOfClasses - 1);

    int restHeigth = legendHeight;

    /* first class */
    int coordStart = 0;
    int coordEnd = coord;
    int classHeigth = coordEnd - coordStart;

    final Device display = graphicCanvas.getDevice();

    // fill
    Color gotColor = m_colorModel.getColor( MAXI_ELEVATION );
    org.eclipse.swt.graphics.Color backgroundColor = new org.eclipse.swt.graphics.Color( display, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
    graphicCanvas.setBackground( backgroundColor );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
    backgroundColor.dispose();

    // border
    final org.eclipse.swt.graphics.Color foregroundColor = new org.eclipse.swt.graphics.Color( display, new RGB( 0, 0, 0 ) );
    graphicCanvas.setForeground( foregroundColor );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );

    restHeigth = restHeigth - 2 * classHeigth; // substract the heigth for the first and last class

    /* inbetween */
    for( int i = 1; i < m_numOfClasses - 1; i++ )
    {
      selectElevation = MAXI_ELEVATION - i * step;

      coordStart = coordEnd;
      coordEnd = coordStart + (int)(Math.ceil( restHeigth / (m_numOfClasses - (i + 1)) ));
      classHeigth = coordEnd - coordStart;
      restHeigth = restHeigth - classHeigth;

      gotColor = m_colorModel.getColor( selectElevation );
      backgroundColor = new org.eclipse.swt.graphics.Color( display, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
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
    backgroundColor = new org.eclipse.swt.graphics.Color( display, (new RGB( gotColor.getRed(), gotColor.getGreen(), gotColor.getBlue() )) );
    graphicCanvas.setBackground( backgroundColor );
    graphicCanvas.fillRectangle( 0, coordStart, 20, classHeigth );
    backgroundColor.dispose();

    graphicCanvas.setForeground( foregroundColor );
    graphicCanvas.drawRectangle( 0, coordStart, 19, classHeigth );
    foregroundColor.dispose();
  }

  /**
   * GUI Part of Second Grouping
   */
  private Control createSecondGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Group optionsColorGroup = new Group( parent, SWT.NONE );
    optionsColorGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.12" ) ); //$NON-NLS-1$

    /* Upper bound */
    toolkit.createLabel( optionsColorGroup, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.14" ), SWT.FILL ); //$NON-NLS-1$

    final double maxElevation = ElevationColorControl.getMaxElevation();
    final String stringMax = String.format( "%.2f", maxElevation ); //$NON-NLS-1$

    // FIXME: use data binding instead...!
    final Text maxText = toolkit.createText( optionsColorGroup, stringMax, SWT.BORDER );
    maxText.setLayoutData( createColorTextData() );

    maxText.addKeyListener( new KeyAdapter()
    {
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

    maxText.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        checkMaxTextValue( maxText );
      }
    } );

    final Display display = optionsColorGroup.getDisplay();
    final org.eclipse.swt.graphics.Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final org.eclipse.swt.graphics.Color badColor = display.getSystemColor( SWT.COLOR_RED );
    maxText.addModifyListener( new DoubleModifyListener( goodColor, badColor ) );

    /* Lower bound */
    toolkit.createLabel( optionsColorGroup, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.16" ), SWT.FILL ); //$NON-NLS-1$

    final double minElevation = ElevationColorControl.getMinElevation();
    final String stringMin = String.format( "%.2f", minElevation ); //$NON-NLS-1$

    final Text minText = toolkit.createText( optionsColorGroup, stringMin, SWT.BORDER );
    minText.setLayoutData( createColorTextData() );

    minText.addKeyListener( new KeyAdapter()
    {
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

    minText.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        checkMinTextValue( minText );
      }
    } );

    minText.addModifyListener( new DoubleModifyListener( goodColor, badColor ) );

    final ColorFieldEditor maxColorSelector = new ColorFieldEditor( LINE_MAX_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.28" ), optionsColorGroup ); //$NON-NLS-1$
    maxColorSelector.setPreferenceStore( m_preferenceStore );
    maxColorSelector.setPropertyChangeListener( m_storePropertyChangeListener );
    maxColorSelector.getColorSelector().addListener( m_storePropertyChangeListener );
    maxColorSelector.load();
    final Button buttonMax = maxColorSelector.getColorSelector().getButton();
    buttonMax.setLayoutData( new GridData( GridData.END, GridData.CENTER, true, false ) );

    final ColorFieldEditor minColorSelector = new ColorFieldEditor( LINE_MIN_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.29" ), optionsColorGroup ); //$NON-NLS-1$
    minColorSelector.setPreferenceStore( m_preferenceStore );
    minColorSelector.setPropertyChangeListener( m_storePropertyChangeListener );
    minColorSelector.getColorSelector().addListener( m_storePropertyChangeListener );
    minColorSelector.load();
    final Button buttonMin = minColorSelector.getColorSelector().getButton();
    buttonMin.setLayoutData( new GridData( GridData.END, GridData.CENTER, true, false ) );

    final ColorFieldEditor noColorSelector = new ColorFieldEditor( LINE_NO_COLOR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.30" ), optionsColorGroup ); //$NON-NLS-1$
    noColorSelector.setPreferenceStore( m_preferenceStore );
    noColorSelector.setPropertyChangeListener( m_storePropertyChangeListener );
    noColorSelector.getColorSelector().addListener( m_storePropertyChangeListener );
    noColorSelector.load();
    final Button buttonNo = noColorSelector.getColorSelector().getButton();
    buttonNo.setLayoutData( new GridData( GridData.END, GridData.CENTER, true, false ) );

    toolkit.createLabel( optionsColorGroup, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.31" ) ); //$NON-NLS-1$

    final Spinner spinNumColorClasses = new Spinner( optionsColorGroup, SWT.BORDER );
    toolkit.adapt( spinNumColorClasses );
    spinNumColorClasses.setLayoutData( createColorTextData() );
    spinNumColorClasses.setMinimum( 1 );
    spinNumColorClasses.setIncrement( 1 );
    spinNumColorClasses.setMaximum( 20 );
    spinNumColorClasses.setSelection( ElevationColorControl.getColorIndex() );
    spinNumColorClasses.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleNumColorSelected( spinNumColorClasses.getSelection() );
      }
    } );

    toolkit.createLabel( optionsColorGroup, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent.33" ) ); //$NON-NLS-1$

    final Spinner spinTransparency = new Spinner( optionsColorGroup, SWT.BORDER );
    toolkit.adapt( spinTransparency );
    spinTransparency.setLayoutData( createColorTextData() );
    spinTransparency.setMinimum( 0 );
    spinTransparency.setIncrement( 10 );
    spinTransparency.setMaximum( 99 );
    spinTransparency.setSelection( ElevationColorControl.getTransparencyIndex() );
    spinTransparency.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleTransparencySelected( spinTransparency.getSelection() );
      }
    } );

    m_legendCanvas.redraw();

    // HACK: the ColorField changes the layout; we should get rid of the field editors here...!
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( optionsColorGroup );

    return optionsColorGroup;
  }

  private Object createColorTextData( )
  {
    final GridData data = new GridData( SWT.FILL, SWT.CENTER, true, false );
    data.widthHint = 30;
    data.minimumWidth = 30;
    return data;
  }

  protected void handleTransparencySelected( final int selection )
  {
    ElevationColorControl.setTransparencyIndex( selection );
    m_preferenceStore.setValue( LINE_TRANSPARENCY, selection );
    m_legendCanvas.redraw();
  }

  protected void handleNumColorSelected( final int selection )
  {
    m_numOfClasses = selection;
    if( m_numOfClasses == 0 )
      m_numOfClasses = 1;
    ElevationColorControl.setColorIndex( m_numOfClasses );
    m_preferenceStore.setValue( LINE_COLOR_INDEX, ElevationColorControl.getColorIndex() );
    // System.out.println( "Auswahl" + numOfClasses );
    m_legendCanvas.redraw();
  }

  /**
   * Returns java.awt Color for RGB
   * 
   * @param RGB
   * @return java.awt.Color
   */
  private static final java.awt.Color makeAWTColor( final RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

  /**
   * Returns RGB for java.awt Color
   * 
   * @param java
   *          .awt.Color
   * @return RGB
   */
  private static final RGB makeRGB( final java.awt.Color color )
  {
    return new RGB( color.getRed(), color.getGreen(), color.getBlue() );
  }

  /**
   * Returns Color from Preference Store of this Plugin with Key.
   * 
   * @param RGB
   * @return java.awt.Color
   */
  static public final java.awt.Color getThisColor( final String key )
  {
    if( !m_preferenceStore.contains( key ) )
    {
      PreferenceConverter.setDefault( m_preferenceStore, key, makeRGB( ElevationColorControl.getMaxColor() ) );
      PreferenceConverter.setValue( m_preferenceStore, key, makeRGB( ElevationColorControl.getMaxColor() ) );
    }
    return makeAWTColor( (PreferenceConverter.getColor( m_preferenceStore, key )) );
  }

  /**
   * checks the user typed string for the min elevation value
   * 
   * @param elevationChooseComposite
   *          composite of the text field
   * @param minText
   *          the text field
   */
  protected void checkMinTextValue( final Text minText )
  {
    try
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
      m_preferenceStore.setValue( ELEV_MIN, m_minElevationBorder );
      m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
      m_legendCanvas.redraw();
      m_minTextLabel.setText( String.format( "%.3f", db ) ); //$NON-NLS-1$
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * checks the user typed string for the max elevation value
   * 
   * @param elevationChooseComposite
   *          composite of the text field
   * @param maxText
   *          the text field
   */
  protected void checkMaxTextValue( final Text maxText )
  {
    try
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
      m_preferenceStore.setValue( ELEV_MAX, m_maxElevationBorder );
      m_colorModel.setElevationMinMax( m_minElevationBorder, m_maxElevationBorder );
      m_legendCanvas.redraw();
      m_maxTextLabel.setText( String.format( "%.3f", db ) ); //$NON-NLS-1$
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
    }
  }

  protected void handlePropertyChange( final PropertyChangeEvent event )
  {
    try
    {
      final Object source = event.getSource();
      final String property = event.getProperty();

      if( source instanceof FieldEditor )
      {
        ((FieldEditor)source).store();
      }
      else if( source instanceof ColorSelector )
      {
        // ColorFieldEditor edi=null;
        //
        // ((ColorSelector)source).
      }
      else if( LINE_MAX_COLOR.equals( property ) )
      {
        ElevationColorControl.setMaxColor( makeAWTColor( (RGB)event.getNewValue() ) );
      }
      else if( LINE_MIN_COLOR.equals( property ) )
      {
        ElevationColorControl.setMinColor( makeAWTColor( (RGB)event.getNewValue() ) );
      }
      else if( LINE_NO_COLOR.equals( property ) )
      {
        ElevationColorControl.setNoElevationColor( makeAWTColor( (RGB)event.getNewValue() ) );
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
      if( m_legendCanvas != null && !m_legendCanvas.isDisposed() )
        m_legendCanvas.redraw();
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }
}
