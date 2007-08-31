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
package org.kalypso.ui.wizards.results;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author Thomas Jung
 * 
 */
public class LineColorMapEditorComposite extends Composite
{

  private final LineColorMap m_colorMap;

  private LineColorMapEntry m_entry;

  private final Pattern m_patternDouble = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]*?" );

  private double m_stepWidth;

  private final int m_fatValue;

  private final int m_fatWidth;

  private double m_minValue;

  private double m_maxValue;

  private final String m_globalMin;

  private final String m_globalMax;

  public LineColorMapEditorComposite( final Composite parent, final int style, final LineColorMap colorMap, final double minGlobalValue, final double maxGlobalValue )
  {

    super( parent, style );
    m_colorMap = colorMap;

    m_minValue = m_colorMap.getColorMap()[0].getQuantity( null );
    m_maxValue = m_colorMap.getColorMap()[m_colorMap.getColorMap().length - 1].getQuantity( null );

    m_globalMin = new Double( minGlobalValue ).toString();
    m_globalMax = new Double( maxGlobalValue ).toString();

    /* default parameter */
    m_stepWidth = 0.1;
    m_fatValue = 5;
    m_fatWidth = 5;

    createControl();
  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    createMinMaxComponent( this );

    createEntryGroup();

    createPropertyGroup();

  }

  private void createPropertyGroup( )
  {
    final Group propertyGroup = new Group( this, SWT.NONE );
    propertyGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    propertyGroup.setLayout( new GridLayout( 2, false ) );
    propertyGroup.setText( "erweiterte Einstellungen" );

    // step width spinner
    final Label labelWithSpinner = new Label( propertyGroup, SWT.NONE );
    labelWithSpinner.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    labelWithSpinner.setText( "Intervallbreite" );

    final Text stepWidthText = new Text( propertyGroup, SWT.BORDER | SWT.TRAIL );
    GridData gridDataStepWidthText = new GridData( SWT.END, SWT.CENTER, true, false );
    gridDataStepWidthText.widthHint = 20;
    stepWidthText.setLayoutData( gridDataStepWidthText );

    final String stringStepWidth = String.valueOf( m_stepWidth );
    stepWidthText.setText( stringStepWidth );

    stepWidthText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            checkDoubleTextValue( propertyGroup, stepWidthText );
        }
      }
    } );

    stepWidthText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access")
      public void focusGained( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, stepWidthText );
        if( value != null )
          m_stepWidth = value;
      }

      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, stepWidthText );
        if( value != null )
        {
          m_stepWidth = value;
          updateColorMap();
        }
      }
    } );

    stepWidthText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        final String tempText = stepWidthText.getText();

        final Matcher m = m_patternDouble.matcher( tempText );

        if( !m.matches() )
        {
          stepWidthText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          stepWidthText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." );
        }
      }
    } );

    // fat step spinner
    final Label labelFatStepSpinner = new Label( propertyGroup, SWT.NONE );
    labelFatStepSpinner.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    labelFatStepSpinner.setText( "jede x-te Linie dicker" );

    final Spinner fatStepSpinner = new Spinner( propertyGroup, SWT.NONE );
    fatStepSpinner.setLayoutData( gridDataStepWidthText );
    fatStepSpinner.setBackground( this.getBackground() );
    fatStepSpinner.setSelection( m_fatValue );

    // bold width spinner
    final Label labelboldWidthSpinner = new Label( propertyGroup, SWT.NONE );
    labelboldWidthSpinner.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    labelboldWidthSpinner.setText( "Linenstärke (dick)" );

    final Spinner boldWidthSpinner = new Spinner( propertyGroup, SWT.NONE );
    boldWidthSpinner.setLayoutData( gridDataStepWidthText );
    boldWidthSpinner.setBackground( this.getBackground() );
    boldWidthSpinner.setSelection( m_fatWidth );
  }

  private void createEntryGroup( )
  {
    Group normalColorMapGroup = new Group( this, SWT.NONE );
    normalColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    normalColorMapGroup.setLayout( new GridLayout( 1, true ) );
    normalColorMapGroup.setText( "Liniengrundeinstellungen" );

    m_entry = m_colorMap.getColorMap()[0];
    final LineColorMapEntryEditorComposite fromEntryComposite = new LineColorMapEntryEditorComposite( normalColorMapGroup, SWT.NONE, m_entry );
    fromEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    fromEntryComposite.addModifyListener( new ILineColorMapEntryModifyListener()
    {
      public void onEntryChanged( Object source, LineColorMapEntry entry )
      {
        updateColorMap();
      }
    } );
  }

  private void createMinMaxComponent( final Composite commonComposite )
  {
    /* properties (global min / max, displayed min / max */
    final Group propertyGroup = new Group( commonComposite, SWT.NONE );
    GridData gridDataProperty = new GridData( SWT.FILL, SWT.FILL, true, true );
    gridDataProperty.horizontalSpan = 2;
    propertyGroup.setLayoutData( gridDataProperty );
    propertyGroup.setLayout( new GridLayout( 2, true ) );
    propertyGroup.setText( "Wertebereich" );

    final Composite globalComposite = new Composite( propertyGroup, SWT.NONE );
    GridData gridDataGlobalComp = new GridData( SWT.FILL, SWT.CENTER, true, false );
    globalComposite.setLayoutData( gridDataGlobalComp );
    globalComposite.setLayout( new GridLayout( 2, false ) );

    final Composite displayComposite = new Composite( propertyGroup, SWT.NONE );
    GridData gridDataDisplayComp = new GridData( SWT.FILL, SWT.CENTER, true, false );
    displayComposite.setLayoutData( gridDataDisplayComp );
    displayComposite.setLayout( new GridLayout( 2, false ) );

    final Label globalMaxLabel = new Label( globalComposite, SWT.NONE );
    final GridData gridDataGlobalMax = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    gridDataGlobalMax.heightHint = 15;
    globalMaxLabel.setLayoutData( gridDataGlobalMax );
    globalMaxLabel.setText( "maximaler Wert: " );

    final Label globalMaxValueLabel = new Label( globalComposite, SWT.NONE );
    GridData gridDataMaxValueLabel = new GridData( SWT.END, SWT.CENTER, false, false );
    gridDataMaxValueLabel.widthHint = 40;
    gridDataMaxValueLabel.heightHint = 15;

    globalMaxValueLabel.setLayoutData( gridDataMaxValueLabel );
    globalMaxValueLabel.setText( m_globalMax );
    globalMaxValueLabel.setAlignment( SWT.RIGHT );

    final Label globalMinLabel = new Label( globalComposite, SWT.NONE );
    globalMinLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    globalMinLabel.setText( "minimaler Wert: " );

    final Label globalMinValueLabel = new Label( globalComposite, SWT.NONE );
    GridData gridDataMinValueLabel = new GridData( SWT.END, SWT.CENTER, false, false );
    gridDataMinValueLabel.widthHint = 40;

    globalMinValueLabel.setLayoutData( gridDataMinValueLabel );
    globalMinValueLabel.setText( m_globalMin );
    globalMinValueLabel.setAlignment( SWT.RIGHT );

    /* max value to display */
    final Label displayMaxLabel = new Label( displayComposite, SWT.NONE );
    displayMaxLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    displayMaxLabel.setText( "maximaler angezeigter Wert: " );

    final Text maxValueText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    GridData gridDataMaxText = new GridData( SWT.END, SWT.CENTER, true, false );
    gridDataMaxText.widthHint = 30;
    gridDataMaxText.heightHint = 10;
    maxValueText.setLayoutData( gridDataMaxText );

    final String stringMax = String.valueOf( m_maxValue );
    maxValueText.setText( stringMax );

    /* min value to display */
    final Label displayMinLabel = new Label( displayComposite, SWT.NONE );
    displayMinLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    displayMinLabel.setText( "minimaler angezeigter Wert: " );

    final Text minValueText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    GridData gridDataMinText = new GridData( SWT.END, SWT.CENTER, true, false );
    gridDataMinText.widthHint = 30;
    gridDataMinText.heightHint = 10;
    minValueText.setLayoutData( gridDataMinText );

    final String stringMin = String.valueOf( m_minValue );
    minValueText.setText( stringMin );

    minValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final Double value = checkDoubleTextValue( propertyGroup, minValueText );
            if( value != null )
              m_minValue = value;
        }
      }
    } );

    minValueText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access")
      public void focusGained( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, minValueText );
        if( value != null )
          m_minValue = value;
      }

      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, minValueText );
        if( value != null )
        {
          m_minValue = value;
          updateColorMap();
        }
      }
    } );

    minValueText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        final String tempText = minValueText.getText();

        final Matcher m = m_patternDouble.matcher( tempText );

        if( !m.matches() )
        {
          minValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          minValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." );
        }
      }
    } );

    maxValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final Double value = checkDoubleTextValue( propertyGroup, maxValueText );
            if( value != null )
              m_maxValue = value;
        }
      }
    } );

    maxValueText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access")
      public void focusGained( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, maxValueText );
        if( value != null )
          m_maxValue = value;
      }

      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        final Double value = checkDoubleTextValue( propertyGroup, maxValueText );
        if( value != null )
        {
          m_maxValue = value;
          updateColorMap();
        }
      }
    } );

    maxValueText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        final String tempText = maxValueText.getText();

        final Matcher m = m_patternDouble.matcher( tempText );

        if( !m.matches() )
        {
          maxValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          maxValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." );
        }
      }
    } );
  }

  /**
   * checks the user typed string for the double value
   * 
   * @param comp
   *            composite of the text field
   * @param text
   *            the text field
   */
  private Double checkDoubleTextValue( final Composite comp, final Text text )
  {
    String tempText = text.getText();

    final Matcher m = m_patternDouble.matcher( tempText );

    if( !m.matches() )
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }
    else
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      tempText = tempText.replaceAll( ",", "." );

      Double db = new Double( tempText );
      if( db > 0 )
      {
        text.setText( db.toString() );
      }
      else
      {
        db = 0.0;
        text.setText( db.toString() );
      }

      return db;
    }
    return null;
  }

  /**
   * sets the parameters for the colormap of an isoline
   */
  protected void updateColorMap( )
  {
    final Stroke stroke = m_entry.getStroke();

    try
    {
      final Color fromColor = stroke.getStroke( null );
      final Color toColor = stroke.getStroke( null );

      final double opacity = stroke.getOpacity( null );
      final double normalWidth = stroke.getWidth( null );
      final float[] dashArray = stroke.getDashArray( null );

      final BigDecimal minDecimal = new BigDecimal( m_minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );
      final BigDecimal maxDecimal = new BigDecimal( m_maxValue ).setScale( 1, BigDecimal.ROUND_CEILING );

      final BigDecimal stepWidth = new BigDecimal( m_stepWidth ).setScale( 1, BigDecimal.ROUND_HALF_UP );
      final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( stepWidth )).intValue() + 1;

      final List<LineColorMapEntry> colorMapList = new LinkedList<LineColorMapEntry>();

      for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
      {
        final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

        Color lineColor;
        if( fromColor == toColor )
          lineColor = fromColor;
        else
          lineColor = ResultSldHelper.interpolateColor( fromColor, toColor, currentClass, numOfClasses );

        final double strokeWidth;
        if( currentValue % m_fatValue == 0 )
          strokeWidth = m_fatWidth;
        else
          strokeWidth = normalWidth;

        final Stroke newStroke = StyleFactory.createStroke( lineColor, strokeWidth );
        newStroke.setOpacity( opacity );
        newStroke.setDashArray( dashArray );

        final ParameterValueType label = StyleFactory.createParameterValueType( "Isolinie " + currentClass );
        final ParameterValueType quantity = StyleFactory.createParameterValueType( currentValue );

        final LineColorMapEntry colorMapEntry = new LineColorMapEntry_Impl( newStroke, label, quantity );
        colorMapList.add( colorMapEntry );
      }
      if( colorMapList.size() > 0 )
        m_colorMap.replaceColorMap( colorMapList );
    }
    catch( FilterEvaluationException e )
    {
      e.printStackTrace();
    }

  }
}
