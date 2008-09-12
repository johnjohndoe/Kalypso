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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.i18n.Messages;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author Thomas Jung
 */

public abstract class PolygonColorMapEditorComposite extends Composite
{
  private static final Color DEFAULT_COLOR_MIN = new Color( Integer.parseInt( "ff0000", 16 ) ); //$NON-NLS-1$

  private static final Color DEFAULT_COLOR_MAX = new Color( Integer.parseInt( "0000ff", 16 ) ); //$NON-NLS-1$

  private PolygonColorMapEntry m_toEntry;

  private PolygonColorMapEntry m_fromEntry;

  private final Pattern m_patternDouble = Pattern.compile( "[\\+\\-]?[0-9]+[\\.\\,]?[0-9]*?" ); //$NON-NLS-1$

  private boolean m_strokeChecked;

  private BigDecimal m_stepWidth;

  private BigDecimal m_minValue;

  private BigDecimal m_maxValue;

  private final BigDecimal m_globalMin;

  private final BigDecimal m_globalMax;

  public PolygonColorMapEditorComposite( final Composite parent, final int style, final PolygonColorMapEntry from, final PolygonColorMapEntry to, final BigDecimal minGlobalValue, final BigDecimal maxGlobalValue )
  {
    super( parent, style );
    m_fromEntry = from;
    m_toEntry = to;

    m_globalMin = minGlobalValue;
    m_globalMax = maxGlobalValue;

    // check if an entry is null. If that is the case, create both entries.
    if( m_fromEntry == null || m_toEntry == null )
    {
      final BigDecimal width = m_globalMax.subtract( m_globalMin ).divide( new BigDecimal( 4 ), BigDecimal.ROUND_HALF_UP ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      m_fromEntry = StyleFactory.createPolygonColorMapEntry( DEFAULT_COLOR_MIN, DEFAULT_COLOR_MIN, m_globalMin, m_globalMin.add( width ) );
      m_toEntry = StyleFactory.createPolygonColorMapEntry( DEFAULT_COLOR_MAX, DEFAULT_COLOR_MAX, m_globalMax.subtract( width ), m_globalMax );
    }

    m_minValue = new BigDecimal( m_fromEntry.getFrom( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    m_maxValue = new BigDecimal( m_toEntry.getTo( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );

    m_stepWidth = new BigDecimal( m_fromEntry.getTo( null ) - m_fromEntry.getFrom( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );

    createControl();
  }

  /**
   * creates an default PolygonColorMapEntry TODO: move to style helper classes
   */
  protected static PolygonColorMapEntry_Impl createDefaultColorMapEntry( final Color color, final BigDecimal fromValue, final BigDecimal toValue )
  {
    // fill
    final Fill defaultFillFrom = StyleFactory.createFill( color );

    // stroke
    final Stroke defaultStrokeFrom = StyleFactory.createStroke( color );

    // parameters
    final String label = String.format( "%s - %s", fromValue.toString(), toValue.toString() ); //$NON-NLS-1$

    final ParameterValueType defaultLabel = StyleFactory.createParameterValueType( label );
    final ParameterValueType defaultFrom = StyleFactory.createParameterValueType( fromValue.doubleValue() );
    final ParameterValueType defaultTo = StyleFactory.createParameterValueType( toValue.doubleValue() );

    return new PolygonColorMapEntry_Impl( defaultFillFrom, defaultStrokeFrom, defaultLabel, defaultFrom, defaultTo );
  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    createMinMaxGroup( this );

    Group fromColorMapGroup = new Group( this, SWT.NONE );
    fromColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fromColorMapGroup.setLayout( new GridLayout( 1, true ) );
    fromColorMapGroup.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.4") ); //$NON-NLS-1$

    Group toColorMapGroup = new Group( this, SWT.NONE );
    toColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toColorMapGroup.setLayout( new GridLayout( 1, true ) );
    toColorMapGroup.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.5") ); //$NON-NLS-1$

    final PolygonColorMapEntryEditorComposite fromEntryComposite = new PolygonColorMapEntryEditorComposite( fromColorMapGroup, SWT.NONE, m_fromEntry );
    fromEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fromEntryComposite.addModifyListener( new IPolygonColorMapEntryModifyListener()
    {
      public void onEntryChanged( Object source, PolygonColorMapEntry entry )
      {
        colorMapChanged();
      }
    } );

    final PolygonColorMapEntryEditorComposite toEntryComposite = new PolygonColorMapEntryEditorComposite( toColorMapGroup, SWT.NONE, m_toEntry );
    toEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toEntryComposite.addModifyListener( new IPolygonColorMapEntryModifyListener()
    {
      public void onEntryChanged( Object source, PolygonColorMapEntry entry )
      {
        colorMapChanged();
      }
    } );

  }

  private void createMinMaxGroup( final Composite commonComposite )
  {
    /* properties (global min / max, displayed min / max */
    final Group propertyGroup = new Group( commonComposite, SWT.NONE );
    GridData gridDataProperty = new GridData( SWT.FILL, SWT.FILL, true, true );
    gridDataProperty.horizontalSpan = 2;
    propertyGroup.setLayoutData( gridDataProperty );
    propertyGroup.setLayout( new GridLayout( 2, true ) );
    propertyGroup.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.6") ); //$NON-NLS-1$

    final Composite globalComposite = new Composite( propertyGroup, SWT.NONE );
    GridData gridDataGlobalComp = new GridData( SWT.FILL, SWT.FILL, true, false );
    globalComposite.setLayoutData( gridDataGlobalComp );
    globalComposite.setLayout( new GridLayout( 2, false ) );

    final Composite displayComposite = new Composite( propertyGroup, SWT.NONE );
    GridData gridDataDisplayComp = new GridData( SWT.FILL, SWT.FILL, true, false );
    displayComposite.setLayoutData( gridDataDisplayComp );
    displayComposite.setLayout( new GridLayout( 2, false ) );

    final Label globalMaxLabel = new Label( globalComposite, SWT.NONE );
    final GridData gridDataGlobalMax = new GridData( SWT.BEGINNING, SWT.UP, false, false );
    gridDataGlobalMax.heightHint = 15;
    globalMaxLabel.setLayoutData( gridDataGlobalMax );
    globalMaxLabel.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.7") ); //$NON-NLS-1$

    final Label globalMaxValueLabel = new Label( globalComposite, SWT.NONE );
    GridData gridDataMaxValueLabel = new GridData( SWT.END, SWT.UP, false, false );
    gridDataMaxValueLabel.widthHint = 40;
    gridDataMaxValueLabel.heightHint = 15;

    globalMaxValueLabel.setLayoutData( gridDataMaxValueLabel );
    globalMaxValueLabel.setText( m_globalMax.setScale( 2, BigDecimal.ROUND_HALF_UP ).toString() );
    globalMaxValueLabel.setAlignment( SWT.RIGHT );

    final Label globalMinLabel = new Label( globalComposite, SWT.NONE );
    globalMinLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, false, false ) );
    globalMinLabel.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.8") ); //$NON-NLS-1$

    final Label globalMinValueLabel = new Label( globalComposite, SWT.NONE );
    GridData gridDataMinValueLabel = new GridData( SWT.END, SWT.UP, false, false );
    gridDataMinValueLabel.widthHint = 40;

    globalMinValueLabel.setLayoutData( gridDataMinValueLabel );
    globalMinValueLabel.setText( m_globalMin.setScale( 2, BigDecimal.ROUND_HALF_UP ).toString() );
    globalMinValueLabel.setAlignment( SWT.RIGHT );

    final Label checkStroke = new Label( globalComposite, SWT.NONE );
    checkStroke.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, false, false ) );
    checkStroke.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.9") ); //$NON-NLS-1$

    final Button checkStrokeFrom = new Button( globalComposite, SWT.CHECK );
    checkStrokeFrom.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, true, false ) );

    if( m_fromEntry.getStroke() != null )
    {
      checkStrokeFrom.setSelection( true );
    }
    else
    {
      checkStrokeFrom.setSelection( false );
    }
    m_strokeChecked = checkStrokeFrom.getSelection();

    checkStrokeFrom.addSelectionListener( new SelectionAdapter()
    {

      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_strokeChecked = checkStrokeFrom.getSelection();
        colorMapChanged();
      }
    } );

    /* max value to display */
    final Label displayMaxLabel = new Label( displayComposite, SWT.NONE );
    displayMaxLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, true, false ) );
    displayMaxLabel.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.11") ); //$NON-NLS-1$

    final Text maxValueText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    GridData gridDataMaxText = new GridData( SWT.END, SWT.UP, true, false );
    gridDataMaxText.widthHint = 30;
    gridDataMaxText.heightHint = 10;
    maxValueText.setLayoutData( gridDataMaxText );

    final String stringMax = String.valueOf( m_maxValue );
    maxValueText.setText( stringMax );

    /* min value to display */
    final Label displayMinLabel = new Label( displayComposite, SWT.NONE );
    displayMinLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, true, false ) );
    displayMinLabel.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.12") ); //$NON-NLS-1$

    final Text minValueText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    GridData gridDataMinText = new GridData( SWT.END, SWT.UP, true, false );
    gridDataMinText.widthHint = 30;
    gridDataMinText.heightHint = 10;
    minValueText.setLayoutData( gridDataMinText );

    final String stringMin = String.valueOf( m_minValue );
    minValueText.setText( stringMin );

    minValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, minValueText, m_patternDouble );
            if( value != null )
              m_minValue = value;
        }
      }
    } );

    minValueText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, minValueText, m_patternDouble );
        if( value != null )
          m_minValue = value;
      }

      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, minValueText, m_patternDouble );
        if( value != null )
        {
          m_minValue = value;
          colorMapChanged();
        }
      }
    } );

    minValueText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
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
          tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    } );

    maxValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, maxValueText, m_patternDouble );
            if( value != null )
              m_maxValue = value;
        }
      }
    } );

    maxValueText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, maxValueText, m_patternDouble );
        if( value != null )
          m_maxValue = value;
      }

      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, maxValueText, m_patternDouble );
        if( value != null )
        {
          m_maxValue = value;
          colorMapChanged();
        }
      }
    } );

    maxValueText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
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
          tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    } );

    // step width spinner
    final Label labelWithSpinner = new Label( displayComposite, SWT.NONE );
    labelWithSpinner.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, true, false ) );
    labelWithSpinner.setText( Messages.getString("org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite.25") ); //$NON-NLS-1$

    final Text stepWidthText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    GridData gridDataStepWidthText = new GridData( SWT.END, SWT.UP, true, false );
    gridDataStepWidthText.widthHint = 30;
    gridDataStepWidthText.heightHint = 10;
    stepWidthText.setLayoutData( gridDataStepWidthText );

    final String stringStepWidth = String.valueOf( m_stepWidth );
    stepWidthText.setText( stringStepWidth );

    stepWidthText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
            if( value != null )
              m_stepWidth = value;
        }
      }
    } );

    stepWidthText.addFocusListener( new FocusListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
        if( value != null )
          m_stepWidth = value;
      }

      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
        if( value != null )
        {
          m_stepWidth = value;
          colorMapChanged();
        }
      }
    } );

    stepWidthText.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
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
          tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    } );

  }

  public List<PolygonColorMapEntry> getColorMap( )
  {
    return createColorMap( m_fromEntry, m_toEntry, m_stepWidth, m_minValue, m_maxValue, m_strokeChecked );
  }

  protected abstract void colorMapChanged( );

  protected static List<PolygonColorMapEntry> createColorMap( final PolygonColorMapEntry fromEntry, final PolygonColorMapEntry toEntry, final BigDecimal stepWidth, final BigDecimal minValue, final BigDecimal maxValue, final boolean useStroke )
  {
    final List<PolygonColorMapEntry> colorMapList = new LinkedList<PolygonColorMapEntry>();

    try
    {
      final Color fromPolygonColor = fromEntry.getFill().getFill( null );
      final Color toPolygonColor = toEntry.getFill().getFill( null );
      final double polygonOpacityFrom = fromEntry.getFill().getOpacity( null );
      final double polygonOpacityTo = toEntry.getFill().getOpacity( null );

      // Stroke
      final Color fromLineColor = fromEntry.getStroke().getStroke( null );
      final Color toLineColor = toEntry.getStroke().getStroke( null );
      final double lineOpacityFrom = fromEntry.getStroke().getOpacity( null );
      final double lineOpacityTo = toEntry.getStroke().getOpacity( null );
      final double lineWidthFrom = fromEntry.getStroke().getWidth( null );
      final double lineWidthTo = toEntry.getStroke().getWidth( null );

      // get rounded values below min and above max (rounded by first decimal)
      final BigDecimal minDecimal = minValue.setScale( 2, BigDecimal.ROUND_FLOOR );
      final BigDecimal maxDecimal = maxValue.setScale( 2, BigDecimal.ROUND_CEILING );

      final BigDecimal polygonStepWidth = stepWidth.setScale( 2, BigDecimal.ROUND_FLOOR );
      final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth, BigDecimal.ROUND_HALF_UP )).intValue();

      for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
      {
        final BigDecimal fromValue = new BigDecimal( minDecimal.doubleValue() + currentClass * polygonStepWidth.doubleValue() ).setScale( 2, BigDecimal.ROUND_HALF_UP );
        final BigDecimal toValue = new BigDecimal( minDecimal.doubleValue() + (currentClass + 1) * polygonStepWidth.doubleValue() ).setScale( 2, BigDecimal.ROUND_HALF_UP );

        // Stroke
        Color lineColor;
        if( fromLineColor == toLineColor )
          lineColor = fromLineColor;
        else
          lineColor = SldHelper.interpolateColor( fromLineColor, toLineColor, currentClass, numOfClasses );

        // Fill
        final Color polygonColor = SldHelper.interpolateColor( fromPolygonColor, toPolygonColor, currentClass, numOfClasses );
        double polygonOpacity = SldHelper.interpolate( polygonOpacityFrom, polygonOpacityTo, currentClass, numOfClasses );
        final Fill fill = StyleFactory.createFill( polygonColor, polygonOpacity );

        // Stroke
        BigDecimal lineOpacity;
        BigDecimal lineWidth;

        if( useStroke == false )
        {
          lineOpacity = new BigDecimal( 0 ).setScale( 2, BigDecimal.ROUND_HALF_UP );
          lineWidth = new BigDecimal( 0.01 ).setScale( 2, BigDecimal.ROUND_HALF_UP );
        }
        else
        {
          lineOpacity = new BigDecimal( SldHelper.interpolate( lineOpacityFrom, lineOpacityTo, currentClass, numOfClasses ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );
          lineWidth = new BigDecimal( SldHelper.interpolate( lineWidthFrom, lineWidthTo, currentClass, numOfClasses ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );
          if( lineWidth == new BigDecimal( 0 ) )
            lineWidth = new BigDecimal( 0.01 ).setScale( 2, BigDecimal.ROUND_HALF_UP );
        }

        final Stroke stroke = StyleFactory.createStroke( lineColor, lineWidth.doubleValue(), lineOpacity.doubleValue() );
        stroke.setLineCap( java.awt.BasicStroke.CAP_ROUND );
        stroke.setLineJoin( java.awt.BasicStroke.JOIN_ROUND );
        final String labelStr = String.format( "%.2f - %.2f", fromValue, toValue ); //$NON-NLS-1$

        final ParameterValueType label = StyleFactory.createParameterValueType( labelStr );
        final ParameterValueType from = StyleFactory.createParameterValueType( fromValue.doubleValue() );
        final ParameterValueType to = StyleFactory.createParameterValueType( toValue.doubleValue() );

        final PolygonColorMapEntry colorMapEntry = new PolygonColorMapEntry_Impl( fill, stroke, label, from, to );

        colorMapList.add( colorMapEntry );
      }
    }
    catch( FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return colorMapList;
  }

}
