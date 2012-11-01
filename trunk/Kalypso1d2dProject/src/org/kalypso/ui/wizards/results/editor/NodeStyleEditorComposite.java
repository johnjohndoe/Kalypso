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
package org.kalypso.ui.wizards.results.editor;

import java.awt.Color;
import java.math.BigDecimal;
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
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ui.editor.sldEditor.FillEditorComposite;
import org.kalypso.ui.editor.sldEditor.IFillModifyListener;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.SldHelper;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author jung
 *
 */
public abstract class NodeStyleEditorComposite extends Composite
{
  private static final Color DEFAULT_COLOR_MIN = new Color( Integer.parseInt( "ff0000", 16 ) ); //$NON-NLS-1$

  private static final Color DEFAULT_COLOR_MAX = new Color( Integer.parseInt( "0000ff", 16 ) ); //$NON-NLS-1$

  private PolygonColorMapEntry m_toEntry;

  private PolygonColorMapEntry m_fromEntry;

  private final Pattern m_patternDouble = Pattern.compile( "[\\+\\-]?[0-9]+[\\.\\,]?[0-9]*?" ); //$NON-NLS-1$

  private int m_amontOfInterpolationsClasses;

  private BigDecimal m_minValue;

  private BigDecimal m_maxValue;

  private final String m_globalMin;

  private final String m_globalMax;

  private FillEditorComposite m_fillEditor;

  public NodeStyleEditorComposite( final Composite parent, final int style, final PolygonColorMapEntry from, final PolygonColorMapEntry to, final BigDecimal minGlobalValue, final BigDecimal maxGlobalValue, final int amountClasses )
  {
    super( parent, style );

    m_fromEntry = from;
    m_toEntry = to;

    final BigDecimal globalMin = minGlobalValue == null ? new BigDecimal( m_fromEntry.getFrom( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP ) : minGlobalValue.setScale( 2, BigDecimal.ROUND_HALF_UP );
    final BigDecimal globalMax = maxGlobalValue == null ? new BigDecimal( m_toEntry.getFrom( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP ) : maxGlobalValue.setScale( 2, BigDecimal.ROUND_HALF_UP );

    m_globalMin = minGlobalValue == null ? "<Unknown>" : globalMin.toPlainString(); //$NON-NLS-1$
    m_globalMax = minGlobalValue == null ? "<Unknown>" : globalMax.toPlainString(); //$NON-NLS-1$

    m_minValue = minGlobalValue;
    m_maxValue = maxGlobalValue;
    // check if an entry is null. If that is the case, create both entries.
    if( m_fromEntry == null || m_toEntry == null )
    {
      final BigDecimal width = globalMax.subtract( globalMin ).divide( new BigDecimal( 4 ), BigDecimal.ROUND_HALF_UP ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      m_fromEntry = StyleFactory.createPolygonColorMapEntry( DEFAULT_COLOR_MIN, DEFAULT_COLOR_MIN, globalMin, globalMin.add( width ) );
      m_toEntry = StyleFactory.createPolygonColorMapEntry( DEFAULT_COLOR_MAX, DEFAULT_COLOR_MAX, globalMax.subtract( width ), globalMax );
    }

    m_minValue = new BigDecimal( m_fromEntry.getFrom( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    m_maxValue = new BigDecimal( m_toEntry.getTo( null ) ).setScale( 2, BigDecimal.ROUND_HALF_UP );

    // m_stepWidthBD = new BigDecimal( m_fromEntry.getTo( null ) - m_fromEntry.getFrom( null ) ).setScale( 2,
    // BigDecimal.ROUND_HALF_UP );

    m_amontOfInterpolationsClasses = amountClasses;

    createControl();
  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    createMinMaxGroup( this );

    final Group fromColorMapGroup = new Group( this, SWT.NONE );
    fromColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fromColorMapGroup.setLayout( new GridLayout( 1, true ) );
    fromColorMapGroup.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.3" ) ); //$NON-NLS-1$

    final Group toColorMapGroup = new Group( this, SWT.NONE );
    toColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toColorMapGroup.setLayout( new GridLayout( 1, true ) );
    toColorMapGroup.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.3" ) ); //$NON-NLS-1$

    m_fillEditor = new FillEditorComposite( this, SWT.NONE, m_fromEntry.getFill(), true );
    m_fillEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_fillEditor.addModifyListener( new IFillModifyListener()
    {
      @Override
      public void onFillChanged( final Object source, final Fill fill )
      {
        contentChanged();
      }
    } );

    m_fillEditor = new FillEditorComposite( this, SWT.NONE, m_toEntry.getFill(), true );
    m_fillEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_fillEditor.addModifyListener( new IFillModifyListener()
    {
      @Override
      public void onFillChanged( final Object source, final Fill fill )
      {
        contentChanged();
      }
    } );

  }

  private void createMinMaxGroup( final Composite commonComposite )
  {
    /* properties (global min / max, displayed min / max */
    final Group propertyGroup = new Group( commonComposite, SWT.NONE );
    final GridData gridDataProperty = new GridData( SWT.FILL, SWT.BEGINNING, true, false );
    gridDataProperty.horizontalSpan = 2;
    propertyGroup.setLayoutData( gridDataProperty );
    propertyGroup.setLayout( new GridLayout( 3, false ) );
    propertyGroup.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.6" ) ); //$NON-NLS-1$

    final Label globalMinLabel = new Label( propertyGroup, SWT.BEGINNING );
    globalMinLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, true, false ) );
    globalMinLabel.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.10", m_globalMin ) ); //$NON-NLS-1$
    globalMinLabel.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.10", m_globalMin ) ); //$NON-NLS-1$

    final Label globalMaxLabel = new Label( propertyGroup, SWT.BEGINNING );
    globalMaxLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, true, false ) );
    globalMaxLabel.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.11", m_globalMax ) ); //$NON-NLS-1$
    globalMaxLabel.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.11", m_globalMax ) ); //$NON-NLS-1$

    final Label labelWithSpinner = new Label( propertyGroup, SWT.NONE );
    labelWithSpinner.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, true, false ) );
    labelWithSpinner.setText( Messages.getString( "org.kalypso.ui.wizards.results.NodeStyleEditorComposite.12" ) ); //$NON-NLS-1$

    final Text minValueText = new Text( propertyGroup, SWT.BORDER | SWT.TRAIL );
    final GridData minValueTextData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    minValueTextData.widthHint = 100;
    minValueText.setLayoutData( minValueTextData );
    minValueText.setText( String.valueOf( m_minValue ) );
    minValueText.setEditable( false );
    minValueText.setEnabled( false );

    final Text maxValueText = new Text( propertyGroup, SWT.BORDER | SWT.TRAIL );
    final GridData maxValueTextData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    maxValueTextData.widthHint = 100;
    maxValueText.setLayoutData( maxValueTextData );
    maxValueText.setText( String.valueOf( m_maxValue ) );
    maxValueText.setEditable( false );
    maxValueText.setEnabled( false );

    final Text stepWidthText = new Text( propertyGroup, SWT.BORDER | SWT.TRAIL );
    final GridData gridDataStepWidthText = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataStepWidthText.widthHint = 100;
    stepWidthText.setLayoutData( gridDataStepWidthText );
    stepWidthText.setText( String.valueOf( m_amontOfInterpolationsClasses ) );

    minValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
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
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, minValueText, m_patternDouble );
        if( value != null )
          m_minValue = value;
      }

      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, minValueText, m_patternDouble );
        if( value != null )
        {
          m_minValue = value;
          contentChanged();
        }
      }
    } );

    minValueText.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
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
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
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
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, maxValueText, m_patternDouble );
        if( value != null )
          m_maxValue = value;
      }

      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkDoubleTextValue( propertyGroup, maxValueText, m_patternDouble );
        if( value != null )
        {
          m_maxValue = value;
          contentChanged();
        }
      }
    } );

    maxValueText.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
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

    stepWidthText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
            if( value != null )
              m_amontOfInterpolationsClasses = value.intValue();
        }
      }
    } );

    stepWidthText.addFocusListener( new FocusListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
        if( value != null )
          m_amontOfInterpolationsClasses = value.intValue();
      }

      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, stepWidthText, m_patternDouble );
        if( value != null )
        {
          m_amontOfInterpolationsClasses = value.intValue();
          contentChanged();
        }
      }
    } );

    stepWidthText.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
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

  public final int getAmountOfClassesForInterpolation( )
  {
    return m_amontOfInterpolationsClasses;
  }

  public final PolygonColorMapEntry getToEntry( )
  {
    return m_toEntry;
  }

  public final PolygonColorMapEntry getFromEntry( )
  {
    return m_fromEntry;
  }

  protected abstract void contentChanged( );

}
