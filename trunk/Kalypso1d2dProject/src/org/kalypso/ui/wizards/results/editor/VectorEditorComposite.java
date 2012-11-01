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

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.kalypso.ui.editor.sldEditor.IStrokeModifyListener;
import org.kalypso.ui.editor.sldEditor.StrokeEditorComposite;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.SldHelper;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree_impl.filterencoding.ArithmeticExpression;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author jung
 *
 */
public class VectorEditorComposite extends Composite
{
  private final Pattern m_patternDouble = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]*?" ); //$NON-NLS-1$

  private final String m_globalMin;

  private final String m_globalMax;

  private BigDecimal m_scale;

  private final Graphic m_graphic;

  private Mark m_mark;

  private Fill m_fill;

  private Stroke m_stroke;

  private final Set<IVectorModifyListener> m_listeners = new HashSet<>();

  private Literal m_firstExpression;

  private final UOM m_uom;

  private final PointSymbolizer m_symb;

  private FillEditorComposite m_fillEditor;

  public VectorEditorComposite( final Composite parent, final int style, final PointSymbolizer symb, final BigDecimal minGlobalValue, final BigDecimal maxGlobalValue )
  {
    super( parent, style );
    m_symb = symb;

    m_graphic = m_symb.getGraphic();
    m_uom = m_symb.getUom();

    m_globalMin = minGlobalValue.toString();
    m_globalMax = maxGlobalValue.toString();

    final ParameterValueType sizeParameter = m_graphic.getSizeParameter();
    final Object[] components = sizeParameter.getComponents();

    ArithmeticExpression mult = null;
    for( final Object object : components )
    {
      if( object instanceof ArithmeticExpression )
      {
        mult = (ArithmeticExpression) object;
        break;
      }
    }
    if( mult == null )
      m_scale = new BigDecimal( 10 );
    else
    {
      m_firstExpression = (Literal) mult.getFirstExpression();
      m_scale = new BigDecimal( m_firstExpression.getValue() );
    }

    final Object[] mag = m_graphic.getMarksAndExtGraphics();
    final Object object = mag[0];
    if( object instanceof Mark )
    {
      m_mark = (Mark) object;
      try
      {
        m_fill = m_mark.getFill();
      }
      catch( final Exception e )
      {
        // TODO: handle exception
      }
      m_stroke = m_mark.getStroke();
    }

    createControl();
  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    createMinMaxGroup( this );

    createStrokeFillControl( this );
  }

  private void createStrokeFillControl( final Composite commonComposite )
  {
    final Group graphicGroup = new Group( commonComposite, SWT.NONE );
    final GridData gridDataProperty = new GridData( SWT.FILL, SWT.FILL, true, true );
    gridDataProperty.horizontalSpan = 2;
    graphicGroup.setLayoutData( gridDataProperty );
    graphicGroup.setLayout( new GridLayout( 2, true ) );
    graphicGroup.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.1") ); //$NON-NLS-1$

    final Group strokeColorMapGroup = new Group( graphicGroup, SWT.NONE );
    strokeColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    strokeColorMapGroup.setLayout( new GridLayout( 1, true ) );
    strokeColorMapGroup.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.2") ); //$NON-NLS-1$

    final Group fillColorMapGroup = new Group( graphicGroup, SWT.NONE );
    fillColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    fillColorMapGroup.setLayout( new GridLayout( 1, true ) );
    fillColorMapGroup.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.3") ); //$NON-NLS-1$

    final StrokeEditorComposite strokeEditor = new StrokeEditorComposite( strokeColorMapGroup, SWT.NONE, m_stroke, false );
    strokeEditor.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    strokeEditor.addModifyListener( new IStrokeModifyListener()
    {
      @Override
      public void onStrokeChanged( final Object source, final Stroke stroke )
      {
        contentChanged();
      }
    } );

    m_fillEditor = new FillEditorComposite( fillColorMapGroup, SWT.NONE, m_fill, false );
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

  private void createUomControl( final Composite comp )
  {
    /* uom type combo */
    // combo text
    final Label comboTextLabel = new Label( comp, SWT.NONE );
    comboTextLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    comboTextLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.4") ); //$NON-NLS-1$

    final ComboViewer uomTypeCombo = new ComboViewer( comp, SWT.READ_ONLY );
    final GridData comboGridData = new GridData( SWT.END, SWT.CENTER, false, false );
    comboGridData.widthHint = 25;
    uomTypeCombo.getControl().setLayoutData( comboGridData );
    uomTypeCombo.setContentProvider( new ArrayContentProvider() );

    final String[] types = new String[2];
    types[0] = "Meter"; //$NON-NLS-1$
    types[1] = "Pixel"; //$NON-NLS-1$

    uomTypeCombo.setInput( types );
    if( m_uom == UOM.pixel )
      uomTypeCombo.setSelection( new StructuredSelection( uomTypeCombo.getElementAt( 1 ) ) );
    else
      uomTypeCombo.setSelection( new StructuredSelection( uomTypeCombo.getElementAt( 0 ) ) );

    uomTypeCombo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {

        return super.getText( element );
      }
    } );

    // selection listener
    uomTypeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {

      @Override
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object element = selection.getFirstElement();

        final String string = (String) element;

        // TODO: get the GraphicFill from a GraphicFill editor.
        // right now, there is just possible a plain fill .

        if( string == "Meter" ) //$NON-NLS-1$
        {
          m_symb.setUom( UOM.meter );
        }
        else if( string == "Pixel" ) //$NON-NLS-1$
        {
          m_symb.setUom( UOM.pixel );
        }

        contentChanged();
      }
    } );
  }

  private void createMinMaxGroup( final Composite commonComposite )
  {
    /* properties (global min / max, displayed min / max */
    final Group propertyGroup = new Group( commonComposite, SWT.NONE );
    final GridData gridDataProperty = new GridData( SWT.FILL, SWT.FILL, true, true );
    gridDataProperty.horizontalSpan = 2;
    propertyGroup.setLayoutData( gridDataProperty );
    propertyGroup.setLayout( new GridLayout( 2, true ) );
    propertyGroup.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.9") ); //$NON-NLS-1$

    final Composite globalComposite = new Composite( propertyGroup, SWT.NONE );
    final GridData gridDataGlobalComp = new GridData( SWT.FILL, SWT.FILL, true, false );
    globalComposite.setLayoutData( gridDataGlobalComp );
    globalComposite.setLayout( new GridLayout( 2, false ) );

    final Composite displayComposite = new Composite( propertyGroup, SWT.NONE );
    final GridData gridDataDisplayComp = new GridData( SWT.FILL, SWT.FILL, true, false );
    displayComposite.setLayoutData( gridDataDisplayComp );
    displayComposite.setLayout( new GridLayout( 2, false ) );

    final Label globalMaxLabel = new Label( globalComposite, SWT.NONE );
    final GridData gridDataGlobalMax = new GridData( SWT.BEGINNING, SWT.UP, false, false );
    gridDataGlobalMax.heightHint = 15;
    globalMaxLabel.setLayoutData( gridDataGlobalMax );
    globalMaxLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.10") ); //$NON-NLS-1$

    final Label globalMaxValueLabel = new Label( globalComposite, SWT.NONE );
    final GridData gridDataMaxValueLabel = new GridData( SWT.END, SWT.UP, false, false );
    gridDataMaxValueLabel.widthHint = 40;
    gridDataMaxValueLabel.heightHint = 15;

    globalMaxValueLabel.setLayoutData( gridDataMaxValueLabel );
    globalMaxValueLabel.setText( m_globalMax );
    globalMaxValueLabel.setAlignment( SWT.RIGHT );

    final Label globalMinLabel = new Label( globalComposite, SWT.NONE );
    globalMinLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, false, false ) );
    globalMinLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.11") ); //$NON-NLS-1$

    final Label globalMinValueLabel = new Label( globalComposite, SWT.NONE );
    final GridData gridDataMinValueLabel = new GridData( SWT.END, SWT.UP, false, false );
    gridDataMinValueLabel.widthHint = 40;

    globalMinValueLabel.setLayoutData( gridDataMinValueLabel );
    globalMinValueLabel.setText( m_globalMin );
    globalMinValueLabel.setAlignment( SWT.RIGHT );

    /* scale value to display */
    final Label displayScaleLabel = new Label( displayComposite, SWT.NONE );
    displayScaleLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.UP, true, false ) );
    displayScaleLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.VectorEditorComposite.12") ); //$NON-NLS-1$

    final Text scaleValueText = new Text( displayComposite, SWT.BORDER | SWT.TRAIL );
    final GridData gridDataMaxText = new GridData( SWT.END, SWT.UP, true, false );
    gridDataMaxText.widthHint = 40;
    gridDataMaxText.heightHint = 10;
    scaleValueText.setLayoutData( gridDataMaxText );

    scaleValueText.setText( m_scale.toString() );

    scaleValueText.addKeyListener( new KeyAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, scaleValueText, m_patternDouble );
            if( value != null )
            {
              m_scale = value;
              m_firstExpression.setValue( value.toString() );
              contentChanged();
            }
        }
      }
    } );

    scaleValueText.addFocusListener( new FocusListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void focusGained( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, scaleValueText, m_patternDouble );
        if( value != null )
        {
          m_scale = value;
          m_firstExpression.setValue( value.toString() );
        }
      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        final BigDecimal value = SldHelper.checkPositiveDoubleTextValue( propertyGroup, scaleValueText, m_patternDouble );
        if( value != null )
        {
          m_firstExpression.setValue( value.toString() );
          contentChanged();
        }
      }
    } );

    scaleValueText.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        final String tempText = scaleValueText.getText();

        final Matcher m = m_patternDouble.matcher( tempText );

        if( !m.matches() )
        {
          scaleValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        }
        else
        {
          scaleValueText.setBackground( propertyGroup.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
          tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
          m_firstExpression.setValue( tempText );
          contentChanged();
        }
      }
    } );

    createUomControl( displayComposite );

  }

  protected void disposeControl( )
  {
    m_listeners.clear();
  }

  /**
   * Add the listener to the list of listeners. If an identical listeners has already been registered, this has no
   * effect.
   */
  public void addModifyListener( final IVectorModifyListener l )
  {
    m_listeners.add( l );
  }

  public void removeModifyListener( final IVectorModifyListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireModified( )
  {
    final IVectorModifyListener[] ls = m_listeners.toArray( new IVectorModifyListener[m_listeners.size()] );
    for( final IVectorModifyListener graphicModifyListener : ls )
      graphicModifyListener.onGraphicChanged( this, m_graphic );
  }

  protected void contentChanged( )
  {
    fireModified();
  }

}
