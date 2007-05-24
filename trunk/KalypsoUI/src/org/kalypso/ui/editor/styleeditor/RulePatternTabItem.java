/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/*
 * Created on 12.07.2004
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;
import org.kalypso.ui.editor.styleeditor.panels.AddFilterPropertyPanel;
import org.kalypso.ui.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.DenominatorInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.EditSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.RulePatternInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author F.Lindemann
 */
public class RulePatternTabItem
{
  private TabFolder ruleTabFolder = null;

  private KalypsoUserStyle userStyle = null;

  private IFeatureType featureType = null;

  private int focusedRuleItem = -1;

  private int focusedSymbolizerItem = -1;

  private RuleFilterCollection rulePatternCollection = null;

  private String[] numericFeatureTypePropertylist = null;

  public RulePatternTabItem( final TabFolder m_ruleTabFolder, final KalypsoUserStyle m_userStyle, final IFeatureType m_featureType, final RuleFilterCollection m_rulePatternCollection, final ArrayList m_numericFeatureTypePropertylist )
  {
    this.ruleTabFolder = m_ruleTabFolder;
    setUserStyle( m_userStyle );
    setFeatureType( m_featureType );
    setRulePatternCollection( m_rulePatternCollection );
    setNumericFeatureTypePropertylist( m_numericFeatureTypePropertylist );
  }

  double minValue = -1;

  double maxValue = -1;

  double step = -1;

  public void drawPatternRule( final RuleCollection ruleCollection, final int index )
  {

    if( ruleCollection.size() == 0 )
      return;

    final Rule tmpRule = ruleCollection.get( 0 );
    // PatternRule only possible for PropertyIsBetweenOperation
    if( tmpRule.getFilter() == null || !((((ComplexFilter) tmpRule.getFilter()).getOperation()) instanceof PropertyIsBetweenOperation) )
    {
      return;
    }

    // 1. get global values for name, minDen, maxDen,
    String rulePatternName = tmpRule.getTitle();
    if( rulePatternName == null || rulePatternName.trim().length() == 0 )
    {
      rulePatternName = MessageBundle.STYLE_EDITOR_SET_VALUE;
      tmpRule.setTitle( rulePatternName );
    }
    final double rulePatternMinDenom = tmpRule.getMinScaleDenominator();
    final double rulePatternMaxDenom = tmpRule.getMaxScaleDenominator();

    // 2. Begin to draw the first lines
    final TabItem tabItem = new TabItem( ruleTabFolder, SWT.NULL );
    final Composite composite = new Composite( ruleTabFolder, SWT.NULL );
    final GridLayout compositeLayout = new GridLayout();
    composite.setSize( 270, 230 );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 5;
    compositeLayout.marginHeight = 5;
    composite.layout();
    tabItem.setControl( composite );
    tabItem.setText( MessageBundle.STYLE_EDITOR_PATTERN + rulePatternName );

    final TabFolder symbolizerTabFolder;
    RulePatternInputPanel rulePatternInputPanel = null;

    final TextInputPanel titleInputPanel = new TextInputPanel( composite, MessageBundle.STYLE_EDITOR_TITLE, rulePatternName );
    titleInputPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final String name = ((TextInputPanel) event.getSource()).getLabelText();
        if( name == null || name.trim().length() == 0 )
        {
          final StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT, MessageBundle.STYLE_EDITOR_ERROR_NO_TITLE );
          errorDialog.showError();
        }
        else
        {
          for( int counter6 = 0; counter6 < ruleCollection.size(); counter6++ )
          {
            ruleCollection.get( counter6 ).setTitle( name );
          }
          getUserStyle().fireStyleChanged();
        }
        tabItem.setText( MessageBundle.STYLE_EDITOR_PATTERN + " " + name );
        setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
      }
    } );

    final DenominatorInputPanel minDenominatorPanel = new DenominatorInputPanel( composite, MessageBundle.STYLE_EDITOR_MIN_DENOM, rulePatternMinDenom );
    minDenominatorPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final double min = ((DenominatorInputPanel) event.getSource()).getDenominator();
        final double max = tmpRule.getMaxScaleDenominator();
        // verify that min<=max
        if( min > max )
        {
          final StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT, MessageBundle.STYLE_EDITOR_ERROR_MIN_DENOM_BIG );
          errorDialog.showError();
          minDenominatorPanel.setDenominator( tmpRule.getMinScaleDenominator() );
        }
        else
        {
          for( int counter7 = 0; counter7 < ruleCollection.size(); counter7++ )
          {
            ruleCollection.get( counter7 ).setMinScaleDenominator( min );
            final Symbolizer symbolizers[] = ruleCollection.get( counter7 ).getSymbolizers();
            for( final Symbolizer element : symbolizers )
            {
              element.setMinScaleDenominator( min );
            }
          }
          getUserStyle().fireStyleChanged();
        }
        setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
      }
    } );

    // max denominator cannot be 0.0 as this would imply that the min
    // denominator needs to be smaller than 0.0 -> does not make sense
    // hence, if no max denomiator specified, get the denominator of the
    // individiual symbolizer
    if( tmpRule.getMaxScaleDenominator() == 0.0 )
    {
      if( tmpRule.getSymbolizers().length > 0 )
        tmpRule.setMaxScaleDenominator( tmpRule.getSymbolizers()[0].getMaxScaleDenominator() );
      else
        tmpRule.setMaxScaleDenominator( Double.MAX_VALUE );
    }
    final DenominatorInputPanel maxDenominatorPanel = new DenominatorInputPanel( composite, MessageBundle.STYLE_EDITOR_MAX_DENOM, rulePatternMaxDenom );
    maxDenominatorPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        double max = ((DenominatorInputPanel) event.getSource()).getDenominator();
        final double min = tmpRule.getMinScaleDenominator();
        // verify that min<=max
        if( min > max )
        {
          final StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT, MessageBundle.STYLE_EDITOR_ERROR_MAX_DENOM_SMALL );
          errorDialog.showError();
          maxDenominatorPanel.setDenominator( tmpRule.getMaxScaleDenominator() );
        }
        else
        {
          // add a minimum to max in order to be a little bit larger than the
          // current scale and
          // to keep the current view -> otherwise the rule would automatically
          // exculde this configuration
          max += 0.01;
          for( int counter8 = 0; counter8 < ruleCollection.size(); counter8++ )
          {
            ruleCollection.get( counter8 ).setMaxScaleDenominator( max );
            final Symbolizer symbolizers[] = ruleCollection.get( counter8 ).getSymbolizers();
            for( final Symbolizer element : symbolizers )
            {
              element.setMaxScaleDenominator( max );
            }
          }
          getUserStyle().fireStyleChanged();
        }
        setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
      }
    } );

    final AddFilterPropertyPanel addFilterPropertyPanel = new AddFilterPropertyPanel( composite, MessageBundle.STYLE_EDITOR_FILTER_PROPERTY, getNumericFeatureTypePropertylist() );
    // necessary if focus had been changed and rule-pattern is redrawn
    addFilterPropertyPanel.setSelection( ((PropertyIsBetweenOperation) ((ComplexFilter) tmpRule.getFilter()).getOperation()).getPropertyName().getValue() );
    // if numeric Property selection for Filter has changed -> need to change it
    // for every rule of the pattern
    addFilterPropertyPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final String filterPropertyName = addFilterPropertyPanel.getSelection();
        for( int i = 0; i < ruleCollection.size(); i++ )
        {
          final ComplexFilter filter = (ComplexFilter) ruleCollection.get( i ).getFilter();
          final PropertyIsBetweenOperation oldOperation = (PropertyIsBetweenOperation) filter.getOperation();
          final PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation( new PropertyName( filterPropertyName ), oldOperation.getLowerBoundary(), oldOperation.getUpperBoundary() );
          ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
        }
        getUserStyle().fireStyleChanged();
      }
    } );

    final AddSymbolizerPanel addSymbolizerPanel = new AddSymbolizerPanel( composite, MessageBundle.STYLE_EDITOR_SYMBOLIZER, featureType, false );

    // 3. getFilterType -> at the moment we assume only a pattern of
    // PropertyIsBetween
    // draw the pattern line
    final Filter filter = tmpRule.getFilter();
    // must be a complex filter -> then we can find out what operation-id is has
    if( filter instanceof ComplexFilter )
    {
      // if PropertyIsBetween
      if( ((ComplexFilter) filter).getOperation().getOperatorId() == OperationDefines.PROPERTYISBETWEEN )
      {
        // find out the settings of the filter - min, max and step values

        for( int j = 0; j < ruleCollection.size(); j++ )
        {
          // verify again that it is a complexFilter and of type PropertyIs
          // Between for every rule
          if( ruleCollection.get( j ).getFilter() instanceof ComplexFilter )
          {
            final Operation ruleOperation = ((ComplexFilter) ruleCollection.get( j ).getFilter()).getOperation();
            if( ruleOperation.getOperatorId() == OperationDefines.PROPERTYISBETWEEN )
            {
              final PropertyIsBetweenOperation isBetweenOperation = (PropertyIsBetweenOperation) ruleOperation;
              if( j == 0 )
              {
                minValue = Double.parseDouble( ((BoundaryExpression) isBetweenOperation.getLowerBoundary()).getValue() );
                maxValue = Double.parseDouble( ((BoundaryExpression) isBetweenOperation.getUpperBoundary()).getValue() );
                step = maxValue - minValue;
              }
              else
              {
                final double tmpMinValue = Double.parseDouble( ((BoundaryExpression) isBetweenOperation.getLowerBoundary()).getValue() );
                final double tmpMaxValue = Double.parseDouble( ((BoundaryExpression) isBetweenOperation.getUpperBoundary()).getValue() );
                if( tmpMinValue < minValue )
                  minValue = tmpMinValue;
                if( tmpMaxValue > maxValue )
                  maxValue = tmpMaxValue;
              }
            }
          }
        }
        rulePatternInputPanel = new RulePatternInputPanel( composite, MessageBundle.STYLE_EDITOR_PATTERN, minValue, maxValue, step );
      }
    }
    else
      return;

    final EditSymbolizerPanel editSymbolizerPanel = new EditSymbolizerPanel( composite, tmpRule.getSymbolizers().length );

    symbolizerTabFolder = new TabFolder( composite, SWT.NULL );

    addSymbolizerPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final Symbolizer symbolizer = ((AddSymbolizerPanel) event.getSource()).getSelection();
        if( symbolizer != null )
        {
          for( int i = 0; i < ruleCollection.size(); i++ )
          {
            final Symbolizer[] symb = { symbolizer };
            ruleCollection.get( i ).addSymbolizer( cloneSymbolizer( symb )[0] );
          }
          getUserStyle().fireStyleChanged();
          setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
          editSymbolizerPanel.update( ruleCollection.get( 0 ).getSymbolizers().length );
          drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
          symbolizerTabFolder.setSelection( ruleCollection.get( 0 ).getSymbolizers().length - 1 );
        }
      }
    } );

    if( rulePatternInputPanel != null )
    {
      rulePatternInputPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( final PanelEvent event )
        {
          final RulePatternInputPanel panel = (RulePatternInputPanel) event.getSource();
          // reset the values for all rules in this pattern if step did not
          // change !!!!
          minValue = panel.getMin();
          maxValue = panel.getMax();
          step = panel.getStep();

          // first create new rules
          BoundaryExpression upperBoundary = null;
          BoundaryExpression lowerBoundary = null;
          final ArrayList<Rule> ruleList = new ArrayList<Rule>();
          final PropertyName propertyName = new PropertyName( addFilterPropertyPanel.getSelection() );
          PropertyIsBetweenOperation operation = null;

          // only need to take first rule and duplicate it
          // plus apply the pattern
          // there needs to be at least one rule, otherwise no pattern rule
          // visible !!!!
          final int patternRuleNumber = (int) Math.ceil( (maxValue - minValue) / step );
          final Symbolizer[] symbolizer = tmpRule.getSymbolizers();

          // first add those that are existing and are to be kept
          final int currentSize = ruleCollection.size();

          if( patternRuleNumber <= currentSize )
          {
            for( int i = 0; i < patternRuleNumber; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + (minValue + (i * step)) );
              if( (minValue + ((i + 1) * step)) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + (minValue + ((i + 1) * step)) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
              ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
              ruleList.add( ruleCollection.get( i ) );
            }
          }
          else if( patternRuleNumber > currentSize )
          {
            for( int i = 0; i < currentSize; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + (minValue + (i * step)) );
              if( (minValue + ((i + 1) * step)) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + (minValue + ((i + 1) * step)) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
              ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
              ruleList.add( ruleCollection.get( i ) );
            }
            for( int i = currentSize; i < patternRuleNumber; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + (minValue + (i * step)) );
              if( (minValue + ((i + 1) * step)) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + (minValue + ((i + 1) * step)) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
              ruleList.add( StyleFactory.createRule( cloneSymbolizer( symbolizer ), tmpRule.getName(), "-name-" + i, "abstract", null, new ComplexFilter( operation ), false, tmpRule.getMinScaleDenominator(), tmpRule.getMaxScaleDenominator() ) );
            }
          }

          // then remove old ones
          final int collSize = ruleCollection.size() - 1;
          for( int i = collSize; i >= 0; i-- )
          {
            removeRule( ruleCollection.get( i ), getUserStyle() );
            getRulePatternCollection().removeRule( ruleCollection.get( i ) );
          }

          // add new ones
          for( int j = 0; j < ruleList.size(); j++ )
          {
            getRulePatternCollection().addRule( ruleList.get( j ) );
            getUserStyle().getFeatureTypeStyles()[0].addRule( ruleList.get( j ) );
          }
          // update
          drawSymbolizerTabItems( tmpRule, symbolizerTabFolder, ruleCollection );
          getUserStyle().fireStyleChanged();
        }
      } );
    }
    editSymbolizerPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final int action = ((EditSymbolizerPanel) event.getSource()).getAction();

        if( action == EditSymbolizerPanel.REM_SYMB )
        {
          final int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 >= 0 )
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              final Symbolizer s[] = ruleCollection.get( i ).getSymbolizers();
              ruleCollection.get( i ).removeSymbolizer( s[index1] );
            }
            symbolizerTabFolder.getItem( index1 ).dispose();
            setFocusedSymbolizerItem( index1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireStyleChanged();
          }
          drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
          symbolizerTabFolder.setSelection( index1 - 1 );
        }
        else if( action == EditSymbolizerPanel.FOR_SYMB )
        {
          final int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 == (ruleCollection.get( 0 ).getSymbolizers().length - 1) || index1 < 0 )
          {
            // nothing
          }
          else
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              final Symbolizer newOrderedObjects[] = new Symbolizer[ruleCollection.get( i ).getSymbolizers().length];
              for( int counter4 = 0; counter4 < ruleCollection.get( i ).getSymbolizers().length; counter4++ )
              {
                if( counter4 == index1 )
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4 + 1];
                else if( counter4 == (index1 + 1) )
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4 - 1];
                else
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4];
              }
              ruleCollection.get( i ).setSymbolizers( newOrderedObjects );
            }
            setFocusedSymbolizerItem( index1 + 1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireStyleChanged();
            drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
            symbolizerTabFolder.setSelection( index1 + 1 );
          }
        }
        else if( action == EditSymbolizerPanel.BAK_SYMB )
        {
          final int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 > 0 )
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              final Symbolizer newOrderedObjects[] = new Symbolizer[ruleCollection.get( i ).getSymbolizers().length];
              for( int counter5 = 0; counter5 < ruleCollection.get( i ).getSymbolizers().length; counter5++ )
              {
                if( counter5 == index1 )
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5 - 1];
                else if( counter5 == (index1 - 1) )
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5 + 1];
                else
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5];
              }
              ruleCollection.get( i ).setSymbolizers( newOrderedObjects );
            }
            setFocusedSymbolizerItem( index1 - 1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireStyleChanged();
            drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
            symbolizerTabFolder.setSelection( index1 - 1 );
          }
        }
      }
    } );

    // ******* DISPLAY ALL symbolizers
    drawSymbolizerTabItems( tmpRule, symbolizerTabFolder, ruleCollection );

    focusedRuleItem = index;
    composite.pack( true );
  }

  void drawSymbolizerTabItems( final Rule rule, final TabFolder symbolizerTabFolder, final RuleCollection ruleCollection )
  {
    // remove all existing items from tab folder
    final TabItem[] items = symbolizerTabFolder.getItems();
    for( int i = 0; i < items.length; i++ )
    {
      items[i].dispose();
      items[i] = null;
    }

    if( rule.getSymbolizers().length == 0 )
    {
      // add dummy invisilbe placeholder
      new FilterPatternSymbolizerTabItemBuilder( symbolizerTabFolder, null, userStyle, ruleCollection, -1 );
      symbolizerTabFolder.setVisible( false );
    }
    else
    {
      for( int j = 0; j < rule.getSymbolizers().length; j++ )
      {
        new FilterPatternSymbolizerTabItemBuilder( symbolizerTabFolder, rule.getSymbolizers()[j], userStyle, ruleCollection, j );
      }
      symbolizerTabFolder.pack();
      symbolizerTabFolder.setSize( 224, 259 );
      symbolizerTabFolder.setVisible( true );
    }
  }

  Symbolizer[] cloneSymbolizer( final Symbolizer[] symbolizers )
  {
    final Symbolizer[] returnArray = new Symbolizer[symbolizers.length];
    for( int i = 0; i < symbolizers.length; i++ )
    {
      final String geomPropertyName = symbolizers[i].getGeometry().getPropertyName();
      if( symbolizers[i] instanceof PointSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Point", getFeatureType() );
      }
      else if( symbolizers[i] instanceof LineSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Line", getFeatureType() );
      }
      else if( symbolizers[i] instanceof TextSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Text", getFeatureType() );
      }
      else if( symbolizers[i] instanceof PolygonSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Polygon", getFeatureType() );
      }
      else
        return null;
    }
    return returnArray;
  }

  void removeRule( final Rule rule, final UserStyle style )
  {
    final FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    fts[0].removeRule( rule );
  }

  public int getFocusedRuleItem( )
  {
    return focusedRuleItem;
  }

  public void setFocusedRuleItem( final int m_focusedRuleItem )
  {
    this.focusedRuleItem = m_focusedRuleItem;
  }

  public TabFolder getRuleTabFolder( )
  {
    return ruleTabFolder;
  }

  public void setRuleTabFolder( final TabFolder m_ruleTabFolder )
  {
    this.ruleTabFolder = m_ruleTabFolder;
  }

  public int getFocusedSymbolizerItem( )
  {
    return focusedSymbolizerItem;
  }

  public void setFocusedSymbolizerItem( final int m_focusedSymbolizerItem )
  {
    this.focusedSymbolizerItem = m_focusedSymbolizerItem;
  }

  public RuleFilterCollection getRulePatternCollection( )
  {
    return rulePatternCollection;
  }

  public void setRulePatternCollection( final RuleFilterCollection m_rulePatternCollection )
  {
    this.rulePatternCollection = m_rulePatternCollection;
  }

  public IFeatureType getFeatureType( )
  {
    return featureType;
  }

  public void setFeatureType( final IFeatureType m_featureType )
  {
    this.featureType = m_featureType;
  }

  public String[] getNumericFeatureTypePropertylist( )
  {
    return numericFeatureTypePropertylist;
  }

  public void setNumericFeatureTypePropertylist( final ArrayList m_numericFeatureTypePropertylist )
  {
    final String[] tmpList = new String[m_numericFeatureTypePropertylist.size()];
    for( int i = 0; i < m_numericFeatureTypePropertylist.size(); i++ )
      tmpList[i] = ((IPropertyType) m_numericFeatureTypePropertylist.get( i )).getName();
    this.numericFeatureTypePropertylist = tmpList;
  }

  public KalypsoUserStyle getUserStyle( )
  {
    return userStyle;
  }

  public void setUserStyle( final KalypsoUserStyle m_userStyle )
  {
    this.userStyle = m_userStyle;
  }
}