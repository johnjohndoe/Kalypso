/*
 * Created on 12.07.2004
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;

import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.BoundaryExpression;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
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

/**
 * @author F.Lindemann
 *  
 */
public class RulePatternTabItem
{
  private TabFolder ruleTabFolder = null;

  private KalypsoUserStyle userStyle = null;

  private FeatureType featureType = null;

  private int focusedRuleItem = -1;

  private int focusedSymbolizerItem = -1;

  private RuleFilterCollection rulePatternCollection = null;

  private String[] numericFeatureTypePropertylist = null;

  public RulePatternTabItem( TabFolder m_ruleTabFolder, KalypsoUserStyle m_userStyle,
      FeatureType m_featureType, RuleFilterCollection m_rulePatternCollection,
      ArrayList m_numericFeatureTypePropertylist )
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

  public void drawPatternRule( final RuleCollection ruleCollection, int index )
  {

    if( ruleCollection.size() == 0 )
      return;

    final Rule tmpRule = ruleCollection.get( 0 );
    // PatternRule only possible for PropertyIsBetweenOperation
    if( tmpRule.getFilter() == null
        || !( ( ( (ComplexFilter)tmpRule.getFilter() ).getOperation() ) instanceof PropertyIsBetweenOperation ) )
    {
      return;
    }

    // 1. get global values for name, minDen, maxDen,
    String rulePatternName = tmpRule.getTitle();
    if( rulePatternName == null || rulePatternName.trim().length() == 0 )
    {
      rulePatternName = "set title";
      tmpRule.setTitle( rulePatternName );
    }
    double rulePatternMinDenom = tmpRule.getMinScaleDenominator();
    double rulePatternMaxDenom = tmpRule.getMaxScaleDenominator();

    // 2. Begin to draw the first lines
    final TabItem tabItem = new TabItem( ruleTabFolder, SWT.NULL );
    final Composite composite = new Composite( ruleTabFolder, SWT.NULL );
    GridLayout compositeLayout = new GridLayout();
    composite.setSize( 270, 230 );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 5;
    compositeLayout.marginHeight = 5;
    composite.layout();
    tabItem.setControl( composite );
    tabItem.setText( "Pattern:" + rulePatternName );

    final TabFolder symbolizerTabFolder;
    RulePatternInputPanel rulePatternInputPanel = null;

    TextInputPanel titleInputPanel = new TextInputPanel( composite, "Title:", rulePatternName );
    titleInputPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        String name = ( (TextInputPanel)event.getSource() ).getLabelText();
        if( name == null || name.trim().length() == 0 )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(),
              "Invalid input value", "Title should not be null" );
          errorDialog.showError();
        }
        else
        {
          for( int counter6 = 0; counter6 < ruleCollection.size(); counter6++ )
          {
            ruleCollection.get( counter6 ).setTitle( name );
          }
          getUserStyle().fireModellEvent(
              new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
        }
        tabItem.setText( "Pattern:" + name );
        setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
      }
    } );

    final DenominatorInputPanel minDenominatorPanel = new DenominatorInputPanel( composite,
        "MinDenom:", rulePatternMinDenom );
    minDenominatorPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double min = ( (DenominatorInputPanel)event.getSource() ).getDenominator();
        double max = tmpRule.getMaxScaleDenominator();
        // verify that min<=max
        if( min > max )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(),
              "Invalid value for MinDenominator",
              "MinDenominator cannot be larger then MaxDenominator" );
          errorDialog.showError();
          minDenominatorPanel.setDenominator( tmpRule.getMinScaleDenominator() );
        }
        else
        {
          for( int counter7 = 0; counter7 < ruleCollection.size(); counter7++ )
          {
            ruleCollection.get( counter7 ).setMinScaleDenominator( min );
            Symbolizer symbolizers[] = ruleCollection.get( counter7 ).getSymbolizers();
            for( int i = 0; i < symbolizers.length; i++ )
            {
              symbolizers[i].setMinScaleDenominator( min );
            }
          }
          getUserStyle().fireModellEvent(
              new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
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
    final DenominatorInputPanel maxDenominatorPanel = new DenominatorInputPanel( composite,
        "MaxDenom:", rulePatternMaxDenom );
    maxDenominatorPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double max = ( (DenominatorInputPanel)event.getSource() ).getDenominator();
        double min = tmpRule.getMinScaleDenominator();
        // verify that min<=max
        if( min > max )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( composite.getShell(),
              "Invalid value for MaxDenominator",
              "MaxDenominator needs to be larger then MinDenominator" );
          errorDialog.showError();
          maxDenominatorPanel.setDenominator( tmpRule.getMaxScaleDenominator() );
        }
        else
        {
          //add a minimum to max in order to be a little bit larger than the
          // current scale and
          // to keep the current view -> otherwise the rule would automatically
          // exculde this configuration
          max += 0.01;
          for( int counter8 = 0; counter8 < ruleCollection.size(); counter8++ )
          {
            ruleCollection.get( counter8 ).setMaxScaleDenominator( max );
            Symbolizer symbolizers[] = ruleCollection.get( counter8 ).getSymbolizers();
            for( int i = 0; i < symbolizers.length; i++ )
            {
              symbolizers[i].setMaxScaleDenominator( max );
            }
          }
          getUserStyle().fireModellEvent(
              new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
        }
        setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
      }
    } );

    final AddFilterPropertyPanel addFilterPropertyPanel = new AddFilterPropertyPanel( composite,
        "FilterProperty", getNumericFeatureTypePropertylist() );
    // necessary if focus had been changed and rule-pattern is redrawn
    addFilterPropertyPanel.setSelection( ( (PropertyIsBetweenOperation)( (ComplexFilter)tmpRule
        .getFilter() ).getOperation() ).getPropertyName().getValue() );
    // if numeric Property selection for Filter has changed -> need to change it
    // for every rule of the pattern
    addFilterPropertyPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        String filterPropertyName = addFilterPropertyPanel.getSelection();
        for( int i = 0; i < ruleCollection.size(); i++ )
        {
          ComplexFilter filter = (ComplexFilter)ruleCollection.get( i ).getFilter();
          PropertyIsBetweenOperation oldOperation = (PropertyIsBetweenOperation)filter
              .getOperation();
          PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation( new PropertyName(
              filterPropertyName ), oldOperation.getLowerBoundary(), oldOperation
              .getUpperBoundary() );
          ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
        }
        getUserStyle()
            .fireModellEvent( new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
      }
    } );

    AddSymbolizerPanel addSymbolizerPanel = new AddSymbolizerPanel( composite, "Symbolizer",
        featureType, false );

    // 3. getFilterType -> at the moment we assume only a pattern of
    // PropertyIsBetween
    // draw the pattern line
    Filter filter = tmpRule.getFilter();
    // must be a complex filter -> then we can find out what operation-id is has
    if( filter instanceof ComplexFilter )
    {
      // if PropertyIsBetween
      if( ( (ComplexFilter)filter ).getOperation().getOperatorId() == OperationDefines.PROPERTYISBETWEEN )
      {
        // find out the settings of the filter - min, max and step values

        for( int j = 0; j < ruleCollection.size(); j++ )
        {
          // verify again that it is a complexFilter and of type PropertyIs
          // Between for every rule
          if( ruleCollection.get( j ).getFilter() instanceof ComplexFilter )
          {
            Operation ruleOperation = ( (ComplexFilter)ruleCollection.get( j ).getFilter() )
                .getOperation();
            if( ruleOperation.getOperatorId() == OperationDefines.PROPERTYISBETWEEN )
            {
              PropertyIsBetweenOperation isBetweenOperation = (PropertyIsBetweenOperation)ruleOperation;
              if( j == 0 )
              {
                minValue = Double.parseDouble( ( (BoundaryExpression)isBetweenOperation
                    .getLowerBoundary() ).getValue() );
                maxValue = Double.parseDouble( ( (BoundaryExpression)isBetweenOperation
                    .getUpperBoundary() ).getValue() );
                step = maxValue - minValue;
              }
              else
              {
                double tmpMinValue = Double.parseDouble( ( (BoundaryExpression)isBetweenOperation
                    .getLowerBoundary() ).getValue() );
                double tmpMaxValue = Double.parseDouble( ( (BoundaryExpression)isBetweenOperation
                    .getUpperBoundary() ).getValue() );
                if( tmpMinValue < minValue )
                  minValue = tmpMinValue;
                if( tmpMaxValue > maxValue )
                  maxValue = tmpMaxValue;
              }
            }
          }
        }
        rulePatternInputPanel = new RulePatternInputPanel( composite, "Pattern:", minValue,
            maxValue, step );
      }
    }
    else
      return;

    final EditSymbolizerPanel editSymbolizerPanel = new EditSymbolizerPanel( composite, tmpRule
        .getSymbolizers().length );

    symbolizerTabFolder = new TabFolder( composite, SWT.NULL );

    addSymbolizerPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Symbolizer symbolizer = ( (AddSymbolizerPanel)event.getSource() ).getSelection();
        if( symbolizer != null )
        {
          for( int i = 0; i < ruleCollection.size(); i++ )
          {
            Symbolizer[] symb =
            { symbolizer };
            ruleCollection.get( i ).addSymbolizer( cloneSymbolizer( symb )[0] );
          }
          getUserStyle().fireModellEvent(
              new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
          setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
          editSymbolizerPanel.update( ruleCollection.get( 0 ).getSymbolizers().length );
          drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
        }
      }
    } );

    if( rulePatternInputPanel != null )
    {
      rulePatternInputPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          RulePatternInputPanel panel = (RulePatternInputPanel)event.getSource();
          // reset the values for all rules in this pattern if step did not
          // change !!!!
          minValue = panel.getMin();
          maxValue = panel.getMax();
          step = panel.getStep();

          // first create new rules
          BoundaryExpression upperBoundary = null;
          BoundaryExpression lowerBoundary = null;
          ArrayList ruleList = new ArrayList();
          PropertyName propertyName = new PropertyName( addFilterPropertyPanel.getSelection() );
          PropertyIsBetweenOperation operation = null;

          // only need to take first rule and duplicate it
          // plus apply the pattern
          // there needs to be at least one rule, otherwise no pattern rule
          // visible !!!!
          int patternRuleNumber = (int)Math.ceil( ( maxValue - minValue ) / step );
          Symbolizer[] symbolizer = tmpRule.getSymbolizers();

          // first add those that are existing and are to be kept
          int currentSize = ruleCollection.size();

          if( patternRuleNumber <= currentSize )
          {
            for( int i = 0; i < patternRuleNumber; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + ( minValue + ( i * step ) ) );
              if( ( minValue + ( ( i + 1 ) * step ) ) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + ( minValue + ( ( i + 1 ) * step ) ) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
                  upperBoundary );
              ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
              ruleList.add( ruleCollection.get( i ) );
            }
          }
          else if( patternRuleNumber > currentSize )
          {
            for( int i = 0; i < currentSize; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + ( minValue + ( i * step ) ) );
              if( ( minValue + ( ( i + 1 ) * step ) ) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + ( minValue + ( ( i + 1 ) * step ) ) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
                  upperBoundary );
              ruleCollection.get( i ).setFilter( new ComplexFilter( operation ) );
              ruleList.add( ruleCollection.get( i ) );
            }
            for( int i = currentSize; i < patternRuleNumber; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + ( minValue + ( i * step ) ) );
              if( ( minValue + ( ( i + 1 ) * step ) ) > maxValue )
                upperBoundary = new BoundaryExpression( "" + maxValue );
              else
                upperBoundary = new BoundaryExpression( "" + ( minValue + ( ( i + 1 ) * step ) ) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
                  upperBoundary );
              ruleList.add( StyleFactory.createRule( cloneSymbolizer( symbolizer ), tmpRule
                  .getName(), "-name-" + i, "abstract", null, new ComplexFilter( operation ),
                  false, tmpRule.getMinScaleDenominator(), tmpRule.getMaxScaleDenominator() ) );
            }
          }

          // then remove old ones
          int collSize = ruleCollection.size() - 1;
          for( int i = collSize; i >= 0; i-- )
          {
            removeRule( ruleCollection.get( i ), getUserStyle() );
            getRulePatternCollection().removeRule( ruleCollection.get( i ) );
          }

          // add new ones
          for( int j = 0; j < ruleList.size(); j++ )
          {
            getRulePatternCollection().addRule( (Rule)ruleList.get( j ) );
            getUserStyle().getFeatureTypeStyles()[0].addRule( (Rule)ruleList.get( j ) );
          }
          // update
          drawSymbolizerTabItems( tmpRule, symbolizerTabFolder, ruleCollection );
          getUserStyle().fireModellEvent(
              new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
        }
      } );
    }
    editSymbolizerPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int action = ( (EditSymbolizerPanel)event.getSource() ).getAction();

        if( action == EditSymbolizerPanel.REM_SYMB )
        {
          int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 >= 0 )
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              Symbolizer s[] = ruleCollection.get( i ).getSymbolizers();
              ruleCollection.get( i ).removeSymbolizer( s[index1] );
            }
            symbolizerTabFolder.getItem( index1 ).dispose();
            setFocusedSymbolizerItem( index1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireModellEvent(
                new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
          }
          drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
        }
        else if( action == EditSymbolizerPanel.FOR_SYMB )
        {
          int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 == ( ruleCollection.get( 0 ).getSymbolizers().length - 1 ) || index1 < 0 )
          {/**/}
          else
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              Symbolizer newOrderedObjects[] = new Symbolizer[ruleCollection.get( i )
                  .getSymbolizers().length];
              for( int counter4 = 0; counter4 < ruleCollection.get( i ).getSymbolizers().length; counter4++ )
              {
                if( counter4 == index1 )
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4 + 1];
                else if( counter4 == ( index1 + 1 ) )
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4 - 1];
                else
                  newOrderedObjects[counter4] = ruleCollection.get( i ).getSymbolizers()[counter4];
              }
              ruleCollection.get( i ).setSymbolizers( newOrderedObjects );
            }
            setFocusedSymbolizerItem( index1 + 1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireModellEvent(
                new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
            drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
          }
        }
        else if( action == EditSymbolizerPanel.BAK_SYMB )
        {
          int index1 = symbolizerTabFolder.getSelectionIndex();
          if( index1 > 0 )
          {
            for( int i = 0; i < ruleCollection.size(); i++ )
            {
              Symbolizer newOrderedObjects[] = new Symbolizer[ruleCollection.get( i )
                  .getSymbolizers().length];
              for( int counter5 = 0; counter5 < ruleCollection.get( i ).getSymbolizers().length; counter5++ )
              {
                if( counter5 == index1 )
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5 - 1];
                else if( counter5 == ( index1 - 1 ) )
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5 + 1];
                else
                  newOrderedObjects[counter5] = ruleCollection.get( i ).getSymbolizers()[counter5];
              }
              ruleCollection.get( i ).setSymbolizers( newOrderedObjects );
            }
            setFocusedSymbolizerItem( index1 - 1 );
            setFocusedRuleItem( getRuleTabFolder().getSelectionIndex() );
            getUserStyle().fireModellEvent(
                new ModellEvent( getUserStyle(), ModellEvent.STYLE_CHANGE ) );
            drawSymbolizerTabItems( ruleCollection.get( 0 ), symbolizerTabFolder, ruleCollection );
          }
        }
      }
    } );

    // ******* DISPLAY ALL symbolizers
    drawSymbolizerTabItems( tmpRule, symbolizerTabFolder, ruleCollection );

    focusedRuleItem = index;
    composite.pack( true );
  }

  void drawSymbolizerTabItems( Rule rule, TabFolder symbolizerTabFolder,
      RuleCollection ruleCollection )
  {
    // remove all existing items from tab folder
    TabItem[] items = symbolizerTabFolder.getItems();
    for( int i = 0; i < items.length; i++ )
    {
      items[i].dispose();
      items[i] = null;
    }

    if( rule.getSymbolizers().length == 0 )
    {
      // add dummy invisilbe placeholder
      new FilterPatternSymbolizerTabItemBuilder( symbolizerTabFolder, null, userStyle,
          ruleCollection, -1 );
      symbolizerTabFolder.setVisible( false );
    }
    else
    {
      for( int j = 0; j < rule.getSymbolizers().length; j++ )
      {
        new FilterPatternSymbolizerTabItemBuilder( symbolizerTabFolder, rule.getSymbolizers()[j],
            userStyle, ruleCollection, j );
      }
      symbolizerTabFolder.pack();
      symbolizerTabFolder.setSize( 224, 259 );
      symbolizerTabFolder.setVisible( true );
    }
  }

  Symbolizer[] cloneSymbolizer( Symbolizer[] symbolizers )
  {
    Symbolizer[] returnArray = new Symbolizer[symbolizers.length];
    for( int i = 0; i < symbolizers.length; i++ )
    {
      String geomPropertyName = symbolizers[i].getGeometry().getPropertyName();
      if( symbolizers[i] instanceof PointSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Point",
            getFeatureType() );
      }
      else if( symbolizers[i] instanceof LineSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Line",
            getFeatureType() );
      }
      else if( symbolizers[i] instanceof TextSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Text",
            getFeatureType() );
      }
      else if( symbolizers[i] instanceof PolygonSymbolizer )
      {
        returnArray[i] = AddSymbolizerPanel.getSymbolizer( geomPropertyName, "Polygon",
            getFeatureType() );
      }
      else
        return null;
    }
    return returnArray;
  }

  void removeRule( Rule rule, UserStyle style )
  {
    FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    fts[0].removeRule( rule );
  }

  public int getFocusedRuleItem()
  {
    return focusedRuleItem;
  }

  public void setFocusedRuleItem( int m_focusedRuleItem )
  {
    this.focusedRuleItem = m_focusedRuleItem;
  }

  public TabFolder getRuleTabFolder()
  {
    return ruleTabFolder;
  }

  public void setRuleTabFolder( TabFolder m_ruleTabFolder )
  {
    this.ruleTabFolder = m_ruleTabFolder;
  }

  public int getFocusedSymbolizerItem()
  {
    return focusedSymbolizerItem;
  }

  public void setFocusedSymbolizerItem( int m_focusedSymbolizerItem )
  {
    this.focusedSymbolizerItem = m_focusedSymbolizerItem;
  }

  public RuleFilterCollection getRulePatternCollection()
  {
    return rulePatternCollection;
  }

  public void setRulePatternCollection( RuleFilterCollection m_rulePatternCollection )
  {
    this.rulePatternCollection = m_rulePatternCollection;
  }

  public FeatureType getFeatureType()
  {
    return featureType;
  }

  public void setFeatureType( FeatureType m_featureType )
  {
    this.featureType = m_featureType;
  }

  public String[] getNumericFeatureTypePropertylist()
  {
    return numericFeatureTypePropertylist;
  }

  public void setNumericFeatureTypePropertylist( ArrayList m_numericFeatureTypePropertylist )
  {
    String[] tmpList = new String[m_numericFeatureTypePropertylist.size()];
    for( int i = 0; i < m_numericFeatureTypePropertylist.size(); i++ )
      tmpList[i] = ( (FeatureTypeProperty)m_numericFeatureTypePropertylist.get( i ) ).getName();
    this.numericFeatureTypePropertylist = tmpList;
  }

  public KalypsoUserStyle getUserStyle()
  {
    return userStyle;
  }

  public void setUserStyle( KalypsoUserStyle m_userStyle )
  {
    this.userStyle = m_userStyle;
  }
}