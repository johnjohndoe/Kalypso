/*
 * Created on 09.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;

import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.ui.editor.styleeditor.dialogs.filterencoding.BoundaryExpression;
import org.kalypso.ui.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.ControlRulePanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;

/**
 * @author F.Lindemann
 *  
 */
public class SLDEditorGuiBuilder
{

  private FeatureType featureType = null;

  private Composite parent = null;

  private ScrolledComposite scrollComposite = null;

  private Label titleLabel = null;

  private int focusedRuleItem = -1;

  private RuleFilterCollection rulePatternCollection = null;

  public SLDEditorGuiBuilder( Composite m_parent )
  {
    this.parent = m_parent;
    buildSWTGui( null, null );
  }

  public void buildSWTGui( final KalypsoUserStyle userStyle, IKalypsoFeatureTheme theme )
  {
    buildSWTGui( userStyle, theme, -1 );
  }

  public void buildSWTGui( final KalypsoUserStyle userStyle, final IKalypsoFeatureTheme theme,
      final int index )
  {
    if( index != -1 )
      focusedRuleItem = index;
    if( scrollComposite != null )
      scrollComposite.dispose();

    // get FeatureType from layer
    if( theme != null )
      featureType = theme.getFeatureType();

    scrollComposite = new ScrolledComposite( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    Composite mainComposite = new Composite( scrollComposite, SWT.NONE );
    mainComposite.setLayout( new GridLayout() );
    mainComposite.layout();
    scrollComposite.setContent( mainComposite );
    scrollComposite.setSize( parent.getSize() );
    Label nameLabel = null;
    if( userStyle == null )
    {
      nameLabel = new Label( mainComposite, 0 );
      nameLabel.setText( "No style found to show in editor" );
      mainComposite.pack( true );
      return;
    }

    titleLabel = new Label( mainComposite, SWT.NULL );
    titleLabel.setText( "Style Editor v1.0" );
    titleLabel.setFont( new Font( null, "Arial", 12, SWT.BOLD ) );
    nameLabel = new Label( mainComposite, 0 );
    nameLabel.setText( "Style: " + userStyle.getName() );
    nameLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );

    final Rule[] rules = getRules( userStyle );
    rulePatternCollection = RuleFilterCollection.getInstance();
    // filter patterns from rules and draw them afterwards
    for( int i = 0; i < rules.length; i++ )
    {
      rulePatternCollection.addRule( rules[i] );
    }

    // check whether there are featureTypes that have numeric properties to be used by a pattern-filter    
    final ArrayList numericFeatureTypePropertylist = new ArrayList();
    FeatureTypeProperty[] ftp = getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      if( ftp[i].getType().equalsIgnoreCase( "java.lang.Double" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigInteger" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Byte" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigDecimal" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Float" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Integer" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Long" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Short" ) )
        numericFeatureTypePropertylist.add( ftp[i] );
    }    
    ControlRulePanel controlRulePanel = new ControlRulePanel( mainComposite, "Rule:",
        rulePatternCollection.size() , numericFeatureTypePropertylist.size());

    final RuleTabItemBuilder ruleTabItemBuilder = new RuleTabItemBuilder(mainComposite,rulePatternCollection, userStyle,theme); 

    controlRulePanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int action = ( (ControlRulePanel)event.getSource() ).getAction();
        switch( action )
        {
        case ControlRulePanel.ADD_RULE:
        {
          Symbolizer symbolizers[] = null;
          Rule rule = StyleFactory.createRule( symbolizers );
          addRule( rule, userStyle );
          setFocusedRuleItem( getRulePatternCollection().size() );
          buildSWTGui( userStyle, theme );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          break;
        }
        case ControlRulePanel.REM_RULE:
        {
          int index2 = ruleTabItemBuilder.getSelectedRule();
          if( index2 > -1 && index2 < getRulePatternCollection().size() )
          {
            Object ruleObject = getRulePatternCollection().getFilteredRuleCollection().get( index2 );
            if( ruleObject instanceof Rule )
            {
              removeRule( (Rule)ruleObject, userStyle );
            }
            else if( ruleObject instanceof RuleCollection )
            {
              RuleCollection ruleCollection = (RuleCollection)ruleObject;
              for( int i = 0; i < ruleCollection.size(); i++ )
                removeRule( ruleCollection.get( i ), userStyle );
            }

            if( index2 >= 0 )
              setFocusedRuleItem( --index2 );
            buildSWTGui( userStyle, theme );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          }
          break;
        }
        case ControlRulePanel.BAK_RULE:
        {
          int index3 = ruleTabItemBuilder.getSelectedRule();
          if( index3 > 0 )
          {
            ArrayList newOrdered = new ArrayList();
            for( int i = 0; i < getRulePatternCollection().size(); i++ )
            {
              if( i == index3 )
                newOrdered
                    .add( getRulePatternCollection().getFilteredRuleCollection().get( i - 1 ) );
              else if( i == ( index3 - 1 ) )
                newOrdered
                    .add( getRulePatternCollection().getFilteredRuleCollection().get( i + 1 ) );
              else
                newOrdered.add( getRulePatternCollection().getFilteredRuleCollection().get( i ) );
            }
            setRules( newOrdered, userStyle );
            setFocusedRuleItem( index3 - 1 );
            buildSWTGui( userStyle, theme );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          }
          break;
        }
        case ControlRulePanel.FOR_RULE:
        {
          int index4 = ruleTabItemBuilder.getSelectedRule();
          if( index4 == ( getRulePatternCollection().size() - 1 ) || index4 < 0 )
          {/**/}
          else
          {
            ArrayList newOrdered = new ArrayList();
            for( int i = 0; i < getRulePatternCollection().size(); i++ )
            {
              if( i == index4 )
                newOrdered
                    .add( getRulePatternCollection().getFilteredRuleCollection().get( i + 1 ) );
              else if( i == ( index4 + 1 ) )
                newOrdered
                    .add( getRulePatternCollection().getFilteredRuleCollection().get( i - 1 ) );
              else
                newOrdered.add( getRulePatternCollection().getFilteredRuleCollection().get( i ) );
            }
            setRules( newOrdered, userStyle );
            setFocusedRuleItem( index4 + 1 );
            buildSWTGui( userStyle, theme );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          }
          break;
        }
        case ControlRulePanel.ADD_PATTERN_RULE:
        {
          // create a pattern-filter for this style
          if( numericFeatureTypePropertylist.size() > 0 )
          {
//            ArrayList ruleList = new ArrayList();
            // set by default first featuretypeproperty
            FeatureTypeProperty prop = (FeatureTypeProperty)numericFeatureTypePropertylist.get( 0 );
//            BoundaryExpression upperBoundary = null;
//            BoundaryExpression lowerBoundary = null;
//            PropertyName propertyName = new PropertyName( prop.getName() );
//            PropertyIsBetweenOperation operation = null;           
           
//            int geometryType = TextSymbolizerLayout.getFeatureTypeGeometryType(getFeatureType());
//            switch(geometryType)
//            {
//              case GM_POINT:
//              {
//                break;
//              }
//              case GM_LINESTRING:
//              {
//                break;
//              }
//              case GM_POLYGON:
//              {
//                break;
//              }
//              case GM_MULTIPOINT:
//              {
//                break;
//              }  
//              case GM_OBJECT:
//              {
//                break;
//              }                  
//            }
              
              
            String[] geometryObjects = AddSymbolizerPanel.getGeometries( getFeatureType() );
            if( geometryObjects.length > 0 )
            {       
              System.out.println("prop " + prop + " geom " +  geometryObjects[0] );
//              // I choose to use a ploygon-symbolier hopeing that it works
//              Symbolizer symbo = AddSymbolizerPanel.getSymbolizer( geometryObjects[0], "Polygon",
//                  getFeatureType() );
//              Geometry geom = symbo.getGeometry();
//
//              String patternName = "-title-" + new Date().getTime();
//
//              for( int i = 0; i < 5; i++ )
//              {
//                lowerBoundary = new BoundaryExpression( "" + ( i * 1 ) );
//                upperBoundary = new BoundaryExpression( "" + ( ( i + 1 ) * 1 ) );
//                operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
//                    upperBoundary );
//
//                PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
//                symb.setGeometry( geom );
//
//                Color color = new Color( 600000 * ( i + 1 ) );
//                Fill fill = StyleFactory.createFill( color );
//                symb.setFill( fill );
//                Symbolizer s[] =
//                { symb };
//
//                ruleList.add( StyleFactory.createRule( s, patternName, "-name-" + i, "abstract",
//                    null, new ComplexFilter( operation ), false, symb.getMinScaleDenominator(), symb
//                        .getMaxScaleDenominator() ) );
//                userStyle.getFeatureTypeStyles()[0].addRule( StyleFactory.createRule( s,
//                    patternName,"-name-" + i, "abstract", null, new ComplexFilter( operation ),
//                    false, symb.getMinScaleDenominator(), symb.getMaxScaleDenominator() ) );
              }
              //userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );             
           //   buildSWTGui( userStyle, theme );
            }               
          System.out.println("sadfsd");
          break;
        }
        default:
          break;
        }
      }
    } );

    // ***** Button Composite
    final Composite buttonComposite = new Composite( mainComposite, SWT.NULL );
    buttonComposite.setLayout( new GridLayout( 2, true ) );

    // ******* SAVING THE SLD-STYLE
    final Button saveButton = new Button( buttonComposite, SWT.NULL );
    saveButton.setText( "Save" );
    saveButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        SaveStyleAction.saveUserStyle( userStyle, buttonComposite.getShell() );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    // ******* CREATE PATTERN
    final Button patternButton = new Button( buttonComposite, SWT.NULL );
    patternButton.setText( "Pattern" );
    patternButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        ArrayList list = new ArrayList();
        FeatureTypeProperty[] tp = getFeatureType().getProperties();
        for( int i = 0; i < tp.length; i++ )
        {
          if( tp[i].getType().equalsIgnoreCase( "java.lang.Double" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.math.BigInteger" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.lang.Byte" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.math.BigDecimal" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.lang.Float" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.lang.Integer" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.lang.Long" ) )
            list.add( tp[i] );
          else if( tp[i].getType().equalsIgnoreCase( "java.lang.Short" ) )
            list.add( tp[i] );
        }

        if( list.size() > 0 )
        {
          ArrayList ruleList = new ArrayList();
          FeatureTypeProperty prop = (FeatureTypeProperty)list.get( 0 );
          BoundaryExpression upperBoundary = null;
          BoundaryExpression lowerBoundary = null;
          PropertyName propertyName = new PropertyName( prop.getName() );
          PropertyIsBetweenOperation operation = null;

          final Feature[] fts = theme.getWorkspace().getFeatures( theme.getFeatureType() );
          double minValue = -1;
          double maxValue = -1;
          double value;
          boolean hasFeatures = false;
          if( fts.length > 0 )
          {
            value = Double.parseDouble( fts[0].getProperty( prop.getName() ).toString() );
            minValue = value;
            maxValue = value;
            hasFeatures = true;
          }
          // TODO: Need to check whether there are features??? otherwise no
          // min,max value
          for( int i = 0; i < fts.length; i++ )
          {
            value = Double.parseDouble( fts[i].getProperty( prop.getName() ).toString() );
            if( value < minValue )
              minValue = value;
            else if( value > maxValue )
              maxValue = value;
          }
          if( hasFeatures )
            System.out.println( "Min: " + minValue + "  Max: " + maxValue );

          String[] geometryObjects = AddSymbolizerPanel.getGeometries( getFeatureType() );
          if( geometryObjects.length > 0 )
          {
            // I choose to use a ploygon-symbolier hopeing that it works
            Symbolizer symbo = AddSymbolizerPanel.getSymbolizer( geometryObjects[0], "Polygon",
                getFeatureType() );
            Geometry geom = symbo.getGeometry();

            String patternName = "-title-" + new Date().getTime();

            for( int i = 0; i < 5; i++ )
            {
              lowerBoundary = new BoundaryExpression( "" + ( i * 1 ) );
              upperBoundary = new BoundaryExpression( "" + ( ( i + 1 ) * 1 ) );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
                  upperBoundary );

              PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
              symb.setGeometry( geom );

              Color color = new Color( 600000 * ( i + 1 ) );
              Fill fill = StyleFactory.createFill( color );
              symb.setFill( fill );
              Symbolizer s[] =
              { symb };

              ruleList.add( StyleFactory.createRule( s, patternName, "-name-" + i, "abstract",
                  null, new ComplexFilter( operation ), false, symb.getMinScaleDenominator(), symb
                      .getMaxScaleDenominator() ) );
              userStyle.getFeatureTypeStyles()[0].addRule( StyleFactory.createRule( s,
                  patternName,"-name-" + i, "abstract", null, new ComplexFilter( operation ),
                  false, symb.getMinScaleDenominator(), symb.getMaxScaleDenominator() ) );
            }
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
            //System.out.println(userStyle.exportAsXML());
            buildSWTGui( userStyle, theme );
          }
        }

      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    ruleTabItemBuilder.draw();
    if( focusedRuleItem > -1 )
      ruleTabItemBuilder.setSelectedRule( focusedRuleItem );
    mainComposite.pack( true );
  }

  private Rule[] getRules( UserStyle style )
  {
    FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    // TODO Pay attention that it is currently limited to the first fts
    return fts[0].getRules();
  }

  void removeRule( Rule rule, UserStyle style )
  {
    FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    fts[0].removeRule( rule );
  }

  void setRules( ArrayList ruleObjects, UserStyle style )
  {
    FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    ArrayList ruleInstances = new ArrayList();
    for( int i = 0; i < ruleObjects.size(); i++ )
    {
      if( ruleObjects.get( i ) instanceof Rule )
        ruleInstances.add( ruleObjects.get( i ) );
      else if( ruleObjects.get( i ) instanceof RuleCollection )
      {
        RuleCollection ruleCollection = (RuleCollection)ruleObjects.get( i );
        for( int j = 0; j < ruleCollection.size(); j++ )
          ruleInstances.add( ruleCollection.get( j ) );
      }
    }
    Rule[] ruleArray = new Rule[ruleInstances.size()];
    for( int c = 0; c < ruleInstances.size(); c++ )
      ruleArray[c] = (Rule)ruleInstances.get( c );
    fts[0].setRules( ruleArray );
  }

  void addRule( Rule rule, UserStyle style )
  {
    FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    fts[0].addRule( rule );
  }

  public int getFocusedRuleItem()
  {
    return focusedRuleItem;
  }

  public void setFocusedRuleItem( int m_focusedRuleItem )
  {
    this.focusedRuleItem = m_focusedRuleItem;
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
}