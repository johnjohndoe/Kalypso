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
 * Created on 09.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.ui.ImageProvider;
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
      nameLabel.setText( MessageBundle.STYLE_EDITOR_NO_STYLE_FOR_EDITOR );
      mainComposite.pack( true );
      return;
    }

    titleLabel = new Label( mainComposite, SWT.NULL );
    titleLabel.setText( MessageBundle.STYLE_EDITOR_EDITOR_TITLE );
    titleLabel.setFont( new Font( null, "Arial", 12, SWT.BOLD ) );
    nameLabel = new Label( mainComposite, 0 );
    nameLabel.setText( MessageBundle.STYLE_EDITOR_STYLE + userStyle.getName() );
    nameLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );

    final Rule[] rules = getRules( userStyle );
    rulePatternCollection = RuleFilterCollection.getInstance();
    // filter patterns from rules and draw them afterwards
    for( int i = 0; i < rules.length; i++ )
    {
      rulePatternCollection.addRule( rules[i] );
    }

    // check whether there are featureTypes that have numeric properties to be
    // used by a pattern-filter
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
    ControlRulePanel controlRulePanel = new ControlRulePanel( mainComposite,
        MessageBundle.STYLE_EDITOR_RULE, rulePatternCollection.size(),
        numericFeatureTypePropertylist.size() );

    final RuleTabItemBuilder ruleTabItemBuilder = new RuleTabItemBuilder( mainComposite,
        rulePatternCollection, userStyle, theme, numericFeatureTypePropertylist );

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
          {
            // nothing
          }
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
            ArrayList ruleList = new ArrayList();
            // set by default first featuretypeproperty
            FeatureTypeProperty prop = (FeatureTypeProperty)numericFeatureTypePropertylist.get( 0 );
            BoundaryExpression upperBoundary = null;
            BoundaryExpression lowerBoundary = null;
            PropertyName propertyName = new PropertyName( prop.getName() );
            PropertyIsBetweenOperation operation = null;

            List geometryObjects = AddSymbolizerPanel.queryGeometriesPropertyNames( getFeatureType().getProperties(),null );
            //              geometryObjects = AddSymbolizerPanel.queryGeometriesPropertyNames( getFeatureType().getVirtuelFeatureTypeProperty(),geometryObjects );

            if( geometryObjects.size() > 0 )
            {
              Symbolizer symbo = AddSymbolizerPanel.getSymbolizer( (String)geometryObjects.get(0), "Point",
                  getFeatureType() );
              String patternName = "-name-" + new Date().getTime();
              lowerBoundary = new BoundaryExpression( "0" );
              upperBoundary = new BoundaryExpression( "1" );
              operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary,
                  upperBoundary );

              ruleList.add( StyleFactory.createRule( null, patternName, "", "abstract", null,
                  new ComplexFilter( operation ), false, symbo.getMinScaleDenominator(), symbo
                      .getMaxScaleDenominator() ) );
              userStyle.getFeatureTypeStyles()[0].addRule( StyleFactory.createRule( null,
                  patternName, "", "abstract", null, new ComplexFilter( operation ), false, symbo
                      .getMinScaleDenominator(), symbo.getMaxScaleDenominator() ) );
            }
            setFocusedRuleItem( getRulePatternCollection().size() );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
            buildSWTGui( userStyle, theme );
          }
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
    final Label saveButton = new Label( buttonComposite, SWT.NULL );
    saveButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_SAVE.createImage() );
    saveButton.setToolTipText( MessageBundle.STYLE_EDITOR_SAVE_STYLE );
    saveButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        SaveStyleAction.saveUserStyle( userStyle, buttonComposite.getShell() );
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
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