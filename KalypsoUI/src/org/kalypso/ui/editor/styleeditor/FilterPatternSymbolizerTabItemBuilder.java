/*
 * Created on 12.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor;

import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.AbstractSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.FilterPatternLineSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.FilterPatternPointSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.FilterPatternPolygonSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.SymbolizerLayout;

/**
 * @author F.Lindemann
 *  
 */
public class FilterPatternSymbolizerTabItemBuilder
{

  public FilterPatternSymbolizerTabItemBuilder( TabFolder tabFolder, Symbolizer symbolizer,
      KalypsoUserStyle userStyle, RuleCollection ruleCollection, int symbolizerIndex )
  {
    TabItem tabItem = new TabItem( tabFolder, SWT.NULL );

    Composite composite = new Composite( tabFolder, SWT.NULL );
    GridLayout compositeLayout = new GridLayout();
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    composite.layout();
    tabItem.setControl( composite );

    AbstractSymbolizerLayout symbolizerLayout = null;

    if( symbolizer == null )
    {
      symbolizerLayout = new SymbolizerLayout( composite );
    }
    else if( symbolizer instanceof PolygonSymbolizer )
    {
      tabItem.setText( "Polygon" );
      symbolizerLayout = new FilterPatternPolygonSymbolizerLayout( composite, symbolizer,
          userStyle, ruleCollection, symbolizerIndex );
    }
    else if( symbolizer instanceof PointSymbolizer )
    {
      tabItem.setText( "Point" );
      symbolizerLayout = new FilterPatternPointSymbolizerLayout( composite, symbolizer, userStyle,
          ruleCollection, symbolizerIndex );
    }
    else if( symbolizer instanceof LineSymbolizer )
    {
      tabItem.setText( "Line" );
      symbolizerLayout = new FilterPatternLineSymbolizerLayout( composite, symbolizer, userStyle,
          ruleCollection, symbolizerIndex );
    }

    if( symbolizerLayout != null )
    {
      try
      {
        symbolizerLayout.draw();
      }
      catch( FilterEvaluationException e )
      {
        e.printStackTrace();
      }
    }

  }
}