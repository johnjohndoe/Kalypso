/*
 * Created on 12.07.2004
 *
 */
package org.kalypso.editor.styleeditor;

import org.deegree.graphics.sld.*;
import org.deegree.model.feature.FeatureType;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.editor.styleeditor.symbolizerLayouts.LineSymbolizerLayout;
import org.kalypso.editor.styleeditor.symbolizerLayouts.PointSymbolizerLayout;
import org.kalypso.editor.styleeditor.symbolizerLayouts.PolygonSymbolizerLayout;
import org.kalypso.editor.styleeditor.symbolizerLayouts.RasterSymbolizerLayout;
import org.kalypso.editor.styleeditor.symbolizerLayouts.SymbolizerLayout;
import org.kalypso.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;
import org.kalypso.ogc.gml.KalypsoUserStyle;


/**
 * @author Administrator
 *
 */
public class SymbolizerTabItemBuilder {
	
	public SymbolizerTabItemBuilder(TabFolder tabFolder, Symbolizer symbolizer, KalypsoUserStyle userStyle, FeatureType featureType)
	{				
		TabItem tabItem = new TabItem(tabFolder, SWT.NULL);
		
		Composite composite = new Composite(tabFolder, SWT.NULL);				
		GridLayout compositeLayout = new GridLayout();
		composite.setLayout(compositeLayout);
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		composite.layout();				
		tabItem.setControl(composite);
		
		SymbolizerLayout symbolizerLayout = null;
		if(symbolizer instanceof PolygonSymbolizer)
		{
			tabItem.setText("Polygon");
			symbolizerLayout = new PolygonSymbolizerLayout(composite, symbolizer,userStyle);
		}
		else if(symbolizer instanceof PointSymbolizer)
		{
			tabItem.setText("Point");
			symbolizerLayout = new PointSymbolizerLayout(composite, symbolizer, userStyle);
		}
		else if(symbolizer instanceof LineSymbolizer)
		{
			tabItem.setText("Line");
			symbolizerLayout = new LineSymbolizerLayout(composite, symbolizer, userStyle);
		}
		else if(symbolizer instanceof RasterSymbolizer)
		{
			tabItem.setText("Raster");
			symbolizerLayout = new RasterSymbolizerLayout(composite, symbolizer, userStyle);
		}
		else if(symbolizer instanceof TextSymbolizer)
		{
			tabItem.setText("Text");						
			symbolizerLayout = new TextSymbolizerLayout(composite, symbolizer, userStyle, featureType);
			
		}
		
		if(symbolizerLayout != null)
		{
			try {
				symbolizerLayout.draw();
			} catch (FilterEvaluationException e) {
				System.out.println("org.kalypso.editor.styleeditor.SymbolizerTabItemBuilder " + e);
				e.printStackTrace();
			}
		}
		
	}
}