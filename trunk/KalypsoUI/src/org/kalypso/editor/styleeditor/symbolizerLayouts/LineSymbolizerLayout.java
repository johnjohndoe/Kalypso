/*
 * Created on 26.07.2004
 *
 */
package org.kalypso.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.editor.styleeditor.panels.ComboPanel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.editor.styleeditor.panels.SliderPanel;
import org.kalypso.editor.styleeditor.panels.StrokeDasharrayPanel;
import org.kalypso.editor.styleeditor.panels.StrokeDashoffsetPanel;
import org.kalypso.editor.styleeditor.panels.StrokeLinecapComboPanel;
import org.kalypso.editor.styleeditor.panels.StrokeLinejoinComboPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 *
 */

public class LineSymbolizerLayout extends SymbolizerLayout{

	public LineSymbolizerLayout(Composite composite, Symbolizer symbolizer, KalypsoUserStyle userStyle)
	{
		super(composite,symbolizer,userStyle);
	}
	
	public void draw() throws FilterEvaluationException
	{
		LineSymbolizer lineSymbolizer = (LineSymbolizer) symbolizer;
		final Stroke stroke = lineSymbolizer.getStroke();
		
		GridLayout compositeLayout = new GridLayout();		
		compositeLayout.marginHeight = 2;
		
		Group strokeGroup = new Group(composite,SWT.NULL);
		strokeGroup.setText("Stroke");
		GridData strokeGroupData = new GridData();
		strokeGroupData.widthHint = 210;			
		strokeGroup.setLayoutData(strokeGroupData);			
		strokeGroup.setLayout(compositeLayout);
		strokeGroup.layout();	
		
		// Stroke ColorChooser
		ColorChooserPanel strokeColorChooserPanel = null;
		strokeColorChooserPanel = new ColorChooserPanel(strokeGroup, "Color:", stroke.getStroke(null));
		strokeColorChooserPanel.addColorChooserListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				Color color = ((ColorChooserPanel) event.getSource()).getColor();			
				stroke.setStroke(new java.awt.Color(color.getRed(), color.getGreen(), color.getBlue()));
				userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
			}
		});
		
		// Stroke Width Slider
		SliderPanel strokeWidthPanel = new SliderPanel(strokeGroup, "Width:", 0,10,1,SliderPanel.INTEGER, stroke.getWidth(null));
		strokeWidthPanel.addPanelListener(new PanelListener() {			
			public void valueChanged(PanelEvent event) {
				double width = ((SliderPanel)event.getSource()).getSelection();
				stroke.setWidth(width);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
			}
		});
		
		// Stroke Opacity Slider
		SliderPanel strokeOpacityPanel = new SliderPanel(strokeGroup, "Opacity:", 0,1,1,SliderPanel.DECIMAL,stroke.getOpacity(null));
		strokeOpacityPanel.addPanelListener(new PanelListener() {			
			public void valueChanged(PanelEvent event) {
				double opacity = ((SliderPanel)event.getSource()).getSelection();
				stroke.setOpacity(opacity);			
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});	
		
		// Stroke Linejoin ComboPanel		
		ComboPanel strokeLinejoinPanel = new StrokeLinejoinComboPanel(strokeGroup, "Linejoin",stroke.getLineJoin(null));
		stroke.setLineJoin(strokeLinejoinPanel.getSelection());		
		strokeLinejoinPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				int lineJoin = ((StrokeLinejoinComboPanel)event.getSource()).getSelection();
				stroke.setLineJoin(lineJoin);											
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});

		// Stroke Linecap ComboPanel
		ComboPanel strokeLinecapPanel = new StrokeLinecapComboPanel(strokeGroup, "Linecap", stroke.getLineCap(null));
		stroke.setLineCap(strokeLinecapPanel.getSelection());		
		strokeLinecapPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				int lineCap = ((StrokeLinecapComboPanel)event.getSource()).getSelection();
				stroke.setLineCap(lineCap);				
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});
		
		// Stroke DashOffset		
		StrokeDashoffsetPanel strokeDashoffsetPanel = new StrokeDashoffsetPanel(strokeGroup, "Dashoffset", stroke.getDashOffset(null));
		strokeDashoffsetPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				float dashOffset = ((StrokeDashoffsetPanel)event.getSource()).getValue();
				stroke.setDashOffset(dashOffset);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});
		
		// Stroke DashArray
		StrokeDasharrayPanel strokeDasharrayPanel = new StrokeDasharrayPanel(strokeGroup, "Dasharray", stroke.getDashArray(null));
		strokeDasharrayPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				float dashArray[] = ((StrokeDasharrayPanel)event.getSource()).getValue();
				stroke.setDashArray(dashArray);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});					
	}
}
