/*
 * Created on 12.07.2004
 *
 */
package org.kalypso.editor.styleeditor;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.StringReader;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.graphics.Encoders;
import org.deegree.graphics.legend.LegendElement;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.xml.XMLTools;
import org.deegree_impl.graphics.legend.LegendFactory;
import org.deegree_impl.graphics.sld.Rule_Impl;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsLikeOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.editor.styleeditor.dialogs.FilterDialog;
import org.kalypso.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.EditSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.editor.styleeditor.panels.SliderPanel;
import org.kalypso.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.w3c.dom.Document;

/**
 * @author Administrator
 *
 */
public class RuleTabItemBuilder {
	
	private Composite parent = null;
	private Composite globalComposite = null;
	private Composite tabFolderComposite = null;	
	private int counter = 0;
		
	private TabItem tabItem = null;
	private Rule[] rules = null;
	private TabFolder ruleTabFolder = null;
	
	private KalypsoUserStyle userStyle = null;
	private FeatureType featureType = null;
		
	private int focuedRuleItem = -1;
	private int focusedSymbolizerItem = -1;
	
	
	public RuleTabItemBuilder(Composite composite, Rule[] rules, KalypsoUserStyle userStyle, FeatureType featureType)
	{	
		this.parent = composite; 
		this.userStyle = userStyle;
		this.featureType = featureType;		
		this.rules = rules;		
		globalComposite = new Composite(composite,SWT.NULL);
	}	
		
	public void draw()
	{	
		this.counter = 0;
		if(tabFolderComposite != null)
			tabFolderComposite.dispose();
		
		tabFolderComposite = new Composite(globalComposite, SWT.NULL);	
		tabFolderComposite.setLayout(new FormLayout());
		tabFolderComposite.layout();
		
		ruleTabFolder = new TabFolder(tabFolderComposite,SWT.NULL);		
		FormData RuleTableFolderLData = new FormData();
		RuleTableFolderLData.height = 500;
		RuleTableFolderLData.width = 235;
		RuleTableFolderLData.top =  new FormAttachment(10, 1000, 0);						
		ruleTabFolder.setLayoutData(RuleTableFolderLData);				
		
		for(int i= 0;i<rules.length; i++)
		{			
			TabItem tabItem = new TabItem(ruleTabFolder, SWT.NULL);	
			final Composite composite = new Composite(ruleTabFolder,SWT.NULL);		
			GridLayout compositeLayout = new GridLayout();
			composite.setSize(270,400);
			composite.setLayout(compositeLayout);
			compositeLayout.marginWidth = 5;
			compositeLayout.marginHeight = 5;		
			composite.layout();				
			tabItem.setControl(composite);
			String ruleName;
			final Rule rule = rules[i];
			if(rule.getTitle() != null)
				ruleName = rule.getTitle();
			else if(rule.getName() != null)
				ruleName = rule.getName();
			else
			{
				ruleName = "Rule " + (++counter);
				rule.setTitle(ruleName);
			}
			tabItem.setText(ruleName);										
			
			final TabFolder symbolizerTabFolder;
			
			TextInputPanel titleInputPanel = new TextInputPanel(composite,"Title:",rule.getTitle());
			titleInputPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					rule.setTitle(((TextInputPanel)event.getSource()).getLabelText());
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));									
					focuedRuleItem = ruleTabFolder.getSelectionIndex();										
				}
			});
			
			TextInputPanel nameInputPanel = new TextInputPanel(composite,"Name:",rule.getName());
			nameInputPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					rule.setName(((TextInputPanel)event.getSource()).getLabelText());
				
					
					
					  PropertyIsLikeOperation operation = new PropertyIsLikeOperation(new PropertyName("GEOM"),new Literal("test"),'*','a','b');
					  ComplexFilter filter = new ComplexFilter(operation);
					  rule.setFilter(filter);
					 
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
						System.out.println(((Rule_Impl)rule).exportAsXML());
					
					focuedRuleItem = ruleTabFolder.getSelectionIndex();						
				}
			});
			
			SliderPanel minDenominatorPanel = new SliderPanel(composite,"MinDenomiator:",0,10000000,100000,SliderPanel.INTEGER,rule.getMinScaleDenominator());
			minDenominatorPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					rule.setMinScaleDenominator(((SliderPanel)event.getSource()).getSelection());
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					focuedRuleItem = ruleTabFolder.getSelectionIndex();						
				}
			});			
			SliderPanel maxDenominatorPanel = new SliderPanel(composite,"MaxDenomiator:",0,10000000,100000,SliderPanel.INTEGER,rule.getMaxScaleDenominator());
			maxDenominatorPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					double max = ((SliderPanel)event.getSource()).getSelection();
					rule.setMaxScaleDenominator(max);
					Symbolizer symbolizers[] = rule.getSymbolizers();
					for(int i=0; i<symbolizers.length; i++){
						symbolizers[i].setMaxScaleDenominator(max);
					}					
					focuedRuleItem = ruleTabFolder.getSelectionIndex();
//					PropertyIsLikeOperation operation = new PropertyIsLikeOperation(new PropertyName("NAME"),new Literal("E*"),'*','?','/');
//					ComplexFilter filter = new ComplexFilter(operation);
//					rule.setFilter(filter);
					
					System.out.println(((Rule_Impl)rule).exportAsXML());
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));						
				}
			});
			
			AddSymbolizerPanel addSymbolizerPanel = new AddSymbolizerPanel(composite,"Symbolizer:",featureType);
			EditSymbolizerPanel editSymbolizerPanel = new EditSymbolizerPanel(composite,rule.getSymbolizers().length);
									
			symbolizerTabFolder = new TabFolder(composite,SWT.NULL);
			
			editSymbolizerPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					int action = ((EditSymbolizerPanel)event.getSource()).getAction();
					
					if(action == EditSymbolizerPanel.REM_SYMB)
					{							
						int index = symbolizerTabFolder.getSelectionIndex();						
						if(index >=0)
						{				
							Symbolizer s[] = rule.getSymbolizers();							
							rule.removeSymbolizer(s[index]);
							symbolizerTabFolder.getItem(index).dispose();																					
							focusedSymbolizerItem = index;							
							focuedRuleItem = ruleTabFolder.getSelectionIndex();							
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));											
						}	
						draw();						
					}
					else if(action == EditSymbolizerPanel.FOR_SYMB)
					{								
						int index = symbolizerTabFolder.getSelectionIndex();						
						if(index == (rule.getSymbolizers().length-1) || index <0){}
						else
						{																			
							Symbolizer newOrderedObjects[] = new Symbolizer[rule.getSymbolizers().length];
							for(int i=0; i<rule.getSymbolizers().length; i++)
							{								
								if(i == index)
									newOrderedObjects[i] = rule.getSymbolizers()[i+1];
								else if(i == (index+1))
									newOrderedObjects[i] = rule.getSymbolizers()[i-1];
								else
									newOrderedObjects[i] = rule.getSymbolizers()[i];											
							}
							rule.setSymbolizers(newOrderedObjects);
							focusedSymbolizerItem = index+1;
							focuedRuleItem = ruleTabFolder.getSelectionIndex();											
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
							draw();
						}
					}
					
					else if(action == EditSymbolizerPanel.BAK_SYMB)
					{											
						int index = symbolizerTabFolder.getSelectionIndex();
						if(index >0)
						{													
							Symbolizer newOrderedObjects[] = new Symbolizer[rule.getSymbolizers().length];
							for(int i=0; i<rule.getSymbolizers().length; i++)
							{								
								if(i == index)
									newOrderedObjects[i] = rule.getSymbolizers()[i-1];
								else if(i == (index-1))
									newOrderedObjects[i] = rule.getSymbolizers()[i+1];
								else
									newOrderedObjects[i] = rule.getSymbolizers()[i];											
							}
							rule.setSymbolizers(newOrderedObjects);
							focusedSymbolizerItem = index-1;
							focuedRuleItem = ruleTabFolder.getSelectionIndex();							
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
							draw();	
						}
					}				
				}
			});							
			
			addSymbolizerPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					Symbolizer symbolizer = ((AddSymbolizerPanel)event.getSource()).getSelection();
					if(symbolizer instanceof Symbolizer)
					{
						rule.addSymbolizer(symbolizer);											
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
						focuedRuleItem = ruleTabFolder.getSelectionIndex();
						focusedSymbolizerItem = rule.getSymbolizers().length-1;						
						draw();
					}									
				}
			});
				
//			Button button = new Button(composite,SWT.NULL);
//			final FilterDialog filterDialog = new FilterDialog(composite.getShell());
//			button.addSelectionListener(new SelectionListener() {
//				public void widgetSelected(SelectionEvent e) {
//					filterDialog.open();			
//				}
//				public void widgetDefaultSelected(SelectionEvent e) {
//					widgetSelected(e);
//				}
//			});
			
			// ******* SAVING THE SLD-STYLE
			Button saveButton = new Button(composite,SWT.NULL);
			final Label label = new Label(composite, SWT.NULL);
			saveButton.setText("Save");						
			saveButton.addSelectionListener(new SelectionListener() {
				public void widgetSelected(SelectionEvent e) {						 
					SaveStyleAction.saveUserStyle(userStyle, composite.getShell());							
				}
				public void widgetDefaultSelected(SelectionEvent e) {
					widgetSelected(e);
				}
			});
			
			// ******* DISPLAY ALL symbolizers
			for(int j=0; j<rule.getSymbolizers().length; j++){
				new SymbolizerTabItemBuilder(symbolizerTabFolder,rule.getSymbolizers()[j],userStyle,featureType); 								
			}			
			if(rule.getSymbolizers().length == 0)
				symbolizerTabFolder.setVisible(false);
			if(focuedRuleItem == i && focusedSymbolizerItem!=-1)
				symbolizerTabFolder.setSelection(focusedSymbolizerItem);
		}
		if(focuedRuleItem != -1)
			ruleTabFolder.setSelection(focuedRuleItem);
		if(rules.length == 0)
			ruleTabFolder.setVisible(false);
				
		tabFolderComposite.pack(true);		
	}
	
	public int getSelectedRule()
	{
		if(ruleTabFolder != null)
			return ruleTabFolder.getSelectionIndex();
		else 
			return -1;
	}
	
	public void setSelectedRule(int index)
	{
		ruleTabFolder.setSelection(index);		
	}	
	
	// transforms AWT-BufferedImage into SWT ImageData
	// Copyright (c) 2000, 2004 IBM Corporation and others.
	//* All rights reserved. This program and the accompanying 
	static ImageData convertToSWT(BufferedImage bufferedImage) {
		if (bufferedImage.getColorModel() instanceof DirectColorModel) {
			DirectColorModel colorModel = (DirectColorModel)bufferedImage.getColorModel();
			PaletteData palette = new PaletteData(colorModel.getRedMask(), colorModel.getGreenMask(), colorModel.getBlueMask());
			ImageData data = new ImageData(bufferedImage.getWidth(), bufferedImage.getHeight(), colorModel.getPixelSize(), palette);
			WritableRaster raster = bufferedImage.getRaster();
			int[] pixelArray = new int[3];
			for (int y = 0; y < data.height; y++) {
				for (int x = 0; x < data.width; x++) {
					raster.getPixel(x, y, pixelArray);
					int pixel = palette.getPixel(new RGB(pixelArray[0], pixelArray[1], pixelArray[2]));
					data.setPixel(x, y, pixel);
				}
			}		
			return data;		
		} else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
			IndexColorModel colorModel = (IndexColorModel)bufferedImage.getColorModel();
			int size = colorModel.getMapSize();
			byte[] reds = new byte[size];
			byte[] greens = new byte[size];
			byte[] blues = new byte[size];
			colorModel.getReds(reds);
			colorModel.getGreens(greens);
			colorModel.getBlues(blues);
			RGB[] rgbs = new RGB[size];
			for (int i = 0; i < rgbs.length; i++) {
				rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF, blues[i] & 0xFF);
			}
			PaletteData palette = new PaletteData(rgbs);
			ImageData data = new ImageData(bufferedImage.getWidth(), bufferedImage.getHeight(), colorModel.getPixelSize(), palette);
			data.transparentPixel = colorModel.getTransparentPixel();
			WritableRaster raster = bufferedImage.getRaster();
			int[] pixelArray = new int[1];
			for (int y = 0; y < data.height; y++) {
				for (int x = 0; x < data.width; x++) {
					raster.getPixel(x, y, pixelArray);
					data.setPixel(x, y, pixelArray[0]);
				}
			}
			return data;
		}
		return null;
	}
	
}
