/*
 * Created on 12.07.2004
 *
 */
package org.kalypso.editor.styleeditor;


import java.awt.Color;
import java.util.ArrayList;

import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.editor.styleeditor.dialogs.errordialog.StyleEditorErrorDialog;
import org.kalypso.editor.styleeditor.dialogs.filterdialog.FilterDialog;
import org.kalypso.editor.styleeditor.dialogs.filterdialog.FilterDialogEvent;
import org.kalypso.editor.styleeditor.dialogs.filterdialog.FilterDialogListener;
import org.kalypso.editor.styleeditor.dialogs.filterdialog.filterencoding.BoundaryExpression;
import org.kalypso.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.DenominatorInputPanel;
import org.kalypso.editor.styleeditor.panels.EditSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.LegendLabel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.SaveStyleAction;

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
		
	private int focusedRuleItem = -1;
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
		RuleTableFolderLData.height = 550;
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
			
			final TextInputPanel titleInputPanel = new TextInputPanel(composite,"Title:",rule.getTitle());
			titleInputPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					String title = ((TextInputPanel)event.getSource()).getLabelText();
					if(title == null || title.trim().length() == 0)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid input value","Title should not be null");						
						errorDialog.showError();						
						titleInputPanel.setInputText(rule.getTitle());
					}
					else
					{
						rule.setTitle(title);
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					}
					focusedRuleItem = ruleTabFolder.getSelectionIndex();										
				}
			});

			TextInputPanel nameInputPanel = new TextInputPanel(composite,"Name:",rule.getName());
			nameInputPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					String name = ((TextInputPanel)event.getSource()).getLabelText();
					if(name == null || name.trim().length() == 0)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid input value","Name should not be null");
						errorDialog.showError();						
						titleInputPanel.setInputText(rule.getTitle());
					}
					else
					{
						rule.setName(name);
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					}
					focusedRuleItem = ruleTabFolder.getSelectionIndex();																			
				}
			});
			
			final DenominatorInputPanel minDenominatorPanel = new DenominatorInputPanel(composite,"MinDenom:",rule.getMinScaleDenominator());		
			minDenominatorPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {					
					double min = ((DenominatorInputPanel)event.getSource()).getDenominator();
					double max = rule.getMaxScaleDenominator();
					// verify that min<=max
					if(min>max)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid value for MinDenominator","MinDenominator cannot be larger then MaxDenominator");
						errorDialog.showError();
						minDenominatorPanel.setDenominator(rule.getMinScaleDenominator());
					}
					else
					{
						rule.setMinScaleDenominator(min);					
						Symbolizer symbolizers[] = rule.getSymbolizers();
						for(int i=0; i<symbolizers.length; i++){
							symbolizers[i].setMinScaleDenominator(min);
						}											
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					}
					focusedRuleItem = ruleTabFolder.getSelectionIndex();
				}
			});			

			// max denominator cannot be 0.0 as this would imply that the min denominator needs to be smaller than 0.0 -> does not make sense
			// hence, if no max denomiator specified, get the denominator of the individiual symbolizer			
			if(rule.getMaxScaleDenominator() == 0.0)
			{
				if(rule.getSymbolizers().length>0)
					rule.setMaxScaleDenominator(rule.getSymbolizers()[0].getMaxScaleDenominator());
				else
					rule.setMaxScaleDenominator(Double.parseDouble("9.0E99"));				
			}
			final DenominatorInputPanel maxDenominatorPanel = new DenominatorInputPanel(composite,"MaxDenom:",rule.getMaxScaleDenominator());
			maxDenominatorPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {					
					double max = ((DenominatorInputPanel)event.getSource()).getDenominator();										
					double min = rule.getMinScaleDenominator();
					// verify that min<=max
					if(min>max)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid value for MaxDenominator","MaxDenominator needs to be larger then MinDenominator");
						errorDialog.showError();
						maxDenominatorPanel.setDenominator(rule.getMaxScaleDenominator());
					}
					else
					{
						//add a minimum to max in order to be a little bit larger than the current scale and
						// to keep the current view -> otherwise the rule would automatically exculde this configuration
						max += 0.01;
						rule.setMaxScaleDenominator(max);
						Symbolizer symbolizers[] = rule.getSymbolizers();
						for(int i=0; i<symbolizers.length; i++){
							symbolizers[i].setMaxScaleDenominator(max);						
						}												
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					}
					focusedRuleItem = ruleTabFolder.getSelectionIndex();															
				}
			});
			
			AddSymbolizerPanel addSymbolizerPanel = new AddSymbolizerPanel(composite,"Symbolizer:",featureType);
			EditSymbolizerPanel editSymbolizerPanel = new EditSymbolizerPanel(composite,rule.getSymbolizers().length);			
						
			LegendLabel legendLabel = new LegendLabel(composite, userStyle, i);			

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
							focusedRuleItem = ruleTabFolder.getSelectionIndex();							
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
							focusedRuleItem = ruleTabFolder.getSelectionIndex();											
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
							focusedRuleItem = ruleTabFolder.getSelectionIndex();							
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
						focusedRuleItem = ruleTabFolder.getSelectionIndex();
						focusedSymbolizerItem = rule.getSymbolizers().length-1;						
						draw();
					}									
				}
			});
				
			// ***** Button Composite
			Composite buttonComposite = new Composite(composite, SWT.NULL);
			buttonComposite.setLayout(new GridLayout(3,true));
			Button button = new Button(buttonComposite,SWT.NULL);
			button.setText("Edit Filter");			
			final FilterDialog filterDialog = new FilterDialog(composite.getShell(),featureType,rule);
			filterDialog.addFilterDialogListener(new FilterDialogListener() {
				public void filterUpdated(FilterDialogEvent event) {						
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});
			button.addSelectionListener(new SelectionListener() {
				public void widgetSelected(SelectionEvent e) {
					int returnCode = filterDialog.open();						
				}
				public void widgetDefaultSelected(SelectionEvent e) {
					widgetSelected(e);
				}
			});
			
			// ******* SAVING THE SLD-STYLE
			final Button saveButton = new Button(buttonComposite,SWT.NULL);			
			saveButton.setText("Save");						
			saveButton.addSelectionListener(new SelectionListener() {
				public void widgetSelected(SelectionEvent e) {					
					SaveStyleAction.saveUserStyle(userStyle, composite.getShell());									
				}
				public void widgetDefaultSelected(SelectionEvent e) {
					widgetSelected(e);
				}
			});
			
			// ******* CREATE PATTERN
			final Button patternButton = new Button(buttonComposite,SWT.NULL);			
			patternButton.setText("Pattern");						
			patternButton.addSelectionListener(new SelectionListener() {
				public void widgetSelected(SelectionEvent e) 
				{		
					ArrayList list = new ArrayList();
				  	FeatureTypeProperty[] ftp = featureType.getProperties();    	
			    	for(int i=0; i<ftp.length; i++)   
			    	{    		    					    		
			    		if(ftp[i].getType().equalsIgnoreCase("java.lang.Double"))
			    			list.add(ftp[i]);		    				
		    			else if(ftp[i].getType().equalsIgnoreCase("java.math.BigInteger"))
		    				list.add(ftp[i]);
		    			else if(ftp[i].getType().equalsIgnoreCase("java.lang.Byte"))
		    				list.add(ftp[i]);  	
		    			else if(ftp[i].getType().equalsIgnoreCase("java.math.BigDecimal"))
		    				list.add(ftp[i]);
		    			else if(ftp[i].getType().equalsIgnoreCase("java.lang.Float"))
		    				list.add(ftp[i]); 
		    			else if(ftp[i].getType().equalsIgnoreCase("java.lang.Integer"))
		    				list.add(ftp[i]);  
		    			else if(ftp[i].getType().equalsIgnoreCase("java.lang.Long"))
		    				list.add(ftp[i]); 
		    			else if(ftp[i].getType().equalsIgnoreCase("java.lang.Short"))
		    				list.add(ftp[i]);		    			
			    	}		
			    	
			    	if(list.size()>0)
			    	{
			    		ArrayList ruleList = new ArrayList();
			    		FeatureTypeProperty prop = (FeatureTypeProperty)list.get(0);			    					    					    					    		
						BoundaryExpression upperBoundary = null; 
						BoundaryExpression lowerBoundary = null; 
						PropertyName propertyName = new PropertyName(prop.getName());
						PropertyIsBetweenOperation operation = null;
						
						// geometry stays of course
						Geometry geom = rule.getSymbolizers()[0].getGeometry();
											
						
			    		// 20 steps 
			    		for(int i=0; i<60; i++)
			    		{
			    			lowerBoundary = new BoundaryExpression(""+(i*1));
			    			upperBoundary = new BoundaryExpression(""+((i+1)*1));
			    			operation = new PropertyIsBetweenOperation(propertyName,lowerBoundary, upperBoundary); 			    			
			    			
			    			PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
							symb.setMinScaleDenominator(rule.getMinScaleDenominator());
							symb.setMaxScaleDenominator(rule.getMaxScaleDenominator());						
							symb.setGeometry(geom);
										    			
			    			Color color = new Color(600000*(i+1));
			    			Fill fill = StyleFactory.createFill(color);
			    			symb.setFill(fill);
			    			Symbolizer s[] = {symb};
			    			
			    			ruleList.add(StyleFactory.createRule(s,"-name-"+i,"-title-"+i,rule.getAbstract(),rule.getLegendGraphic(),new ComplexFilter(operation),rule.hasElseFilter(),rule.getMinScaleDenominator(), rule.getMaxScaleDenominator()));
			    			userStyle.getFeatureTypeStyles()[0].addRule(StyleFactory.createRule(s,"-name-"+i,"-title-"+i,rule.getAbstract(),rule.getLegendGraphic(),new ComplexFilter(operation),rule.hasElseFilter(),rule.getMinScaleDenominator(), rule.getMaxScaleDenominator()));
			    		}			    	
			    		userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			    		//System.out.println(userStyle.exportAsXML());
			    		draw();
			    	}
					
				}
				public void widgetDefaultSelected(SelectionEvent e) {
					widgetSelected(e);
				}
			});			
			
			// ******* DISPLAY ALL symbolizers
			if(rule.getSymbolizers().length == 0)
			{
				// add dummy invisilbe placeholder
				new SymbolizerTabItemBuilder(symbolizerTabFolder,null,userStyle,featureType);
			}
			else
			{
				for(int j=0; j<rule.getSymbolizers().length; j++){
					new SymbolizerTabItemBuilder(symbolizerTabFolder,rule.getSymbolizers()[j],userStyle,featureType); 								
				}	
			}
			if(rule.getSymbolizers().length == 0)
				symbolizerTabFolder.setVisible(false);
			if(focusedRuleItem == i && focusedSymbolizerItem!=-1)
				symbolizerTabFolder.setSelection(focusedSymbolizerItem);
		}
		if(focusedRuleItem != -1)
			ruleTabFolder.setSelection(focusedRuleItem);
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
}
