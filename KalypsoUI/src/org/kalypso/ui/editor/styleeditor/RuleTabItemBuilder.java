/*
 * Created on 12.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;

import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureType;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.BoundaryExpression;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.editor.styleeditor.RuleCollection;
import org.kalypso.editor.styleeditor.RuleFilterCollection;
import org.kalypso.editor.styleeditor.panels.ColorPalettePanel;
import org.kalypso.editor.styleeditor.panels.RulePatternInputPanel;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;
import org.kalypso.ui.editor.styleeditor.dialogs.filterdialog.FilterDialog;
import org.kalypso.ui.editor.styleeditor.dialogs.filterdialog.FilterDialogEvent;
import org.kalypso.ui.editor.styleeditor.dialogs.filterdialog.FilterDialogListener;
import org.kalypso.ui.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.DenominatorInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.EditSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.LegendLabel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.TextInputPanel;

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
//	private Rule[] rules = null;
	private TabFolder ruleTabFolder = null;
	
	private KalypsoUserStyle userStyle = null;
	private FeatureType featureType = null;
		
	private int focusedRuleItem = -1;
	private int focusedSymbolizerItem = -1;
	private RuleFilterCollection rulePatternCollection = null;
	
	public RuleTabItemBuilder(Composite composite, RuleFilterCollection rulePatternCollection, KalypsoUserStyle userStyle, IKalypsoLayer layer)
	{	
		this.parent = composite; 
		this.userStyle = userStyle;		
		if(layer != null)
			this.featureType = ((KalypsoFeatureLayer)layer).getFeatureType(); 			
		this.rulePatternCollection = rulePatternCollection;
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
		
		// needs to exist otherwise tab-folder expands arbitrarily
		final Composite innerTabFolderComposite = new Composite(tabFolderComposite,SWT.NULL);		
		innerTabFolderComposite.layout();
		
		ruleTabFolder = new TabFolder(innerTabFolderComposite,SWT.NULL);		
		FormData RuleTableFolderLData = new FormData();
		RuleTableFolderLData.height = 577;
		RuleTableFolderLData.width = 245;		
		RuleTableFolderLData.top =  new FormAttachment(10, 1000, 0);
		ruleTabFolder.setLayoutData(RuleTableFolderLData);	
		ruleTabFolder.setSize(new org.eclipse.swt.graphics.Point(245,577));											
		
		ArrayList filteredRules = rulePatternCollection.getFilteredRuleCollection();		
		for(int j = 0; j<filteredRules.size(); j++)
		{
			Object ruleObject = filteredRules.get(j);
			if(ruleObject instanceof Rule)			
				drawRule((Rule)ruleObject,j);				
			else if(ruleObject instanceof RuleCollection)
				drawPatternRule((RuleCollection)ruleObject, j);						
		}

		if(focusedRuleItem != -1)
			ruleTabFolder.setSelection(focusedRuleItem);
		if(filteredRules.size() == 0)
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
	
	public void drawRule(final Rule rule, int i)
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
				rule.setMaxScaleDenominator(Double.MAX_VALUE);				
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
		buttonComposite.setLayout(new GridLayout(1,true));
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
		
		composite.pack(true);
	}
	
	
	
	
// -----------------------------------
	
	double minValue = -1;
	double maxValue = -1;				
	double step = -1;
	Color[] colorArray = null;
	ColorPalettePanel colorPalettePanel = null;
	
	public void drawPatternRule(final RuleCollection ruleCollection, int index)
	{
		// 1. get global values for name, minDen, maxDen,
		if(ruleCollection.size() == 0)
			return;
		final Rule tmpRule = ruleCollection.get(0);			
		
		String rulePatternName = tmpRule.getName();
		double rulePatternMinDenom = tmpRule.getMinScaleDenominator();
		double rulePatternMaxDenom = tmpRule.getMaxScaleDenominator();
		int patternType = -1;
		
		// 2. Begin to draw the first lines
		TabItem tabItem = new TabItem(ruleTabFolder, SWT.NULL);	
		final Composite composite = new Composite(ruleTabFolder,SWT.NULL);		
		GridLayout compositeLayout = new GridLayout();			
		composite.setSize(270,400);
		composite.setLayout(compositeLayout);			
		compositeLayout.marginWidth = 5;
		compositeLayout.marginHeight = 5;		
		composite.layout();				
		tabItem.setControl(composite);		
		tabItem.setText("Pattern:"+rulePatternName);
		
		final TabFolder symbolizerTabFolder;
		
		TextInputPanel nameInputPanel = new TextInputPanel(composite,"Name:",rulePatternName);
		nameInputPanel.addPanelListener(new PanelListener() {				
			public void valueChanged(PanelEvent event) {
				String name = ((TextInputPanel)event.getSource()).getLabelText();
				if(name == null || name.trim().length() == 0)
				{
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid input value","Name should not be null");
					errorDialog.showError();											
				}
				else
				{
					for(int counter=0; counter<ruleCollection.size();counter++)					
						ruleCollection.get(counter).setName(name);					
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
				focusedRuleItem = ruleTabFolder.getSelectionIndex();																			
			}
		});
		
		final DenominatorInputPanel minDenominatorPanel = new DenominatorInputPanel(composite,"MinDenom:",rulePatternMinDenom);		
		minDenominatorPanel.addPanelListener(new PanelListener() {				
			public void valueChanged(PanelEvent event) {					
				double min = ((DenominatorInputPanel)event.getSource()).getDenominator();
				double max = tmpRule.getMaxScaleDenominator();
				// verify that min<=max
				if(min>max)
				{
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid value for MinDenominator","MinDenominator cannot be larger then MaxDenominator");
					errorDialog.showError();
					minDenominatorPanel.setDenominator(tmpRule.getMinScaleDenominator());
				}
				else
				{
					for(int counter=0; counter<ruleCollection.size();counter++)
					{
						ruleCollection.get(counter).setMinScaleDenominator(min);					
						Symbolizer symbolizers[] = ruleCollection.get(counter).getSymbolizers();
						for(int i=0; i<symbolizers.length; i++){
							symbolizers[i].setMinScaleDenominator(min);
						}	
					}
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
				focusedRuleItem = ruleTabFolder.getSelectionIndex();
			}
		});			

		// max denominator cannot be 0.0 as this would imply that the min denominator needs to be smaller than 0.0 -> does not make sense
		// hence, if no max denomiator specified, get the denominator of the individiual symbolizer			
		if(tmpRule.getMaxScaleDenominator() == 0.0)
		{
			if(tmpRule.getSymbolizers().length>0)
				tmpRule.setMaxScaleDenominator(tmpRule.getSymbolizers()[0].getMaxScaleDenominator());
			else
				tmpRule.setMaxScaleDenominator(Double.MAX_VALUE);				
		}
		final DenominatorInputPanel maxDenominatorPanel = new DenominatorInputPanel(composite,"MaxDenom:",rulePatternMaxDenom);
		maxDenominatorPanel.addPanelListener(new PanelListener() {				
			public void valueChanged(PanelEvent event) {					
				double max = ((DenominatorInputPanel)event.getSource()).getDenominator();										
				double min = tmpRule.getMinScaleDenominator();
				// verify that min<=max
				if(min>max)
				{
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Invalid value for MaxDenominator","MaxDenominator needs to be larger then MinDenominator");
					errorDialog.showError();
					maxDenominatorPanel.setDenominator(tmpRule.getMaxScaleDenominator());
				}
				else
				{
					//add a minimum to max in order to be a little bit larger than the current scale and
					// to keep the current view -> otherwise the rule would automatically exculde this configuration
					max += 0.01;
					for(int counter=0; counter<ruleCollection.size();counter++)
					{
						ruleCollection.get(counter).setMaxScaleDenominator(max);					
						Symbolizer symbolizers[] = ruleCollection.get(counter).getSymbolizers();
						for(int i=0; i<symbolizers.length; i++){
							symbolizers[i].setMaxScaleDenominator(max);
						}	
					}												
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
				focusedRuleItem = ruleTabFolder.getSelectionIndex();															
			}
		});		
		
		
		// 3. getFilterType -> at the moment we assume only a pattern of PropertyIsBetween
		// draw the pattern line
		Filter filter = tmpRule.getFilter();
		// must be a complex filter -> then we can find out what operation-id is has
		if(filter instanceof ComplexFilter)
		{	
			// if PropertyIsBetween
			if(((ComplexFilter)filter).getOperation().getOperatorId() == OperationDefines.PROPERTYISBETWEEN)
			{
				patternType = OperationDefines.PROPERTYISBETWEEN;
				// find out the settings of the filter - min, max and step values

				boolean valuesHaveChanged = false;
				for(int j=0; j<ruleCollection.size(); j++)
				{
					// verify again that it is a complexFilter and of type PropertyIs Between for every rule
					if(ruleCollection.get(j).getFilter() instanceof ComplexFilter)
					{
						Operation ruleOperation = ((ComplexFilter)ruleCollection.get(j).getFilter()).getOperation();
						if(ruleOperation.getOperatorId() == OperationDefines.PROPERTYISBETWEEN)
						{
							PropertyIsBetweenOperation isBetweenOperation = (PropertyIsBetweenOperation)ruleOperation;
							if(j == 0)
							{
								minValue = Double.parseDouble(((BoundaryExpression) isBetweenOperation.getLowerBoundary()).getValue());
								maxValue = Double.parseDouble(((BoundaryExpression) isBetweenOperation.getUpperBoundary()).getValue());
								step = maxValue - minValue;
								valuesHaveChanged = true;
							}
							else
							{
								double tmpMinValue = Double.parseDouble(((BoundaryExpression) isBetweenOperation.getLowerBoundary()).getValue());
								double tmpMaxValue = Double.parseDouble(((BoundaryExpression) isBetweenOperation.getUpperBoundary()).getValue());
								double tmpStep = tmpMaxValue - tmpMinValue;
								if(tmpMinValue<minValue)
									minValue = tmpMinValue;
								if(tmpMaxValue>maxValue)
									maxValue = tmpMaxValue;
							}														
						}
					}									
				}
				RulePatternInputPanel rulePatternInputPanel = new RulePatternInputPanel(composite,"Pattern:",minValue,maxValue,step);
				rulePatternInputPanel.addPanelListener(new PanelListener() {
					public void valueChanged(PanelEvent event) {
						RulePatternInputPanel panel = (RulePatternInputPanel)event.getSource();
						// reset the values for all rules in this pattern if step did not change !!!!												
						minValue = panel.getMin();
						maxValue = panel.getMax();				
						step = panel.getStep();
						
						 
						// first create new rules															    					    					    					    					    
						BoundaryExpression upperBoundary = null; 
						BoundaryExpression lowerBoundary = null; 
						ArrayList ruleList = new ArrayList();
						// TODO ID hardgecodet
						PropertyName propertyName = new PropertyName("ID");
						PropertyIsBetweenOperation operation = null;						
//						TODO ID hardgecodet														
						String[] geometryObjects = AddSymbolizerPanel.getGeometries(featureType);
						if(geometryObjects.length>0)
						{
							// I choose to use a ploygon-symbolier hopeing that it works			
							Symbolizer symbo = AddSymbolizerPanel.getSymbolizer(geometryObjects[0],"Polygon",featureType);
							Geometry geom = symbo.getGeometry();																			
							
							String patternTitle = ruleCollection.get(0).getTitle();
											
							int counter = (int)Math.ceil((maxValue-minValue)/step);
							colorArray = ColorPalettePanel.initializeColors(ColorPalettePanel.BLUE_GREEN_TRANSITION,counter);
				    		for(int i=0; i<counter; i++)
				    		{			    		    			    			
				    			lowerBoundary = new BoundaryExpression(""+(minValue +(i*step)));
				    			if((minValue +((i+1)*step))>maxValue)
				    				upperBoundary = new BoundaryExpression(""+maxValue);
				    			else
				    				upperBoundary = new BoundaryExpression(""+(minValue +((i+1)*step)));
				    			operation = new PropertyIsBetweenOperation(propertyName,lowerBoundary, upperBoundary); 				    			
				    			PolygonSymbolizer symb = new PolygonSymbolizer_Impl();											
								symb.setGeometry(geom);																																																
				    			Fill fill = StyleFactory.createFill(new java.awt.Color(colorArray[i].getRed(),colorArray[i].getGreen(),colorArray[i].getBlue()));
				    			symb.setFill(fill);
				    			Symbolizer s[] = {symb};				    			
				    			ruleList.add(StyleFactory.createRule(s,"-name-"+i,patternTitle,"abstract",null,new ComplexFilter(operation),false,symb.getMinScaleDenominator(),symb.getMaxScaleDenominator()));				    			
				    		}
						}	
						
						// then remove old ones
						int collSize = ruleCollection.size()-1;						
						for(int i=collSize; i>=0; i--)
						{							
							removeRule(ruleCollection.get(i), userStyle);		
							rulePatternCollection.removeRule(ruleCollection.get(i));							
						}
												
						// add new ones
						for(int j=0; j<ruleList.size(); j++)
						{
							rulePatternCollection.addRule((Rule)ruleList.get(j));
							userStyle.getFeatureTypeStyles()[0].addRule((Rule)ruleList.get(j));
						}		
						// update ColorPalettePanel	
						colorPalettePanel.setColorPalette(colorArray);
						// update
						draw();
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					}
				});
			}						
		}
		else
			return;
							
//		AddSymbolizerPanel addSymbolizerPanel = new AddSymbolizerPanel(composite,"Symbolizer:",featureType);
//		addSymbolizerPanel.addPanelListener(new PanelListener() {
//			public void valueChanged(PanelEvent event) {
//				Symbolizer symbolizer = ((AddSymbolizerPanel)event.getSource()).getSelection();
//				if(symbolizer instanceof Symbolizer)
//				{
////					rule.addSymbolizer(symbolizer);											
////					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
////					focusedRuleItem = ruleTabFolder.getSelectionIndex();
////					focusedSymbolizerItem = rule.getSymbolizers().length-1;						
////					draw();
//				}									
//			}
//		});		
		
		int numberOfColors = (int)Math.ceil((maxValue-minValue)/step);
		if(colorPalettePanel == null)
			colorPalettePanel = new ColorPalettePanel(composite,colorArray, numberOfColors);
		else
			colorPalettePanel.draw(composite);
		colorPalettePanel.addColorPalettePanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {				
				colorArray = colorPalettePanel.getColorPalette();
				for(int i=0; i<ruleCollection.size(); i++)
				{
					Symbolizer[] symb = ruleCollection.get(i).getSymbolizers();
					for(int j=0; j<symb.length; j++)
					{
						if(symb[j] instanceof PolygonSymbolizer)
							((PolygonSymbolizer)symb[j]).setFill(StyleFactory.createFill(new java.awt.Color(colorArray[i].getRed(),colorArray[i].getGreen(),colorArray[i].getBlue())));
					}
				}
				draw();
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});
		
		
		focusedRuleItem = index;
		composite.pack(true);
	}
	
	private void removeRule(Rule rule, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].removeRule(rule);
	}	
	
}
