/*
 * Created on 12.07.2004
 *
 */
package org.kalypso.editor.styleeditor;

import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.graphics.sld.Rule_Impl;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsLikeOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.EditSymbolizerPanel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.editor.styleeditor.panels.SliderPanel;
import org.kalypso.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;

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
		
		tabFolderComposite = new Composite(globalComposite, SWT.EMBEDDED);	
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
			Composite composite = new Composite(ruleTabFolder,SWT.NULL);		
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
					userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));									
					focuedRuleItem = ruleTabFolder.getSelectionIndex();					
					draw();
				}
			});
			
			TextInputPanel nameInputPanel = new TextInputPanel(composite,"Name:",rule.getName());
			nameInputPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					rule.setName(((TextInputPanel)event.getSource()).getLabelText());
				
					
					
					  PropertyIsLikeOperation operation = new PropertyIsLikeOperation(new PropertyName("GEOM"),new Literal("test"),'*','a','b');
					  ComplexFilter filter = new ComplexFilter(operation);
					  rule.setFilter(filter);
					 
						userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
						System.out.println(((Rule_Impl)rule).exportAsXML());
					
					focuedRuleItem = ruleTabFolder.getSelectionIndex();	
					draw();
				}
			});
			
			SliderPanel minDenominatorPanel = new SliderPanel(composite,"MinDenomiator:",0,10000000,100000,SliderPanel.INTEGER,rule.getMinScaleDenominator());
			minDenominatorPanel.addPanelListener(new PanelListener() {				
				public void valueChanged(PanelEvent event) {
					rule.setMinScaleDenominator(((SliderPanel)event.getSource()).getSelection());
					userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
					focuedRuleItem = ruleTabFolder.getSelectionIndex();	
					draw();
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
					userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
					
					draw();
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
							userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));											
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
							userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
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
							userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
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
						userStyle.fireModellEvent(new ModellEvent(ModellEvent.STYLE_CHANGE));
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
//
//				}
//				public void widgetDefaultSelected(SelectionEvent e) {
//					widgetSelected(e);
//				}
//			});
			
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
		
		//ruleTabFolder.pack(true);
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
