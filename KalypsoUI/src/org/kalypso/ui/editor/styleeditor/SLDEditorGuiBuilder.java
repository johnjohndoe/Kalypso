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
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
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
import org.kalypso.editor.styleeditor.RuleCollection;
import org.kalypso.editor.styleeditor.RuleFilterCollection;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.ui.editor.styleeditor.dialogs.filterencoding.BoundaryExpression;
import org.kalypso.ui.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.ControlRulePanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;

/**
 * @author Administrator
 *
 */
public class SLDEditorGuiBuilder {
	
	private KalypsoUserStyle userStyle = null;
	private FeatureType featureType = null;	
	private Composite parent = null;
	private ScrolledComposite scrollComposite = null;	
	private Label titleLabel = null;
	private int focusedRuleItem = -1;
	private RuleFilterCollection rulePatternCollection = null;
	
	public SLDEditorGuiBuilder(Composite parent)
	{	
		this.parent = parent;		
		buildSWTGui(null,null);		
	}
	
	public void buildSWTGui(final KalypsoUserStyle userStyle, IKalypsoLayer layer)
	{
		buildSWTGui(userStyle, layer, -1);
	}
	
	public void buildSWTGui(final KalypsoUserStyle userStyle, final IKalypsoLayer layer,final int index)
	{	
		if(index != -1)
			focusedRuleItem = index;
		if(scrollComposite != null)
			scrollComposite.dispose();
		
		// get FeatureType from layer
		if(layer != null)
			featureType = ((KalypsoFeatureLayer)layer).getFeatureType(); 		 
	       	         			
		scrollComposite = new ScrolledComposite(parent, SWT.H_SCROLL | SWT.V_SCROLL);				
		Composite mainComposite = new Composite(scrollComposite, SWT.NONE);		
		mainComposite.setLayout(new GridLayout());		
		mainComposite.layout();			
		scrollComposite.setContent(mainComposite);		
		scrollComposite.setSize(parent.getSize());		
		Label nameLabel = null;
		if(userStyle == null)
		{
			nameLabel = new Label(mainComposite, 0);
			nameLabel.setText("No style found to show in editor");
			mainComposite.pack(true);		
			return;
		}
		else
		{
			titleLabel = new Label(mainComposite, SWT.NULL);
			titleLabel.setText("Style Editor v1.0");
			titleLabel.setFont(new Font(null, "Arial",12,SWT.BOLD));						
			nameLabel = new Label(mainComposite, 0);
			nameLabel.setText("Style: "+userStyle.getName());
			nameLabel.setFont(new Font(null, "Arial",8,SWT.BOLD));			
		}
		final Rule[] rules = getRules(userStyle);
		rulePatternCollection = RuleFilterCollection.getInstance();
		// filter patterns from rules and draw them afterwards
		for(int i= 0;i<rules.length; i++)
		{																
			rulePatternCollection.addRule(rules[i]);		
		}
		
		ControlRulePanel controlRulePanel = new ControlRulePanel(mainComposite,"Rule:",rulePatternCollection.size());
		
		final RuleTabItemBuilder ruleTabItemBuilder = new RuleTabItemBuilder(mainComposite,rulePatternCollection,userStyle,layer);		
		
		controlRulePanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				int action = ((ControlRulePanel)event.getSource()).getAction();
				switch(action){
					case ControlRulePanel.ADD_RULE:
					{
						Symbolizer symbolizers[] = null;
						Rule rule = StyleFactory.createRule(symbolizers);
						addRule(rule, userStyle);
						focusedRuleItem = rulePatternCollection.size();					
						buildSWTGui(userStyle, layer);						
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
						break;
					}					
					case ControlRulePanel.REM_RULE:
					{
						int index = ruleTabItemBuilder.getSelectedRule();						
						if(index>-1   &&   index<rulePatternCollection.size())
						{
							Object ruleObject = rulePatternCollection.getFilteredRuleCollection().get(index);
							if(ruleObject instanceof Rule)
							{
								removeRule((Rule)ruleObject,userStyle);
							}
							else if(ruleObject instanceof RuleCollection)
							{								
								RuleCollection ruleCollection = (RuleCollection) ruleObject;
								for(int i=0; i<ruleCollection.size(); i++)
									removeRule(ruleCollection.get(i), userStyle);
							}
							
							if(index >=0)
								focusedRuleItem = --index;
							buildSWTGui(userStyle, layer);
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
						break;
					}
					case ControlRulePanel.BAK_RULE:
					{
						int index = ruleTabItemBuilder.getSelectedRule();
						if(index >0)
						{		
							ArrayList newOrdered = new ArrayList();																				
							for(int i=0; i<rulePatternCollection.size(); i++)
							{								
								if(i == index)
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i-1));
								else if(i == (index-1))
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i+1));									
								else
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i));											
							}
							setRules(newOrdered,userStyle);
							focusedRuleItem = index-1;
							buildSWTGui(userStyle, layer);						
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
						break;
					}
					case ControlRulePanel.FOR_RULE:
					{								
						int index = ruleTabItemBuilder.getSelectedRule();						
						if(index == (rulePatternCollection.size()-1) || index <0){}
						else
						{																										
							ArrayList newOrdered = new ArrayList();
							for(int i=0; i<rulePatternCollection.size(); i++)
							{								
								if(i == index)
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i+1));									
								else if(i == (index+1))
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i-1));
								else
									newOrdered.add(rulePatternCollection.getFilteredRuleCollection().get(i));											
							}
							setRules(newOrdered,userStyle);
							focusedRuleItem = index+1;	
							buildSWTGui(userStyle, layer);
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
					}					
					default: break;
				}
			}
		});			
		
		// ***** Button Composite
		final Composite buttonComposite = new Composite(mainComposite, SWT.NULL);
		buttonComposite.setLayout(new GridLayout(2,true));
		
		// ******* SAVING THE SLD-STYLE
		final Button saveButton = new Button(buttonComposite,SWT.NULL);			
		saveButton.setText("Save");						
		saveButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {					
				SaveStyleAction.saveUserStyle(userStyle, buttonComposite.getShell());									
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
					
					if(layer instanceof KalypsoFeatureLayer)
					{
						KalypsoFeature[] fts = ((KalypsoFeatureLayer)layer).getAllFeatures();
						double minValue = -1;
						double maxValue = -1;
						double value;
						boolean hasFeatures = false;
						if(fts.length>0)
						{
							value = Double.parseDouble(fts[0].getProperty(prop.getName()).toString());
				        	minValue = value;
				        	maxValue = value;	
				        	hasFeatures = true;
						}
						// TODO: Need to check whether there are features??? otherwise no min,max value
				        for(int i=0; i<fts.length; i++)
				        {
				        	value = Double.parseDouble(fts[i].getProperty(prop.getName()).toString());
				        	if(value < minValue)
				        		minValue = value;
				        	else if(value>maxValue)
				        		maxValue = value;				        					        					        	
				        }
				        if(hasFeatures)
				        	System.out.println("Min: "+ minValue + "  Max: "+ maxValue );
					}									
					
					String[] geometryObjects = AddSymbolizerPanel.getGeometries(featureType);
					if(geometryObjects.length>0)
					{
						// I choose to use a ploygon-symbolier hopeing that it works			
						Symbolizer symbo = AddSymbolizerPanel.getSymbolizer(geometryObjects[0],"Polygon",featureType);
						Geometry geom = symbo.getGeometry();																			
						
						String patternTitle = "-title-" + new Date().getTime();
						
						
			    		for(int i=0; i<5; i++)
			    		{			    		    			    			
			    			lowerBoundary = new BoundaryExpression(""+(i*1));
			    			upperBoundary = new BoundaryExpression(""+((i+1)*1));
			    			operation = new PropertyIsBetweenOperation(propertyName,lowerBoundary, upperBoundary); 			    			
			    			
			    			PolygonSymbolizer symb = new PolygonSymbolizer_Impl();											
							symb.setGeometry(geom);
										    			
			    			Color color = new Color(600000*(i+1));
			    			Fill fill = StyleFactory.createFill(color);
			    			symb.setFill(fill);
			    			Symbolizer s[] = {symb};
			    			
			    			ruleList.add(StyleFactory.createRule(s,"-name-"+i,patternTitle,"abstract",null,new ComplexFilter(operation),false,symb.getMinScaleDenominator(),symb.getMaxScaleDenominator()));
			    			userStyle.getFeatureTypeStyles()[0].addRule(StyleFactory.createRule(s,"-name-"+i,patternTitle,"abstract",null,new ComplexFilter(operation),false,symb.getMinScaleDenominator(),symb.getMaxScaleDenominator()));
			    		}			    	
			    		userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			    		//System.out.println(userStyle.exportAsXML());
			    		buildSWTGui(userStyle, layer);
					}
		    	}
				
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});	
		
		ruleTabItemBuilder.draw();
		if(focusedRuleItem>-1)
			ruleTabItemBuilder.setSelectedRule(focusedRuleItem);
		mainComposite.pack(true);		
	}
	
	private Rule[] getRules(UserStyle style){
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		// TODO Pay attention that it is currently limited to the first fts
		return fts[0].getRules();		
	}	
	
	private void removeRule(Rule rule, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].removeRule(rule);
	}	
	
	private void setRules(ArrayList ruleObjects, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		ArrayList ruleInstances = new ArrayList();
		for(int i=0; i<ruleObjects.size(); i++)
		{
			if(ruleObjects.get(i) instanceof Rule)
				ruleInstances.add((Rule)ruleObjects.get(i));
			else if(ruleObjects.get(i) instanceof RuleCollection)
			{
				RuleCollection ruleCollection = (RuleCollection) ruleObjects.get(i); 
				for(int j=0; j<ruleCollection.size(); j++)
					ruleInstances.add(ruleCollection.get(j));
			}
		}
		Rule[] ruleArray = new Rule[ruleInstances.size()];
		for(int c=0; c<ruleInstances.size(); c++)
			ruleArray[c] = (Rule)ruleInstances.get(c);		
		fts[0].setRules(ruleArray);
	}
	
	private void addRule(Rule rule, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].addRule(rule);
	}	
}
