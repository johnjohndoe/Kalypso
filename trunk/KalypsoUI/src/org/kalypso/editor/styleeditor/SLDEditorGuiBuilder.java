/*
 * Created on 09.07.2004
 *
 */
package org.kalypso.editor.styleeditor;


import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.editor.styleeditor.panels.ControlRulePanel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;

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
	
	public SLDEditorGuiBuilder(Composite parent, ViewPart part)
	{	
		this.parent = parent;		
		buildSWTGui(null,null);		
	}
	
	public void buildSWTGui(final KalypsoUserStyle userStyle, final FeatureType featureType)
	{
		buildSWTGui(userStyle, featureType, -1);
	}
	
	public void buildSWTGui(final KalypsoUserStyle userStyle, final FeatureType featureType,final int index)
	{	
		if(index != -1)
			focusedRuleItem = index;
		if(scrollComposite != null)
			scrollComposite.dispose();
				
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
		ControlRulePanel controlRulePanel = new ControlRulePanel(mainComposite,"Rule:",rules.length);
		
		final RuleTabItemBuilder ruleTabItemBuilder = new RuleTabItemBuilder(mainComposite,getRules(userStyle),userStyle,featureType);		
		
		controlRulePanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				int action = ((ControlRulePanel)event.getSource()).getAction();
				switch(action){
					case ControlRulePanel.ADD_RULE:
					{
						Symbolizer symbolizers[] = null;
						Rule rule = StyleFactory.createRule(symbolizers);
						addRule(rule, userStyle);
						focusedRuleItem = rules.length;					
						buildSWTGui(userStyle, featureType);						
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
						break;
					}					
					case ControlRulePanel.REM_RULE:
					{
						int index = ruleTabItemBuilder.getSelectedRule();
						if(index>-1   &&   index<rules.length)
						{
							removeRule(index,userStyle);
							if(index >=0)
								focusedRuleItem = index;
							buildSWTGui(userStyle, featureType);
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
						break;
					}
					case ControlRulePanel.BAK_RULE:
					{
						int index = ruleTabItemBuilder.getSelectedRule();
						if(index >0)
						{													
							Rule newOrderedObjects[] = new Rule[rules.length];
							for(int i=0; i<rules.length; i++)
							{								
								if(i == index)
									newOrderedObjects[i] = rules[i-1];
								else if(i == (index-1))
									newOrderedObjects[i] = rules[i+1];
								else
									newOrderedObjects[i] = rules[i];											
							}
							setRules(newOrderedObjects,userStyle);
							focusedRuleItem = index-1;
							buildSWTGui(userStyle, featureType);						
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
						break;
					}
					case ControlRulePanel.FOR_RULE:
					{								
						int index = ruleTabItemBuilder.getSelectedRule();						
						if(index == (rules.length-1) || index <0){}
						else
						{																			
							Rule newOrderedObjects[] = new Rule[rules.length];
							for(int i=0; i<rules.length; i++)
							{								
								if(i == index)
									newOrderedObjects[i] = rules[i+1];
								else if(i == (index+1))
									newOrderedObjects[i] = rules[i-1];
								else
									newOrderedObjects[i] = rules[i];											
							}
							setRules(newOrderedObjects,userStyle);
							focusedRuleItem = index+1;	
							buildSWTGui(userStyle, featureType);
							userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));							
						}
					}					
					default: break;
				}
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
	
	private void removeRule(int index, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].removeRule(fts[0].getRules()[index]);
	}
	
	private void setRules(Rule[] rules, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].setRules(rules);
	}
	
	private void addRule(Rule rule, UserStyle style)
	{
		FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
		fts[0].addRule(rule);
	}	
}
