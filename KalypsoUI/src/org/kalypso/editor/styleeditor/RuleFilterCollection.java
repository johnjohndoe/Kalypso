/*
 * Created on 12.09.2004
 *
 */
package org.kalypso.editor.styleeditor;

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.graphics.sld.Rule;

/**
 * @author Administrator

 */
/* This class is fed with rules. It identifies whether it is a normal rule or the
 * rule belongs to a pattern. It collects the rules and returns the number of rule
 * items (-> number of tabitems to be displayed) as a list of Rule and RuleCollection Objects.
 */

public class RuleFilterCollection 
{
	private HashMap patterns = null;
	private ArrayList filteredRuleCollection = null;
	
	private RuleFilterCollection()
	{
		patterns = new HashMap();
		filteredRuleCollection = new ArrayList();
	}
	
	public static RuleFilterCollection getInstance()
	{		
		return new RuleFilterCollection();		
	}		
	
	public void addRule(Rule rule)
	{
		// the title of a rule serves as key for the hashMap
		String key = rule.getTitle();
		// it it is a pattern, add to ruleCollection
		if(key != null && key.startsWith("-title-"))
		{
            // 1. check whether there is already a rule collection with this rule
			if(patterns.containsKey(key))
			{
				// if yes - add rule to collection
				((RuleCollection)patterns.get(key)).addRule(rule);								
			}
			else
			{
				RuleCollection ruleCollection = RuleCollection.getInstance(rule);
				patterns.put(key,ruleCollection);
				filteredRuleCollection.add(ruleCollection);
			}						
		}
		else
		{
			filteredRuleCollection.add(rule);
		}				
	}
	
	public void removeRule(Rule rule)
	{
		// the title of a rule serves as key for the hashMap
		String key = rule.getTitle();
		// it it is a pattern, add to ruleCollection
		if(key != null && key.startsWith("-title-"))
		{
            // 1. check whether there is already a rule collection with this rule
			if(patterns.containsKey(key))
			{
				// if yes - add rule to collection
				((RuleCollection)patterns.get(key)).removeRule(rule);					
			}							
		}
		else
		{
			filteredRuleCollection.remove(rule);
		}		
	}
	
	public void removeRuleCollection(RuleCollection coll)
	{
		filteredRuleCollection.remove(coll);
		patterns.remove(coll.getId());
	}
	
	public ArrayList getFilteredRuleCollection()
	{
		return filteredRuleCollection;
	}
	
	
	
	public int size()
	{
		return filteredRuleCollection.size();
	}
}
