/*
 * Created on 26.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.editor.styleeditor.dialogs.filterencoding;

import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.services.wfs.filterencoding.Expression_Impl;

/**
 * @author Administrator
 *
 */
public class BoundaryExpression extends Expression_Impl{

	private String value = null;
	
	public BoundaryExpression(String value)
	{
		this.value = value;
	}
	
	public StringBuffer toXML() {
		return new StringBuffer(value.toString());
	}
	
	public Object evaluate(Feature feature) throws FilterEvaluationException 
	{    
		Double returnValue = null;
		try{
			returnValue = new Double(value);
		}
		catch (NumberFormatException e) {
			 throw new FilterEvaluationException (
	                "BoundaryExpression:  can only be applied to numerical "
	              + "expressions!");
		}		
		return returnValue;
	}
		
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
}
