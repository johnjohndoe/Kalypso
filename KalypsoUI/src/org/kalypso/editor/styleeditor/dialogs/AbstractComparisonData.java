/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


public abstract class AbstractComparisonData extends AbstractData
{
	protected String propertyName = null;
	

	public String getPropertyName() {
		return propertyName;
	}
	public void setPropertyName(String propertyName) {
		this.propertyName = propertyName;
	}
	
	public abstract boolean verify(); 
}