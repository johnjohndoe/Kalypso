/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


public class BetweenComparisonData extends AbstractComparisonData
{
	private String lower = null;
	private String upper = null;
	
	public String getLower() {
		return lower;
	}
	public void setLower(String lower) {
		this.lower = lower;
	}
	public String getUpper() {
		return upper;
	}
	public void setUpper(String upper) {
		this.upper = upper;
	}

	public boolean verify() {
		if(lower != null && upper != null && propertyName != null)
			return true;
		return false;
	}
}