/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;


public class BinaryComparisonData extends AbstractComparisonData
{
	private String literal = null;
	public String getLiteral() {
		return literal;
	}
	public void setLiteral(String literal) {
		this.literal = literal;
	}

	public boolean verify() {
		if(literal != null && propertyName != null)
			return true;
		return false;				
	}
}