/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;


public class LikeComparisonData extends AbstractComparisonData
{
	private String literal = null;
	private char escapeChar = '\\';
	private char singleChar = '?';
	private char wildCard = '*';		
		
	public String getLiteral() {
		return literal;
	}
	public void setLiteral(String literal) {
		this.literal = literal;
	}
	
	public boolean verify()
	{
		if(literal != null && propertyName != null)
			return true;
		return false;	
	}
	public char getEscapeChar() {
		return escapeChar;
	}
	public void setEscapeChar(char escapeChar) {
		this.escapeChar = escapeChar;
	}
	public char getSingleChar() {
		return singleChar;
	}
	public void setSingleChar(char singleChar) {
		this.singleChar = singleChar;
	}
	public char getWildCard() {
		return wildCard;
	}
	public void setWildCard(char wildCard) {
		this.wildCard = wildCard;
	}
}