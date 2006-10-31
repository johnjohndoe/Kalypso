package org.kalypso.afgui.model;

public interface IActivityHelp
{
	static final int HELP_TYPE_PLAIN_TXT=0;
	static final int HELP_TYPE_HTML=1;
	
	
	/**
	 * To get the String representation of the help
	 * @return
	 */
	public String getHelp();
	
	/**
	 * Returns the type of the help.
	 * The help can be plain text or html.
	 * 
	 * @return the type code for the help:
	 * <ul>
	 * 	<li/>{@link IActivityHelp#HELP_TYPE_HTML} for html help
	 * 	<li/>{@link IActivityHelp#HELP_TYPE_PLAIN_TXT} forplain text help
	 * </ul>
	 */
	public int getType();
}
