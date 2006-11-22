package org.kalypso.afgui.model;

/**
 * Interface to be implemented by help resources.
 * 
 * @author Patrice Congo
 */

public interface IHelp
{
	public enum HELP_TYPE {
			PLAIN_TEXT,
			HTML
	};
	
	public String getHelp();
	
	public HELP_TYPE getHelpType();
}
