package org.kalypso.afgui.model.internal;

import org.kalypso.afgui.model.IHelp;

public class Help implements IHelp {
	final private String helpString;

	final private HELP_TYPE helpType;

	public Help(String helpString, HELP_TYPE helpType) {
		this.helpString = helpString;
		this.helpType = helpType;
	}

	public String getHelp() {
		return helpString;
	}

	public HELP_TYPE getHelpType() {
		return helpType;
	}

}
