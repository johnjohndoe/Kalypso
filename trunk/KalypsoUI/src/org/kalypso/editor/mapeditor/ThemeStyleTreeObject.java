/*
 * Created on 22.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.editor.mapeditor;

import org.deegree.graphics.sld.UserStyle;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class ThemeStyleTreeObject {
	private final KalypsoUserStyle myStyle;

	private final KalypsoTheme myTheme;

	public ThemeStyleTreeObject(KalypsoTheme theme, UserStyle style)
	{
		myTheme = (KalypsoTheme)theme;
		myStyle = (KalypsoUserStyle)style;
	}
	/**
	 *  
	 */
	public ThemeStyleTreeObject(KalypsoTheme theme, KalypsoUserStyle style)
	{
		myTheme = theme;
		myStyle = style;
	}

	public KalypsoUserStyle getStyle() {
		return myStyle;
	}

	public KalypsoTheme getTheme() {
		return myTheme;
	}

	public String toString()
	{
		if(myStyle.getName()!=null)
			return myStyle.getName();
		return myStyle.toString();
	}
}