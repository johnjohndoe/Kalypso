/*
 * Created on 26.07.2004
 *
 */
package org.kalypso.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Symbolizer;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 *
 */

public class RasterSymbolizerLayout extends SymbolizerLayout{	
	
	public RasterSymbolizerLayout(Composite composite, Symbolizer symbolizer, KalypsoUserStyle userStyle)
	{
		super(composite,symbolizer,userStyle);
	}
}
