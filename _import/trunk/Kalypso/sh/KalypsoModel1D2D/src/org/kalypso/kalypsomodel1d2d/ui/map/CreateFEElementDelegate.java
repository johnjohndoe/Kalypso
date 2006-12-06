/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.ui.map;

import org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractGisMapEditorActionDelegate;

/**
 * @author Gernot Belger
 */
public class CreateFEElementDelegate extends AbstractGisMapEditorActionDelegate {
	public CreateFEElementDelegate() {
		super(new FE2DElementEditWidget());
	}
}
