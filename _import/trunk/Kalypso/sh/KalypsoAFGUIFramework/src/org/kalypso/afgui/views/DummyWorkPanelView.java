/**
 * 
 */
package org.kalypso.afgui.views;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;;

/**
 * @author congo
 *
 */
public class DummyWorkPanelView extends ViewPart
{
	static public final String ID="org.kalypso.afgui.views.DummyWorkPanelView";
	
	private TableViewer tv;
	
	@Override
	public void createPartControl(Composite parent)
	{
		tv= new TableViewer(parent, SWT.FILL);
	}

	@Override
	public void setFocus()
	{
		
	}
	

}
