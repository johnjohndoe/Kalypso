/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import org.eclipse.core.internal.resources.Workspace;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypso1d2d.pjt.IActiveProjectChangeListener;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.SimModelBasedContentProvider;


/**
 * @author Patrice Congo
 *
 */
public class SimulationModelDBView extends ViewPart
{

	static public final String ID=
				"org.kalypso.afgui.views.SimulationModelBasedView";
	
	private TreeViewer tv;
	
	private SimModelBasedContentProvider simModelBasedCP;
	
	private IActiveProjectChangeListener activeProjectChangeListener=
		new IActiveProjectChangeListener()
	{

		public void activeProjectChanged(IProject newProject)
		{
			
			tv.refresh();
		}
		
	};
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		tv= new TreeViewer(parent,SWT.FILL);
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		
	}
	
}
