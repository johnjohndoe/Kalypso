/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.db.IWorkflowDBChangeListerner;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
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
	
	private ActiveWorkContext activeWorkContext= 
						ActiveWorkContext.getInstance();
	
	private IActiveProjectChangeListener activeProjectChangeListener=
		new IActiveProjectChangeListener()
	{

		public void activeProjectChanged(
						IProject newProject, 
						IProject olProject,
						IWorkflowDB oldWorkflowDB,
						IWorkflowSystem oldWorkflowSystem)
		{
			oldWorkflowDB.removeWorkflowDBChangeListener(dbChangeListerner);
			IWorkflowDB db=activeWorkContext.getWorkflowDB();
			if(db!=null)
			{
				db.addWorkflowDBChangeListener(dbChangeListerner);
			}
			tv.setInput(activeWorkContext);
		}
		
	};
	
	private IWorkflowDBChangeListerner dbChangeListerner= 
		new IWorkflowDBChangeListerner()
	{
		public void workflowDBChnaged()
		{
			tv.setInput(activeWorkContext);
		}
	};
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		tv= new TreeViewer(parent,SWT.FILL);
		simModelBasedCP= new SimModelBasedContentProvider();
		tv.setContentProvider(simModelBasedCP);
		tv.setInput(activeWorkContext);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		
	}
	
}
