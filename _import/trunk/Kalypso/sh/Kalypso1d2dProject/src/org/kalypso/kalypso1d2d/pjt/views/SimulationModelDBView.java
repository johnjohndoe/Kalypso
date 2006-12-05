/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import org.apache.log4j.Logger;
import org.eclipse.core.internal.events.ResourceDeltaFactory;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.db.IWorkflowDBChangeListerner;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.viz.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.SimModelBasedContentProvider;

import test.org.kalypso.afgui.TestRDFModel;


/**
 * @author Patrice Congo
 *
 */
public class SimulationModelDBView extends ViewPart
{
	final static private Logger logger= 
				Logger.getLogger(SimulationModelDBView.class);
	
	static public final String ID=
				"org.kalypso.afgui.views.SimulationModelBasedView";
	
	private TreeViewer tv;
	
	private SimModelBasedContentProvider simModelBasedCP;
	
	private ActiveWorkContext activeWorkContext= 
						ActiveWorkContext.getInstance();
	
	private IActiveContextChangeListener activeProjectChangeListener=
		new IActiveContextChangeListener()
	{

		public void activeProjectChanged(
						IProject newProject, 
						IProject olProject,
						IWorkflowDB oldWorkflowDB,
						IWorkflowSystem oldWorkflowSystem)
		{
			//oldWorkflowDB.removeWorkflowDBChangeListener(dbChangeListerner);
			if(oldWorkflowDB!=null)
			{
				oldWorkflowDB.removeWorkflowDBChangeListener(
													dbChangeListerner);
			}
			IWorkflowDB newDB=activeWorkContext.getWorkflowDB();
			if(newDB!=null)
			{
				newDB.addWorkflowDBChangeListener(dbChangeListerner);
			}
			else
			{
				logger.warn("New Project DB is nul");
			}
			tv.setInput(activeWorkContext);
		}
		
	};
	
	private IWorkflowDBChangeListerner dbChangeListerner= 
		new IWorkflowDBChangeListerner()
	{
		public void workflowDBChanged()
		{
			tv.setInput(activeWorkContext);
			logger.info("DB changed ");
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
		activeWorkContext.addActiveContextChangeListener(activeProjectChangeListener);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		
	}
	
}
