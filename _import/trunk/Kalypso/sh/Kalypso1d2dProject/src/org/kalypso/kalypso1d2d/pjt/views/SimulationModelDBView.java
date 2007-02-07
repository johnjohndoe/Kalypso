/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.part.WorkbenchPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.db.IWorkflowDBChangeListerner;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.SimModelBasedContentProvider;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.WorkflowDataLabelProvider;



/**
 * @author Patrice Congo
 *
 */
public class SimulationModelDBView extends ViewPart
{
	final static private Logger logger= 
				Logger.getLogger(SimulationModelDBView.class);
	
	static public final String ID=
				"org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView";
	
	private TreeViewer tv;
	
	private SimModelBasedContentProvider simModelBasedCP;
	
	private ActiveWorkContext activeWorkContext= 
						ActiveWorkContext.getInstance();
	
	private WorkflowDataLabelProvider labelProvider=
							new WorkflowDataLabelProvider();
	
	
	
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
				//top.setVisible(true);
			}
			else
			{
				logger.warn("New Project DB is nul");
				//top.setVisible(false);
			}
			tv.setInput(activeWorkContext);
            // TODO: this is for debugging purposes, remove later?
            setContentDescription( newProject == null ? "" : newProject.getName() );
		}
		
	};
	
	private Composite top;
	
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
		top= new Composite(parent,SWT.FILL);
		top.setLayout(new FillLayout());
		
		tv= new TreeViewer(top,SWT.FILL);
		simModelBasedCP= new SimModelBasedContentProvider();
		tv.setContentProvider(simModelBasedCP);
		tv.setInput(activeWorkContext);
		activeWorkContext.addActiveContextChangeListener(activeProjectChangeListener);
		getSite().setSelectionProvider(tv);
		tv.setLabelProvider(labelProvider);
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		
	}
	
	public void updateTreeView(IWorkflowData selected)
	{
		tv.refresh();
		IStructuredSelection selection= new StructuredSelection(selected);
	}
}
