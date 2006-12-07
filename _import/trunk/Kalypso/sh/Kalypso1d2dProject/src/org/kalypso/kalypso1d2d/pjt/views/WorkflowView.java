package org.kalypso.kalypso1d2d.pjt.views;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.viz.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetSelectedAction;

import test.org.kalypso.afgui.TestRDFModel;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowView extends ViewPart
{
	private static final  Logger logger=Logger.getLogger(WorkflowView.class);
	
	final static public String ID="org.kalypso.kalypso1d2d.pjt.views.WorkflowView";
	private WorkflowControl workflowControl;
	private ActiveWorkContext activeWorkContext=
								ActiveWorkContext.getInstance();
	
	private IActiveContextChangeListener workContextChangeListener=
		new IActiveContextChangeListener()
	{

		public void activeProjectChanged(
							IProject newProject, 
							IProject oldProject, 
							IWorkflowDB oldDB, 
							IWorkflowSystem oldWorkflowSystem)
		{
			IWorkflow workflow=activeWorkContext.getCurrentWorkflow();
			logger.info("New Workflow:"+workflow);
			workflowControl.setWorkflow(workflow);
			
		}
		
	};
	
	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
//		TODO change hard coding to test workflow
//		logger.warn("Using SH Test workflow");
//		
//		workflowControl= 
//			new WorkflowControl(TestRDFModel.getTesWorkflow());
		activeWorkContext.addActiveContextChangeListener(
									workContextChangeListener);
		workflowControl= 
			new WorkflowControl(
					activeWorkContext.getCurrentWorkflow());
		workflowControl.createControl(parent);
	}
	
	
	
	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{
		
	}

}
