package org.kalypso.kalypso1d2d.pjt.views;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.viz.WorkflowControl;

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
	
	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
//		TODO change hard coding to test workflow
		logger.warn("Using SH Test workflow");
		workflowControl= 
			new WorkflowControl(TestRDFModel.getTesWorkflow());
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
