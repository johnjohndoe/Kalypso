/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.apache.log4j.Logger;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView;

/**
 * @author pat_dev
 *
 */
public class RemoveWorkflowData implements IViewActionDelegate
{
	private static final Logger logger= Logger.getLogger(RemoveWorkflowData.class);
	
	private IWorkflowData selected;
	
	private IViewPart viewPart;
	
	/**
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view)
	{
		this.viewPart=view;
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action)
	{
		logger.info("WorkflowData.toRemove="+selected);
		if(selected!=null)
		{
			selected.remove();
			action.setEnabled(false);
			selected=null;
		}
		if(viewPart instanceof SimulationModelDBView)
		{
			((SimulationModelDBView)viewPart).updateTreeView(selected);
		}

	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection)
	{
		logger.info("WorkflodataRemSel="+selection);
		selected=null;
		if(selection instanceof IStructuredSelection)
		{
			IStructuredSelection structSel=
						((IStructuredSelection)selection);
			
			if(structSel.size()>1)
			{
				action.setEnabled(false);
			}
			
			Object object=
				structSel.getFirstElement();
			//IWorkflowData parent=null;
			if(object instanceof IWorkflowData)
			{
				action.setEnabled(true);
				selected=(IWorkflowData)object;
			}
			else
			{
				action.setEnabled(false);
			}
			
		}
		else
		{
			action.setEnabled(false);
		}

	}

}
