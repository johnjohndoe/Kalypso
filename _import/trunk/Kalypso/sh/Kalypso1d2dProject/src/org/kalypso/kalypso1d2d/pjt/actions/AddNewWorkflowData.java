/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.apache.log4j.Logger;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;

/**
 * 
 * @author Patrice Congo
 */
abstract public class AddNewWorkflowData implements IViewActionDelegate
{
	final static public Logger logger=
				Logger.getLogger(AddNewWorkflowData.class);
	
	protected IViewPart viewPart;
	
	protected ActiveWorkContext activeWorkContext=
								ActiveWorkContext.getInstance();
	protected Object selected;
	/**
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view)
	{
		viewPart=view;
		try
		{
			IContributionItem cItem=
				view.getViewSite().getActionBars().getToolBarManager().
					find("org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBViewActionsContrib");
			cItem.update();
			logger.info(cItem);
		}
		catch(Throwable th)
		{
			logger.error("Erro while updating contribution items",th);
		}
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action)
	{
		createWorkflowData(viewPart, selected);
		selected=null;
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection)
	{
		this.selected=canHandle(selection);
//		IWorkflowDB workflowDB=activeWorkContext.getWorkflowDB();
//		
//		if(workflowDB==null)
//		{
//			logger.warn(
//					"Now workflow db; cannot derived new workflow data from :"+selection);
//			//action.setEnabled(true);
//			return;
//		}
//		else
//		{
//			//this.selection=canHandle(selection);
//			if(this.selection==null)
//			{
//				//action.setEnabled(true);
//			}
//			else
//			{
//				//action.setEnabled(true);
//			}
//			
//		}

	}
	
	/**
	 * Template method to be implemented by child classes to
	 * provide mechnism to create new workflow data
	 * @param viewPart
	 * @param dataContext
	 */
	abstract protected void createWorkflowData(
									IViewPart viewPart,
									Object dataContext);
	
	/**
	 * Template method to be implemented by child classes 
	 * to answer whether is can handle the selelction. If the
	 * delegate can handle the selection it return a non null
	 * object otherwise null
	 * 
	 * @param sel -- the actual selection to check for handle 
	 * 			compatibility
	 * 
	 * @return an object representing the selection to handle
	 */
	abstract protected Object canHandle(ISelection sel);

}
