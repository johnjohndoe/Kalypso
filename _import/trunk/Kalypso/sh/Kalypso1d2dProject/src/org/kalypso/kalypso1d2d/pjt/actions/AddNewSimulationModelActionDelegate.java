/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewPart;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.wizards.NewSimulationModelControlBuilder;

/**
 * @author Patrice Congo
 *
 */
public class AddNewSimulationModelActionDelegate extends AddNewWorkflowData
{

	/**
	 * @see org.kalypso.kalypso1d2d.pjt.actions.AddNewWorkflowData#canHandle(org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	protected Object canHandle(ISelection selection)
	{
		logger.info("canHandleSlection:"+selection);
		if(selection instanceof IStructuredSelection)
		{
			IStructuredSelection structSel=
						((IStructuredSelection)selection);
			
			if(structSel.size()>1)
			{
				return super.activeWorkContext;
			}
			
			Object object=
				structSel.getFirstElement();
			//IWorkflowData parent=null;
			if(object instanceof IWorkflowData)
			{
				//parent=(IWorkflowData)object;
				
			}
			else
			{
				logger.warn("No Workflow data selcted:"+object);
				object=super.activeWorkContext;
			}
			return object;
			
		}
		else
		{
			logger.warn("Cannot handle selection:"+selection);
			return super.activeWorkContext;
		}
		
	}

	/**
	 * @see org.kalypso.kalypso1d2d.pjt.actions.AddNewWorkflowData#createWorkflowData(org.eclipse.ui.IViewPart, java.lang.Object)
	 */
	@Override
	protected void createWorkflowData(IViewPart viewPart, Object dataContext)
	{
		logger.info("Creating Data:"+dataContext);
		IWorkflowData workflowData=
			(dataContext instanceof IWorkflowData)?(IWorkflowData)dataContext:null;
		try
		{
			NewSimulationModelControlBuilder.startWizard(
							viewPart.getSite().getShell(), workflowData);
		}
		catch(Throwable th)
		{
			logger.error("Error creating data", th);
		}
	}

}
