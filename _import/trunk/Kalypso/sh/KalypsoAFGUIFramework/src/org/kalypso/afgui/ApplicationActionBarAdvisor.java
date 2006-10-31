package org.kalypso.afgui;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.ToolBarContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.kalypso.afgui.model.EActivityAction;

public class ApplicationActionBarAdvisor extends ActionBarAdvisor {

	/**
	 * Action that save the current project
	 */
	private Action reloadAction;
	
    public ApplicationActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);
        
        reloadAction= new Action()
		{
			public void run() {
				KalypsoAFGUIFrameworkPlugin.getDefault().reloadWorkflow();
			}
			
		}; 
		reloadAction.setText("Reload workflow");
		
		ImageRegistry reg=KalypsoAFGUIFrameworkPlugin.getDefault().getImageRegistry();
		reloadAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.RELOAD.toString()));
    }
    

    protected void makeActions(IWorkbenchWindow window) {
    }

    protected void fillMenuBar(IMenuManager menuBar) {
    }
    
    /**
     * @see org.eclipse.ui.application.ActionBarAdvisor#fillCoolBar(org.eclipse.jface.action.ICoolBarManager)
     */
    protected void fillCoolBar(ICoolBarManager coolBar) 
    {
    	coolBar.removeAll();
    	
		IToolBarManager toolbar = 
    		new ToolBarManager(SWT.FLAT | SWT.RIGHT);
        coolBar.add(
        	new ToolBarContributionItem(toolbar, "WorkflowToolbar"));        
		toolbar.add(reloadAction);		
	}
}
