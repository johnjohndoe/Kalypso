package org.kalypso.afgui;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.kalypso.afgui.model.IWorkflow;

public class ApplicationWorkbenchAdvisor extends WorkbenchAdvisor {

	final static private Logger logger=
				Logger.getLogger(ApplicationWorkbenchAdvisor.class);
	private static final String PERSPECTIVE_ID = "KalypsoAFGUIFramework.perspective";

    public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
        
    	return new ApplicationWorkbenchWindowAdvisor(configurer);
    }

	public String getInitialWindowPerspectiveId() {
		return PERSPECTIVE_ID;
	}
	
	@Override
	public IAdaptable getDefaultPageInput()
	{
		return new IAdaptable()
			{

				public Object getAdapter(Class adapter)
				{
					logger.info("\n=============ADAPTER"+adapter);
					
					if(IWorkflow.class.isAssignableFrom(adapter))
					{
						return KalypsoAFGUIFrameworkPlugin.getDefault().getWorkflow();
					}
					else
					{
						return ApplicationWorkbenchAdvisor.super.getDefaultPageInput();
					}
				}
			
			};
	}
}
