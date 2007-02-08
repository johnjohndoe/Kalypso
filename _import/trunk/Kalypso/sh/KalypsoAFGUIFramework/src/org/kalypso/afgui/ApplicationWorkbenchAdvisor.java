package org.kalypso.afgui;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.kalypso.afgui.model.IWorkflow;

public class ApplicationWorkbenchAdvisor extends WorkbenchAdvisor {

	final static private Logger logger=
				Logger.getLogger(ApplicationWorkbenchAdvisor.class.getName());
     private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

        static
        {
          if( !log )
            logger.setUseParentHandlers( false );
        }
        
    
    //TODO change perspective	
	private static final String PERSPECTIVE_ID = 
		//"KalypsoAFGUIFramework.perspective";
		"org.kalypso.kalypso1d2d.pjt.perspective.Perspective";
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
						return KalypsoAFGUIFrameworkPlugin.getDefault().getWorkflowSystem();
					}
					else
					{
						return ApplicationWorkbenchAdvisor.super.getDefaultPageInput();
					}
				}
			
			};
	}
}
