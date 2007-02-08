/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;

/**
 * 
 * TODO: obsolete, remove?
 * 
 * @author Patrice Congo
 */
public class SetActiveProjectActionDelegate implements IViewActionDelegate
{
	final static private Logger logger=
			Logger.getLogger(SetActiveProjectActionDelegate.class.getName());
    private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }
	//private IViewPart viewPart;
	
	private IProject selectedProject;
	
	private ActiveWorkContext activeWorkContext=
						ActiveWorkContext.getInstance();
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view)
	{
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action)
	{
		try
		{
			activeWorkContext.setActiveProject(selectedProject);
		}
		catch (CoreException e)
		{
			logger.log(Level.SEVERE,"error while selting new project:"+selectedProject,e);
		}
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection)
	{
		logger.info(action.toString());
		logger.info(selection.toString());
		
		if(selection instanceof IStructuredSelection)
		{
			if(((IStructuredSelection)selection).size()>1)
			{
				action.setEnabled(false);
				return;
			}
			
			Object object=((IStructuredSelection)selection).getFirstElement();
			
			selectedProject=null;
			if(object instanceof IProject)
			{
				IProject pjt=(IProject)object;
			
				try
				{
					if(pjt.isOpen() && Kalypso1D2DProjectNature.isOfThisNature(pjt))
					{
						selectedProject=pjt;
						action.setEnabled(true);
					}
					else
					{
						action.setEnabled(false);
						logger.info("Project is not open");
					}
				}
				catch (CoreException e)
				{
					logger.log(Level.WARNING, "", e);
					action.setEnabled(false);
				}
			}
			else
			{
				action.setEnabled(false);
			}
		}
		else
		{
			return;
		}
	}

}
