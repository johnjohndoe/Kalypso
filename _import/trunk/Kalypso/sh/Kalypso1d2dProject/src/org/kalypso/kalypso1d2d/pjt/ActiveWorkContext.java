package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem;


/**
 * Represents the work context for a user.
 * A workkontext is made of:
 * <ul>
 * 	<li/>The actuel project the user is working on
 * 	<li/>The Workflow system
 *  <li/>The data basis system
 *  
 * </ul>
 * @author Patrice Congo
 */
public class ActiveWorkContext
{
	final static Logger logger=
				Logger.getLogger(ActiveWorkContext.class);
	
	private final static ActiveWorkContext activeWorkContext= new ActiveWorkContext();;
	
	private IWorkflowDB workflowDB;
	
	private IWorkflowSystem workflowSystem;
	
	private IProject activeProject;
	
	private List< IActiveContextChangeListener> activeProjectChangeListener=
		new ArrayList<IActiveContextChangeListener>();
	
	private ActiveWorkContext()
	{
		//empty
	}
	
	synchronized public void setActiveProject(IProject activeProject) throws CoreException
	{
		logger.info("New Project to Set:"+activeProject);
		IProject oldProject=this.activeProject;
		IWorkflowDB oldWorkflowDB=getWorkflowDB();
		IWorkflowSystem oldWorkflowSystem=getWorkflowSystem();
		if(oldWorkflowDB!=null)
		{
			oldWorkflowDB.persist();
		}
		
		try
		{			
			if(Kalypso1D2DProjectNature.isOfThisNature(activeProject))
			{
				this.activeProject = activeProject;
				this.workflowDB=
					Kalypso1D2DProjectNature.toThisNature(activeProject).getWorkflowDB();
				logger.info("WorkflowDB="+workflowDB);
			}
			else
			{
				
				this.activeProject=null;
				this.workflowDB=null;
				logger.warn("Project to set is not of 1d2d nature");
			}
		}
		catch (CoreException e)
		{
			logger.error("Error setting current project", e);
			throw e;
		}
		finally
		{
			fireActiveProjectChanged(
						activeProject,
						oldProject,
						oldWorkflowDB,
						oldWorkflowSystem);
		}
	}
	
	final static public ActiveWorkContext getInstance()
	{
		return activeWorkContext;
	}
	
	synchronized public IProject getActiveProject()
	{
		return activeProject;
	}
	
	synchronized public IWorkflowDB getWorkflowDB()
	{
		return workflowDB;
	}
	
	synchronized public IWorkflowSystem getWorkflowSystem()
	{
		return workflowSystem;
	}
		
	
	synchronized public void addActiveContextChangeListener(IActiveContextChangeListener l)
	{
		logger.info("Registering Active context change listener:"+l);
		if(l==null)
		{
			return;
		}
		else
		{
			if(activeProjectChangeListener.contains(l))
			{
				return;
			}
			else
			{
				activeProjectChangeListener.add(l);
			}
		}
	}
	
	synchronized public void removeActiveContextChangeListener(
										IActiveContextChangeListener l)
	{
		if(l==null)
		{
			return;
		}
		else
		{
			if(activeProjectChangeListener.contains(l))
			{
				activeProjectChangeListener.add(l);
			}
			else
			{
				//empty
			}
		}
	}
	
	synchronized public void removeAllActiveContextChangeListener()
	{
		activeProjectChangeListener.clear();
	}
	
	final private void fireActiveProjectChanged(
							IProject newProject, 
							IProject oldProject,
							IWorkflowDB oldWorkflowDB,
							IWorkflowSystem oldWorkflowSystem)
	{
		for(IActiveContextChangeListener l:activeProjectChangeListener)
		{
			l.activeProjectChanged(
						newProject, 
						oldProject,
						oldWorkflowDB,
						oldWorkflowSystem);
		}
	}
}
