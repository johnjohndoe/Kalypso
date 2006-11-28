package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.model.impl.WorkflowSystem;

import sun.security.krb5.internal.ac;

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
	
	private List< IActiveProjectChangeListener> activeProjectChangeListener=
		new ArrayList<IActiveProjectChangeListener>();
	
	private ActiveWorkContext()
	{
		//empty
	}
	
	synchronized public void setActiveProject(IProject activeProject) throws CoreException
	{
		logger.info("New Project to Set:"+activeProject);
		
		try
		{
			if(Kalypso1D2DProjectNature.isOfThisNature(activeProject))
			{
				this.activeProject = activeProject;
			}
			else
			{
				//IProject oldProject=this.activeProject;
				this.activeProject=null;
				fireActiveProjectChanged(activeProject);
			}
		}
		catch (CoreException e)
		{
			logger.error("Error setting current project", e);
			throw e;
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
		
	
	synchronized public void addActiveProjectChangedListener(IActiveProjectChangeListener l)
	{
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
	
	synchronized public void removeActiveProjectChangedListener(IActiveProjectChangeListener l)
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
	
	synchronized public void removeAllActiveProjectChangedListener()
	{
		activeProjectChangeListener.clear();
	}
	
	final private void fireActiveProjectChanged(IProject newProject)
	{
		for(IActiveProjectChangeListener l:activeProjectChangeListener)
		{
			l.activeProjectChanged(newProject);
		}
	}
}
