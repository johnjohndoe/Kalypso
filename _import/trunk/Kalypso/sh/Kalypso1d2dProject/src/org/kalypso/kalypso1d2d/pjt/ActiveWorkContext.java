package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.views.navigator.ResourceNavigator;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;

//TODO move to workflow system problem with project??

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
	ISelectionListener resSelListener= new ISelectionListener()
	{
		public void selectionChanged(IWorkbenchPart part, ISelection selection)
		{
//			logger.info("MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM");
			if(part instanceof ResourceNavigator)
			{
				
				if(selection instanceof IStructuredSelection)
				{
					IStructuredSelection isl= (IStructuredSelection)selection;
					int size=isl.size();
					if(size==1)
					{
						Object firstEl=isl.getFirstElement();
						if(firstEl instanceof IProject)
						{
							try
							{
								setActiveProject((IProject)firstEl);
							}
							catch(Throwable th)
							{
								logger.error("Error secting active:"+firstEl,th);
								try{setActiveProject(null);}catch(Throwable th1){}
								
							}
						}
					}
					else
					{
						logger.warn("Can only cope with single selection: "+isl);
					}
				}
			}
		}		
	};
	
	final static Logger logger=
				Logger.getLogger(ActiveWorkContext.class);
	
	private final static ActiveWorkContext activeWorkContext= new ActiveWorkContext();
	
	private IWorkflowDB workflowDB;
	
	private IWorkflowSystem workflowSystem;
	
    private final SzenarioDataProvider m_dataProvider = new SzenarioDataProvider();

	private IProject activeProject;
	
	private List< IActiveContextChangeListener> activeProjectChangeListener=
		new ArrayList<IActiveContextChangeListener>();
	
	private ActiveWorkContext()
	{
		IWorkbenchWindow window=
			PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		window.getSelectionService().addPostSelectionListener(resSelListener);
        final SzenarioSourceProvider simModelProvider = new SzenarioSourceProvider( this );
        final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService(IHandlerService.class);
        service.addSourceProvider(simModelProvider);
        //TODO remove source provider somewhere
	}
	
	synchronized public void setActiveProject(IProject activeProject) throws CoreException
	{
		if(this.activeProject==activeProject)
		{
			return;
		}
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
				Kalypso1D2DProjectNature nature=
					Kalypso1D2DProjectNature.toThisNature(activeProject);
				this.activeProject = activeProject;
				this.workflowDB=nature.getWorkflowDB();
				this.workflowSystem=nature.getWorkflowSystem();
				logger.info("WorkflowDB="+workflowDB);
				logger.info("WorkflowSystem:"+workflowSystem);
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
		
	public IWorkflow getCurrentWorkflow()
	{
		if(activeProject==null)
		{
			return null;
		}
		if(workflowSystem==null)
		{
			return null;
		}
		else
		{
			return workflowSystem.getCurrentWorkFlow();
		}
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
    
    public ISzenarioDataProvider getSzenarioDataProvider( )
    {
      return m_dataProvider;
    }

    public void setCurrentSzenario( final IProject project, final IWorkflowData data )
    {
      m_dataProvider.setCurrent( project, data );
    }
}
