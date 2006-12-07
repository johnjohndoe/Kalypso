package org.kalypso.kalypso1d2d.pjt;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import org.apache.log4j.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.db.WorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem; 
import org.kalypso.afgui.model.impl.WorkflowSystem;



/**
 * Project Nature for 1d 2d simulation 
 * 
 * @author Patrice Congo
 */
public class Kalypso1D2DProjectNature implements IProjectNature
{
	final static private Logger logger=
				Logger.getLogger(Kalypso1D2DProjectNature.class);
	
	static final String ID=
		"org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature";
	
	public static final String METADATA_FOLDER = ".metadata";
	
	public static final String WORKFLOW_DESC="workflow.xml";
	
	public static final String WORKFLOW_DATA_DESC="worflow_data.xml";
	
	private IWorkflowSystem workflowSystem;
	
	private IWorkflowDB workflowDB;
	
	private IProject project;
	
	private IFolder metaDataFolder;
	
	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	public void configure() throws CoreException
	{
		logger.info("Configuring1:"+project);
		addNature(project);
		try
		{
			createMetaDataFolder(project);
		}
		catch (IOException e)
		{
			logger.error(e);
		}
//		metaDataFolder=project.getFolder(METADATA_FOLDER);
//		
//		try
//		{
//			workflowDB=
//				new WorkflowDB(
//						metaDataFolder.getFile(WORKFLOW_DATA_DESC).getRawLocationURI().toURL());
//		}
//		catch (MalformedURLException e)
//		{
//			logger.error("Bad url to work flow desc data",e);
//		}
//		catch (IOException e)
//		{
//			logger.error("Work flow data could not be found",e);
//		}
//		logger.info("Config End:"+workflowDB);
	}
	
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	public void deconfigure() throws CoreException
	{
//		URL specURL=null;
//		URL statusURL=null;
//		try
//		{
//			workflowSystem= new WorkflowSystem(specURL, statusURL);
//			
//		}
//		catch(IOException th)
//		{
//			final String MSG="Error while creating workflow system";
//			
//			IStatus status=
//				new Status( IStatus.ERROR, 
//							Kalypso1d2dProjectPlugin.PLUGIN_ID, 
//							0, 
//							MSG, 
//							th);
//			throw new CoreException(status);
//		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject()
	{
		return project;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	synchronized public void setProject(IProject project)
	{
		logger.info("Setting project");
		this.project=project;
	}
	
	synchronized public IWorkflowDB getWorkflowDB()
	{
		if(workflowDB==null)
		{
			makeWorkflowDB_Sys();
		}
		
		return workflowDB;
		
	}
	
	
	private void makeWorkflowDB_Sys()
	{
		metaDataFolder=project.getFolder(METADATA_FOLDER);
		
		try
		{
			workflowDB=
				new WorkflowDB(
						metaDataFolder.getFile(WORKFLOW_DATA_DESC));
			workflowSystem=
				new WorkflowSystem(
						metaDataFolder.getFile(WORKFLOW_DESC).getRawLocationURI().toURL());
		}
		catch (MalformedURLException e)
		{
			logger.error("Bad url to work flow desc data",e);
		}
		catch (IOException e)
		{
			logger.error("Work flow data could not be found",e);
		}
		logger.info("Config End:"+workflowDB);
		//return workflowDB;
	}
	
	synchronized public IWorkflowSystem getWorkflowSystem()
	{
		if(workflowSystem==null)
		{
			makeWorkflowDB_Sys();
		}
		return workflowSystem;
	}
	
	public static final boolean isOfThisNature(IProject project) throws CoreException
	{
		return project.hasNature(ID);
	}
	
	public static final Kalypso1D2DProjectNature toThisNature(IProject project) throws CoreException
	{
		//project.hasNature(ID);
		return (Kalypso1D2DProjectNature)project.getNature(ID);
	}
	
	public static final void addNature(IProject project) throws CoreException
	{
		
		if(project.hasNature(ID))
		{
			return;
		}
		else
		{
	      IProjectDescription description = project.getDescription();
	      String[] natures = description.getNatureIds();
	      String[] newNatures = new String[natures.length + 1];
	      System.arraycopy(natures, 0, newNatures, 0, natures.length);
	      newNatures[natures.length] = ID;
	      //Kalypso1d2dProjectPlugin.getDefault().showMessage(Arrays.asList(newNatures).toString());
	      description.setNatureIds(newNatures);
	      project.setDescription(description, null);
		}
		 
	}
	
	final static private IFolder createMetaDataFolder(IProject project) throws CoreException, IOException
	{
		logger.info(project);
		final IFolder metaFolder = project.getFolder(METADATA_FOLDER);
		if(metaFolder.exists())
		{
			return metaFolder;
		}
		else
		{
			
			metaFolder.create( false, true, null );
			
			KalypsoAFGUIFrameworkPlugin plugin=
				KalypsoAFGUIFrameworkPlugin.getDefault();
			Object[][] file_specs=
				{
					{WORKFLOW_DATA_DESC,plugin.getTemplateWorkflowData()},
					{WORKFLOW_DESC, plugin.getWorkflowSpec()}
				};
			for(Object[] file_spec:file_specs)
			{
				IFile file= 
					metaFolder.getFile((String)file_spec[0]);
			    file.create(
			    			((URL)file_spec[1]).openStream(), 
			    			true, 
			    			null);			    
			}
			metaFolder.getParent().refreshLocal(1, null);
			 return metaFolder;
		}
		
	}
	
	
}
