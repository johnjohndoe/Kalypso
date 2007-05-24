/**
 * 
 */
package de.renew.workflow.base;

import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

/**
 * This workflow system manages the workflow instance in a description file in the project .metadata folder
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowSystem implements IWorkflowSystem
{
  public static final String METADATA_FOLDER = ".metadata";

  public static final String WORKFLOW_FILENAME = "workflow.xml";

  private static final Logger logger = Logger.getLogger( WorkflowSystem.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final JAXBContext JC = createJAXBContext();

  private Workflow m_currentWorkflow;

  /**
   * Loads a workflow instance for the project
   * 
   * @exception CoreException
   *                if this method fails. Reasons include:
   *                <ul>
   *                <li> The metadata folder is not accessible.</li>
   *                <li> There is a problem loading the workflow.</li>
   */
  public WorkflowSystem( final IProject project ) throws CoreException
  {
    final IFolder metadataFolder = project.getFolder( METADATA_FOLDER );
    final IFile workflowFile = metadataFolder.getFile( WORKFLOW_FILENAME );
    if( workflowFile.exists() )
    {
      m_currentWorkflow = loadModel( workflowFile );
    }
    else
    {
      final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "Workflow definition file could not be found.");
      throw new CoreException( status );
    }
  }

  private static JAXBContext createJAXBContext( )
  {
    try
    {
      return JAXBContext.newInstance( de.renew.workflow.base.ObjectFactory.class, de.renew.workflow.contexts.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private Workflow loadModel( final IFile file ) throws CoreException
  {
    try
    {
      final URL url = file.getRawLocationURI().toURL();
      return (Workflow) JC.createUnmarshaller().unmarshal( url );
    }
    catch( final Throwable e )
    {
      final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "", e );
      throw new CoreException( status );
    }
  }

  /**
   * @see org.kalypso.afgui.model.IWorkflowSystem#getCurrentWorkFlow()
   */
  public Workflow getCurrentWorkflow( )
  {
    return m_currentWorkflow;
  }
}
