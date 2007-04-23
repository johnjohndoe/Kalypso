/**
 * 
 */
package de.renew.workflow.base;

import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jwsdp.JaxbUtilities;

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

  private static final JAXBContext JC = JaxbUtilities.createQuiet( de.renew.workflow.base.ObjectFactory.class, de.renew.workflow.contexts.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class );

  private Workflow m_currentWorkflow;

  /**
   * Loads a workflow instance for the project
   * 
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li> The metadata folder is not accessible.</li>
   *              <li> There is a problem loading the workflow.</li>
   */
  public WorkflowSystem( final IProject project ) throws CoreException
  {
    final IFolder metadataFolder = project.getFolder( METADATA_FOLDER );
    final IFile workflowFile = metadataFolder.getFile( WORKFLOW_FILENAME );
    m_currentWorkflow = loadModel( workflowFile );
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
      final IStatus status = StatusUtilities.statusFromThrowable( e );
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
