/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.workflow.Workflow;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowSystem implements IWorkflowSystem
{
  final static private Logger logger = Logger.getLogger( WorkflowSystem.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.workflow.ObjectFactory.class );

  private Workflow m_currentWorkflow;

  public WorkflowSystem( final URL url ) throws JAXBException
  {
    loadModel( url );
  }

  private void loadModel( final URL url ) throws JAXBException
  {
    m_currentWorkflow = (Workflow) JC.createUnmarshaller().unmarshal( url );
  }

  /**
   * @see org.kalypso.afgui.model.IWorkflowSystem#getCurrentWorkFlow()
   */
  public Workflow getCurrentWorkFlow( )
  {
    return m_currentWorkflow;
  }
}
