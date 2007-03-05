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
 * @author Patrice Congo
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

  // final private Model specModel;
  //
  // final private Model mergedModel;

  // private IWorkflowPartRTContext currentWorkflowRTContext;
  // private IWorkflowPartRTContext currentTaskGroupRTContext;
  // private IWorkflowPartRTContext currentSubTaskGroupRTContext;
  // private IWorkflowPartRTContext currentTaskRTContext;
  // private IWorkflowPartRTContext currentActivityRTContext;

  // private IWorkflowData currentDataModel;

  private Workflow m_currentWorkflow;

  // public Map<String, IActivity> activities = new Hashtable<String, IActivity>();

  // public WorkflowSystem( URL specURL, URL statusURL ) throws IOException
  // {
  // specModel = loadModel( specURL );
  // mergedModel = loadModel( statusURL );
  // mergedModel.add( specModel );
  // // mergedModel.difference(arg0)
  // }

  // public WorkflowSystem( URL mergedSpecURL ) throws IOException
  // {
  // specModel = null;// ModelFactory.createDefaultModel();//loadModel(specURL);
  // mergedModel = loadModel( mergedSpecURL );
  // // mergedModel.add(specModel);
  // // mergedModel.difference(arg0)
  // }

  // private final Model loadModel(final URL url) throws IOException
  // {
  // // TODO: close input streams (finally!)
  // logger.info("specURL="+url);
  // InputStream iStream=url.openStream();
  // Model rdfModel= ModelFactory.createDefaultModel();
  // rdfModel.read(iStream,"");
  // return rdfModel;
  // }

  public WorkflowSystem( final URL url ) throws JAXBException
  {
    loadModel( url );
  }

  private void loadModel( final URL url ) throws JAXBException
  {
    m_currentWorkflow = (Workflow) JC.createUnmarshaller().unmarshal( url );
  }

  // public IWorkflow createWorkflow(URL specURL, URL statusURL) throws IOException
  // {
  // if(specURL==null)
  // {
  // throw new IllegalArgumentException(
  // "A spec url is needed to create a workspace");
  // }
  //		
  // return new WorkflowImpl(
  // loadSpecifications(specURL),
  // loadRuntimeStatus(statusURL));
  // }

  // public IWorkflow getCurrentWorkFlow( )
  // {
  // if( m_currentWorkflow == null )
  // {
  // m_currentWorkflow = new WorkflowImpl( mergedModel.getResource( TestRDFModel.WORKFLOW_SH ) );
  // }
  // return m_currentWorkflow;
  // }

  /**
   * @see org.kalypso.afgui.model.IWorkflowSystem#getCurrentWorkFlow()
   */
  public Workflow getCurrentWorkFlow( )
  {
    return m_currentWorkflow;
  }

  // public IActivity getActivity( String uri )
  // {
  // if( uri == null )
  // {
  // return null;
  // }
  // else
  // {
  // return activities.get( uri );
  // }
  // }
}
