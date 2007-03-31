package de.renew.workflow;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import de.renew.formalism.workflow.TaskOccurrence;
import de.renew.workflow.cases.CaseData;
import de.renew.workflow.cases.CaseDataMapWrapper;
import de.renew.workflow.cases.ECaseDataState;

/**
 * @author Stefan Kurzbach
 */
public class CaseTaskImpl extends TaskImpl
{
  private static final long serialVersionUID = -309633924634403286L;

  private static final Object PARAM_MANDATORY_DATAKEYS = "mandatory";

  private List<String> m_mandatoryDataKeys;

  /**
   * @see TaskImpl
   */
  public CaseTaskImpl( final WorkflowAdaptor adaptor, final String name1, final String description1, final int priority1, final Map<String, Object> settings1 ) throws RemoteException
  {
    super( adaptor, name1, description1, priority1, settings1 );
  }

  /**
   * @see de.renew.workflow.TaskImpl#createWorkItem(de.renew.formalism.workflow.TaskOccurrence, java.lang.Object)
   */
  @Override
  protected WorkItemImpl createWorkItem( TaskOccurrence occurrence, Object parameter ) throws RemoteException
  {
    return new WorkItemImpl( occurrence, parameter );
  }

  /**
   * @author Stefan Kurzbach
   */
  public class WorkItemImpl extends de.renew.workflow.TaskImpl.WorkItemImpl implements CaseTask.WorkItem
  {

    private static final long serialVersionUID = -6770835318635105656L;

    /**
     * @see de.renew.workflow.TaskImpl.WorkItemImpl
     */
    protected WorkItemImpl( final TaskOccurrence occurrence, final Object parameter ) throws RemoteException
    {
      super( occurrence, parameter );
    }

    // /**
    // * @see de.renew.workflow.TaskImpl$WorkItemImpl#notifyRequested(de.renew.workflow.ActivityImpl,
    // * de.renew.workflow.Client)
    // */
    // @Override
    // protected void notifyRequested( final ActivityImpl activity, final Client client )
    // {
    // try
    // {
    // final Map<String, Object> settingsMap = activity.getWorkItem().getTask().getSettings();
    // final String runtimeParameter = activity.getWorkItem().getParameter().toString();
    // settingsMap.put( CaseTask.RUNTIME_PARAMETER, runtimeParameter );
    // final Proxy proxy = new ProxyImpl( activity, client );
    // client.execute( new EclipseOperationDelegate( settingsMap, proxy ) );
    // }
    // catch( final RemoteException e )
    // {
    // e.printStackTrace();
    // }
    // }
    //
    // /**
    // * Notifies an implementation of the work item that is has been restored from the database after e.g. a server
    // * crash. This implementation cancels the activity then, because the task is no longer active and the client may
    // be
    // * no longer valid. This method is automatically called by the workflow package.
    // *
    // * @param activity
    // * The corresponding activity.
    // */
    // @Override
    // protected void notifyRestored( ActivityImpl activity )
    // {
    // activity.cancelChecked( null );
    // }

  }

  /**
   * @see de.renew.workflow.WorkItem#isConfirmable()
   */
  @Override
  public boolean isConfirmable( )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.WorkItem#isRequestable()
   */
  @Override
  public boolean isRequestable( )
  {
    // TODO
    // try
    // {
    // final CaseData data = (CaseData) getParameter();
    // return hasAllMandatoryData( data );
    // }
    // catch( RemoteException e )
    // {
    // e.printStackTrace();
    // }
    return false;
  }

  public boolean hasAllMandatoryData( final CaseData data )
  {
    cacheDataKeys();
    final CaseDataMapWrapper dataMap = new CaseDataMapWrapper( data );
    for( final String dataKey : m_mandatoryDataKeys )
    {
      if( dataMap.get( dataKey ).getState() != ECaseDataState.AVAILABLE )
      {
        return false;
      }
    }
    return true;
  }

  private void cacheDataKeys( )
  {
    if( m_mandatoryDataKeys == null )
    {
      m_mandatoryDataKeys = new ArrayList<String>();
      final String mandatoryDataKeys = (String) settings.get( PARAM_MANDATORY_DATAKEYS );
      final StringTokenizer tokenizer = new StringTokenizer( mandatoryDataKeys, ";" );
      while( tokenizer.hasMoreTokens() )
      {
        final String dataKey = tokenizer.nextToken();
        m_mandatoryDataKeys.add( dataKey );
      }
    }
  }
}
