package de.renew.workflow.connector.cases;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.IWorkbenchAdapter2;

public class CaseAdapterFactory implements IAdapterFactory
{
  private final CaseTreeContentAdapter m_caseTreeContentAdapter = new CaseTreeContentAdapter();

  @SuppressWarnings("unchecked")
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adapterType == IWorkbenchAdapter.class || adapterType == IWorkbenchAdapter2.class )
    {
      return m_caseTreeContentAdapter;
    }
    return null;
  }

  public Class< ? >[] getAdapterList( )
  {
    return new Class[] { IWorkbenchAdapter.class, IWorkbenchAdapter2.class };
  }

}
