package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.IWorkbenchAdapter2;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;

/**
 * @author Stefan Kurzbach
 * TODO: rename to ResultDBWorkbenchAdapterFactory
 */
public class SimulationDescriptorAdapterFactory implements IAdapterFactory
{

  private SimulationDescriptorTreeContentAdapter m_simulationDescriptorTreeContentAdapter;

  private ResultModelDescriptorWorkbenchAdapter m_resultModelDescriptorTreeContentAdapter;

  public SimulationDescriptorAdapterFactory( )
  {
    m_simulationDescriptorTreeContentAdapter = new SimulationDescriptorTreeContentAdapter();
    m_resultModelDescriptorTreeContentAdapter = new ResultModelDescriptorWorkbenchAdapter();
  }

  @SuppressWarnings("unchecked")
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adaptableObject instanceof ISimulationDescriptor )
    {
      return m_simulationDescriptorTreeContentAdapter;
    }
    else if( adaptableObject instanceof IResultModelDescriptor )
    {
      return m_resultModelDescriptorTreeContentAdapter;
    }
    return null;
  }

  @SuppressWarnings("unchecked")
  public Class[] getAdapterList( )
  {
    return new Class[] { IWorkbenchAdapter.class, IWorkbenchAdapter2.class };
  }

}
