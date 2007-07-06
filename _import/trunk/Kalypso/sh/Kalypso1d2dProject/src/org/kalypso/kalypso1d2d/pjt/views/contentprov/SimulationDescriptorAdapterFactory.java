package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.IWorkbenchAdapter2;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;

/**
 * @author Stefan Kurzbach
 * 
 */
public class SimulationDescriptorAdapterFactory implements IAdapterFactory
{

  private SimulationDescriptorTreeContentAdapter m_simulationDescriptorTreeContentAdapter;

  public SimulationDescriptorAdapterFactory( )
  {
    m_simulationDescriptorTreeContentAdapter = new SimulationDescriptorTreeContentAdapter();
  }

  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adaptableObject instanceof ISimulationDescriptor )
    {
      return m_simulationDescriptorTreeContentAdapter;
    }
    return null;
  }

  public Class[] getAdapterList( )
  {
    return new Class[] { IWorkbenchAdapter.class, IWorkbenchAdapter2.class };
  }

}
