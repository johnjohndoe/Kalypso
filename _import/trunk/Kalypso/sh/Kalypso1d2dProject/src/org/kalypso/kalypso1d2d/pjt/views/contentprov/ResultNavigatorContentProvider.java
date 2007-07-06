package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

public class ResultNavigatorContentProvider extends WorkbenchContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object element )
  {
    //TODO: maybe have results for each project??
    if( element instanceof IWorkspaceRoot )
    {
      // TODO: get ResultDB from somewhere
      ResultDB db = null;
      final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = db.getSimulationDescriptors();
      return simulationDescriptors.toArray();
    }
    else if( element instanceof ISimulationDescriptor )
    {
      final ISimulationDescriptor descriptor = (ISimulationDescriptor) element;
      final IFeatureWrapperCollection<IResultModelDescriptor> resultModel = descriptor.getResultModel();
      return resultModel.toArray();
    }
    return new Object[0];
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    if( element instanceof IWorkspaceRoot )
    {
      // TODO: get ResultDB from somewhere
      ResultDB db = null;
      final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = db.getSimulationDescriptors();
      return !simulationDescriptors.isEmpty();
    }
    else if( element instanceof ISimulationDescriptor )
    {
      final ISimulationDescriptor descriptor = (ISimulationDescriptor) element;
      final IFeatureWrapperCollection<IResultModelDescriptor> resultModel = descriptor.getResultModel();
      return !resultModel.isEmpty();
    }
    return false;
  }
}
