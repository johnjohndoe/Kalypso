package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public class ResultNavigatorContentProvider extends WorkbenchContentProvider implements ITreeContentProvider
{
  private final ResultDB m_db;

  private Viewer m_viewer;

  public ResultNavigatorContentProvider( )
  {
    // TODO: isn't it more eclipse-like to use db as input (instead of non-used file object?)

    // TODO: change to project dependend db
    m_db = KalypsoModel1D2DPlugin.getDefault().getResultDB();
    // maybe listen to changes to the db and update the viewer
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object element )
  {
    // consider setting a specific input, now only static access to db is possible
    final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = m_db.getSimulationDescriptors();
    return simulationDescriptors.toArray();
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = viewer;
    super.inputChanged( viewer, oldInput, newInput );
  }
}
