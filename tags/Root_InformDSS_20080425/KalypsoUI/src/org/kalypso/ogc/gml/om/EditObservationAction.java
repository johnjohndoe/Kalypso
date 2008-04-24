package org.kalypso.ogc.gml.om;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.observation.IObservation;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

public class EditObservationAction implements IActionDelegate
{
  private IFeatureSelection m_selection = null;

  @SuppressWarnings("unchecked")
  public void run( final IAction action )
  {
    if( m_selection != null )
    {
      final Feature feature = FeatureSelectionHelper.getSelectedFeature( m_selection );
      if( feature == null )
        return;

      final IObservation obs = (IObservation) feature.getAdapter( IObservation.class );

      final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
      
      final ObservationDialog dlg = new ObservationDialog( window.getShell(), obs );
      dlg.open();
    }
  }

  public void selectionChanged( final IAction action, final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
      m_selection = (IFeatureSelection) selection;
  }
}
