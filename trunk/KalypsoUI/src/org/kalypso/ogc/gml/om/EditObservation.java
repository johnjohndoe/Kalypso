package org.kalypso.ogc.gml.om;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.observation.IObservation;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

public class EditObservation implements IActionDelegate
{
  private IFeatureSelection m_selection = null;

  public void run( final IAction action )
  {
    if( m_selection != null )
    {
      final Feature feature = FeatureSelectionHelper.getSelectedFeature( m_selection );
      if( feature == null )
        return;

      final IObservation obs = (IObservation) feature.getAdapter( IObservation.class );

      System.out.println( obs.getName() );
    }
  }

  public void selectionChanged( final IAction action, final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;
    }
  }
}
