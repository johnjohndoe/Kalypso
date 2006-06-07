package org.kalypso.model.wspm.ui.view.legend;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer;


public class RemoveLayerActionDelegate extends AbstractLegendViewActionDelegate
{
  public void run( final IAction action )
  {
    final IStructuredSelection selection = getSelection();
    if( selection == null || selection.isEmpty() )
    {
      handleError( "Es muss mindestens ein Datensatz selektiert sein." );
      return;
    }

    final IProfilChartLayer layer = (IProfilChartLayer)selection.getFirstElement();

    try
    {
      layer.removeYourself();
    }
    catch( final UnsupportedOperationException e )
    {
      handleError( "Dieser Datensatz kann nicht gelöscht werden." );
      return ;
    }
    catch( ProfilDataException e )
    {
      handleError("Feler beim löschen der Datensätze.");
      return;
    }
  }
}
