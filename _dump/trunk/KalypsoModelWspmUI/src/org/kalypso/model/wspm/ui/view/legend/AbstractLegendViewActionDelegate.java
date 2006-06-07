package org.kalypso.model.wspm.ui.view.legend;

import java.util.logging.Logger;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

public abstract class AbstractLegendViewActionDelegate implements IViewActionDelegate
{
  private IStructuredSelection m_selection;

  private LegendView m_view;

  public final void init( final IViewPart view )
  {
    m_view = (LegendView)view;
  }

  protected final void handleError( final String msg )
  {
    Logger.getLogger( getClass().getName() ).info( "Fehler beim Löschen eines Datensatz: " + msg );

    MessageDialog.openWarning( m_view.getViewSite().getShell(), "Datensatz löschen", msg );
  }

  public final void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = (IStructuredSelection)selection;
  }
  
  protected final LegendView getView( )
  {
    return m_view;
  }
  
  protected final IStructuredSelection getSelection( )
  {
    return m_selection;
  }
}
