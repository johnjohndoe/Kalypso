package org.kalypso.ui.repository.actions;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.window.Window;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.ui.repository.dialogs.NumberOfDaysInputDialog;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Configure preview daterange for current <code>IRepository</code>.
 * 
 * @author schlienger
 */
public class ConfigurePreviewAction extends AbstractRepositoryExplorerAction implements ISelectionChangedListener
{
  public ConfigurePreviewAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Einstellungen", ImageProvider.IMAGE_ZML_REPOSITORY_CONF,
        "Einstellungen der Zeitreihen-Vorschau setzen" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    final int days1;
    final String defValue = rep.getProperty( IKalypsoPreferences.NUMBER_OF_DAYS );
    if( defValue != null )
      days1 = Integer.valueOf( defValue ).intValue();
    else
      days1 = KalypsoGisPlugin.getDefault().getPluginPreferences().getInt(
          IKalypsoPreferences.NUMBER_OF_DAYS );

    final NumberOfDaysInputDialog dlg = new NumberOfDaysInputDialog( getShell(), days1 );

    if( dlg.open() == Window.OK )
    {
      final int days = dlg.getDays();

      rep.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, String.valueOf( days ) );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }
}