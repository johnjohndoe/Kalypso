package org.kalypso.ui.repository.actions;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.jface.action.FullAction;
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
public class ConfigurePreviewAction extends FullAction implements ISelectionChangedListener
{
  private final Shell m_shell;

  private final RepositoryExplorerPart m_explorer;

  public ConfigurePreviewAction( final Shell shell, final RepositoryExplorerPart explorer )
  {
    super( "Einstellungen", ImageProvider.IMAGE_ZML_REPOSITORY_CONF,
        "Einstellungen der Zeitreihen-Vorschau setzen" );

    m_shell = shell;

    m_explorer = explorer;
    m_explorer.addSelectionChangedListener( this );

    setEnabled( m_explorer.isRepository( m_explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IRepository rep = m_explorer.isRepository( m_explorer.getSelection() );
    if( rep == null )
      return;

    final int days1;
    final String defValue = rep.getProperty( IKalypsoPreferences.NUMBER_OF_DAYS );
    if( defValue != null )
      days1 = Integer.valueOf( defValue ).intValue();
    else
      days1 = KalypsoGisPlugin.getDefault().getPluginPreferences().getInt(
          IKalypsoPreferences.NUMBER_OF_DAYS );

    final NumberOfDaysInputDialog dlg = new NumberOfDaysInputDialog( m_shell, days1 );

    if( dlg.open() == Window.OK )
    {
      final int days = dlg.getDays();

      rep.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, String.valueOf( days ) );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( m_explorer.isRepository( event.getSelection() ) != null );
  }

  public void dispose()
  {
    m_explorer.removeSelectionChangedListener( this );
  }
}