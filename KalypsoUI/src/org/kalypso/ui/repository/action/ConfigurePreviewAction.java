package org.kalypso.ui.repository.action;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.dialog.NumberOfDaysInputDialog;

/**
 * @author schlienger
 */
public class ConfigurePreviewAction extends FullAction
{
  private final Shell m_shell;

  public ConfigurePreviewAction( final Shell shell )
  {
    super( "Einstellungen", ImageProvider.IMAGE_ZML_REPOSITORY_CONF, "Einstellungen der Zeitreihen-Vorschau setzen" );
    
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final NumberOfDaysInputDialog dlg = new NumberOfDaysInputDialog( m_shell, 30 );
    
    if( dlg.open() == Window.OK )
    {
      dlg.getDays();
    }
  }
}
