package org.kalypso.ui.repository.action;

import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.jface.action.FullAction;

/**
 * @author schlienger
 */
public class ConfigurePreviewAction extends FullAction
{
  private final Shell m_shell;

  public ConfigurePreviewAction( final Shell shell )
  {
    super( "Einstellungen", null, "Einstellungen von der Zeitreihen-Vorschau setzen" );
    
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    super.run();
  }
}
