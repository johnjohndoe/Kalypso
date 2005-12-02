package org.kalypso.wiskiadapter;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

/**
 * WiskiRepositoryFactory
 * 
 * @author schlienger
 */
public class WiskiRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository() throws RepositoryException
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    final InputDialog dlg = new InputDialog( shell, "Wiski-Verbindung", "Geben Sie hier die Wiski-Verbindungsinformation ein", getConfiguration(), new WiskiConfigValidator() );
    if( dlg.open() == Window.OK )
    {
      setConfiguration( dlg.getValue() );

      return true;
    }

    return false;
  }

  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    return new WiskiRepository( "Wiski", getClass().getName(), getConfiguration(), false );
  }
}
