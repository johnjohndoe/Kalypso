package org.kalypso.util.repository.action;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.plugin.RepositoryProvider;
import org.kalypso.util.repository.file.FileRepository;
import org.kalypso.util.repository.view.RepositoriesContentProvider;

/**
 * Ein Repository hinzuf�gen.
 * 
 * @author schlienger
 *
 */
public class AddRepositoryAction extends FullAction
{
  private final RepositoriesContentProvider m_cp;
  private final Shell m_shell;

  public AddRepositoryAction( final Shell shell, final RepositoriesContentProvider cp )
  {
    super( "Repository hinzuf�gen", null, "F�gt ein Repository hinzu..." );
    
    m_cp = cp;
    m_shell = shell;
  }
  
  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
      ListSelectionDialog dlg = new ListSelectionDialog( m_shell, new Object(), RepositoryProvider.getInstance(), new LabelProvider(), "Repository Typ ausw�hlen" );
      dlg.setTitle("Repositories");
      if( dlg.open() != Window.OK )
         return;

      String selType = (String)dlg.getResult()[0];
      
      DirectoryDialog fileDlg = new DirectoryDialog( m_shell, SWT.OPEN );
      if( fileDlg.open() == null )
          return;
      
      m_cp.addRepository( new FileRepository( selType, fileDlg.getFilterPath(), null ) );
  }
}
