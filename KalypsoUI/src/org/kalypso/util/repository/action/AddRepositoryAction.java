package org.kalypso.util.repository.action;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.plugin.RepositoryProvider;
import org.kalypso.util.repository.IRepositoryContainer;
import org.kalypso.util.repository.file.FileRepository;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 *  
 */
public class AddRepositoryAction extends FullAction
{
  private final Shell m_shell;
  private final IRepositoryContainer m_cp;

  public AddRepositoryAction( final Shell shell, final IRepositoryContainer cp )
  {
    super( "Repository hinzufügen", null, "Fügt ein Repository hinzu..." );

    m_cp = cp;
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    ListDialog dlg = new ListDialog( m_shell );
    dlg.setLabelProvider( new LabelProvider() );
    dlg.setContentProvider( RepositoryProvider.getInstance() );
    dlg.setTitle( "Repository Typ auswählen" );
    dlg.setInput( new Object() );
    if( dlg.open() != Window.OK )
      return;

    String selType = (String)dlg.getResult()[0];

    DirectoryDialog fileDlg = new DirectoryDialog( m_shell, SWT.OPEN );
    if( fileDlg.open() == null )
      return;

    m_cp.addRepository( new FileRepository( selType, fileDlg.getFilterPath(), null ) );
  }
}