package org.kalypso.util.repository.action;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.repository.IRepositoryContainer;
import org.kalypso.util.repository.IRepositoryFactory;
import org.kalypso.util.repository.RepositoryException;
import org.kalypso.util.repository.RepositorySpecification;

/**
 * Ein Repository hinzuf�gen.
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
    super( "Repository hinzuf�gen", null, "F�gt ein Repository hinzu..." );

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
    dlg.setContentProvider( new ArrayContentProvider(  ) );
    dlg.setTitle( "Repository Typ ausw�hlen" );
    dlg.setInput( KalypsoGisPlugin.getDefault().getRepositoriesSpecifications() );
    if( dlg.open() != Window.OK )
      return;

    RepositorySpecification spec = (RepositorySpecification)dlg.getResult()[0];
    IRepositoryFactory f = spec.createFactory();
    
    if( f.configureRepository( m_shell, null ) )
      try
      {
        m_cp.addRepository( f.createRepository() );
      }
      catch( RepositoryException e )
      {
        // TODO: logging
        e.printStackTrace();
      }
  }
}
