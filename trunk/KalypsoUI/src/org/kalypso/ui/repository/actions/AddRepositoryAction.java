package org.kalypso.ui.repository.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositorySpecification;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 */
public class AddRepositoryAction extends FullAction
{
  private final Shell m_shell;

  private final IRepositoryContainer m_cp;

  public AddRepositoryAction( final Shell shell, final IRepositoryContainer cp )
  {
    super( "Repository hinzufügen", ImageProvider.IMAGE_ZML_REPOSITORY_ADD, "Fügt ein Repository hinzu..." );

    m_cp = cp;
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final ListDialog dlg = new ListDialog( m_shell );
    dlg.setLabelProvider( new LabelProvider() );
    dlg.setContentProvider( new ArrayContentProvider() );
    dlg.setTitle( "Repository Typ auswählen" );
    dlg.setInput( KalypsoGisPlugin.getDefault().getRepositoriesSpecifications() );
    if( dlg.open() != Window.OK )
      return;

    final RepositorySpecification spec = (RepositorySpecification)dlg.getResult()[0];

    try
    {
      final IRepositoryFactory f = spec.createFactory( getClass().getClassLoader() );

      if( f.configureRepository( ) )
      {
        final IRepository rep = f.createRepository();

        // set all known properties for repository
        final String value = KalypsoGisPlugin.getDefault().getPluginPreferences().getString( IKalypsoPreferences.NUMBER_OF_DAYS );
        rep.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, value );

        m_cp.addRepository( rep );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      MessageDialog.openError( m_shell, "Repository hinzufügen", e.getLocalizedMessage() );
    }
  }
}