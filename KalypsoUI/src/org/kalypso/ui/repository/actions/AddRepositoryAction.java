package org.kalypso.ui.repository.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositorySpecification;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 */
public class AddRepositoryAction extends AbstractRepositoryExplorerAction
{
  public AddRepositoryAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Repository hinzufügen", ImageProvider.IMAGE_ZML_REPOSITORY_ADD, "Fügt ein Repository hinzu..." );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final ListDialog dlg = new ListDialog( getShell() );
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

        getRepositoryContainer().addRepository( rep );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      MessageDialog.openError( getShell(), "Repository hinzufügen", e.getLocalizedMessage() );
    }
  }
}