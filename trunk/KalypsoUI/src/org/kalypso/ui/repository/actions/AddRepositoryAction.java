package org.kalypso.ui.repository.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.RepositorySpecification;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
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
    super( explorer, "Repository hinzufügen",
        ImageProvider.IMAGE_ZML_REPOSITORY_ADD, "Fügt ein Repository hinzu..." );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run( )
  {
    final ListDialog dlg = new ListDialog( getShell() );
    dlg.setLabelProvider( new LabelProvider() );
    dlg.setContentProvider( new ArrayContentProvider() );
    dlg.setTitle( "Repository Typ auswählen" );
    dlg
        .setInput( KalypsoGisPlugin.getDefault()
            .getRepositoriesSpecifications() );
    if( dlg.open() != Window.OK )
      return;

    final RepositorySpecification spec = (RepositorySpecification) dlg
        .getResult()[0];

    try
    {
      final IRepositoryFactory f = spec.createFactory( getClass()
          .getClassLoader() );

      if( f.configureRepository() )
      {
        final IProgressService progressService = PlatformUI.getWorkbench()
            .getProgressService();
        progressService.busyCursorWhile( new IRunnableWithProgress()
        {
          public void run( IProgressMonitor monitor ) throws InvocationTargetException
          {
            monitor.beginTask( "Repository hinzufügen", 2 );

            final IRepository rep;
            try
            {
              rep = f.createRepository();
              
              monitor.worked(1);
              
              rep.setProperties( KalypsoGisPlugin.getDefault().getDefaultRepositoryProperties() );
              
              getRepositoryContainer().addRepository( rep );
              
              monitor.worked(1);
            }
            catch( RepositoryException e )
            {
              throw new InvocationTargetException( e );
            }
            finally
            {
              monitor.done();
            }
          }
        } );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      MessageDialog.openError( getShell(), "Repository hinzufügen", e
          .getLocalizedMessage() );
    }
  }
}