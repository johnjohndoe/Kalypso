package org.kalypso.ui.repository.actions;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;
import org.kalypso.ui.repository.wizard.ExportAsFileWizard;


/**
 * @author schlienger
 */
public class ExportAsFileAction extends AbstractRepositoryExplorerAction implements ISelectionChangedListener
{
  public ExportAsFileAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Als Datei exportieren", ImageProvider.IMAGE_ZML_FILE, "Exportiert die selektierte Zeitreihe als lokale Datei");
    
    explorer.addSelectionChangedListener( this );
    setEnabled( explorer.isObservationSelected( explorer.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IObservation obs = getExplorer().isObservationSelected( getExplorer().getSelection() );
    if( obs == null )
      return;
    
    final WizardDialog dialog = new WizardDialog( getShell(), new ExportAsFileWizard( obs ) );
    dialog.open();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isObservationSelected( event.getSelection() ) != null );    
  }
}
