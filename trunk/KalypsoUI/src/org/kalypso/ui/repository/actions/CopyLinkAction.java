package org.kalypso.ui.repository.actions;

import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class CopyLinkAction extends AbstractRepositoryExplorerAction
{
  public CopyLinkAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Link kopieren", ImageProvider.IMAGE_OBSERVATION_LINK, "Kopiert den Link in der Zwischenablage für die ausgewählte Zeitreihe" );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IObservation obs = getExplorer().isObservationSelected( getExplorer().getSelection() );
    if( obs == null )
      return;

    // TODO: remove this output once testing finished
    System.out.println("Link: " + obs.getIdentifier() );
    
    final Clipboard clipboard = new Clipboard( getExplorer().getSite().getShell().getDisplay() );
    clipboard.setContents(new Object[]{ obs.getIdentifier() }, new Transfer[]{TextTransfer.getInstance() });
    clipboard.dispose();
  }
}
