package org.kalypso.ui.repository.actions;

import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;


/**
 * @author schlienger
 */
public class ExportAsFileAction extends AbstractRepositoryExplorerAction
{

  public ExportAsFileAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Als Datei exportieren", ImageProvider.IMAGE_ZML_FILE, "Exportiert die selektierte Zeitreihe als lokale Datei");
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    // TODO: implement it Marc. Question: how should the date range be handled? Maybe ask the user...
  }
}
