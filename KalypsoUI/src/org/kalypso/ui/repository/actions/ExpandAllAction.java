package org.kalypso.ui.repository.actions;

import org.eclipse.jface.viewers.TreeViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class ExpandAllAction extends AbstractRepositoryExplorerAction
{
  public ExpandAllAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Alles erweitern", ImageProvider.IMAGE_ZML_REPOSITORY_EXPAND, "Erweitert alle Zweige der Baum" );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    ( (TreeViewer)getExplorer().getViewer() ).expandAll();
  }
}
