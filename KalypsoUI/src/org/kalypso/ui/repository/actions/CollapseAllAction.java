package org.kalypso.ui.repository.actions;

import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class CollapseAllAction extends AbstractRepositoryExplorerAction
{
  public CollapseAllAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Alles reduzieren",
        ImageProvider.IMAGE_ZML_REPOSITORY_COLLAPSE,
        "Reduziert alle Zweige der Baum" );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    getExplorer().getViewer().collapseAll();
  }
}