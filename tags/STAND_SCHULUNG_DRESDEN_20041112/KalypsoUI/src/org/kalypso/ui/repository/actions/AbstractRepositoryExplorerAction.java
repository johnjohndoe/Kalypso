package org.kalypso.ui.repository.actions;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Superclass of all actions provided by the RepositoryExplorer.
 * 
 * @author schlienger
 */
public abstract class AbstractRepositoryExplorerAction extends org.kalypso.eclipse.jface.action.FullAction
{
  private RepositoryExplorerPart m_explorer;

  /**
   * Creates a new instance of the class.
   * @param explorer
   * @param text
   * @param image
   * @param tooltipText
   */
  public AbstractRepositoryExplorerAction( final RepositoryExplorerPart explorer, final String text,
      final ImageDescriptor image, final String tooltipText )
  {
    super( text, image, tooltipText );

    m_explorer = explorer;
  }

  /**
   * @return repository explorer
   */
  public RepositoryExplorerPart getExplorer()
  {
    return m_explorer;
  }

  /**
   * @return the resource viewer
   */
  protected Viewer getViewer()
  {
    return getExplorer().getViewer();
  }

  /**
   * @return the shell to use within actions.
   */
  protected Shell getShell()
  {
    return m_explorer.getSite().getShell();
  }
  
  /**
   * @return the repository container
   */
  protected IRepositoryContainer getRepositoryContainer()
  {
    return m_explorer.getRepositoryContainer();
  }
}