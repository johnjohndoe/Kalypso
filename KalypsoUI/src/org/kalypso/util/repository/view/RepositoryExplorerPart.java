package org.kalypso.util.repository.view;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.util.repository.action.AddRepositoryAction;

/**
 * 
 * 
 * @author schlienger
 */
public class RepositoryExplorerPart extends ViewPart
{
  private TreeViewer m_viewer = null;
  private RepositoriesContentProvider m_contentProvider = null;
  
  public RepositoryExplorerPart(  )
  {
    super();
    
    m_contentProvider = new RepositoriesContentProvider();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    m_viewer = new TreeViewer( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    
    m_viewer.setContentProvider( m_contentProvider );
    
    getViewSite().getActionBars().getToolBarManager().add( new AddRepositoryAction( getSite().getShell(), m_contentProvider) );
    getViewSite().getActionBars().updateActionBars();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // TODO
  }
}
