package org.kalypso.model.wspm.ui.profil.view.table.swt;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TableViewer;

public abstract class ProfilTableAction extends Action
{
  private final ProfilSWTTableView m_tableView;

  public ProfilTableAction( final ProfilSWTTableView tableView )
  {
    this( tableView, null, null );
  }

  public ProfilTableAction( final ProfilSWTTableView tableView, final String text )
  {
    this( tableView, text, null );
  }

  public ProfilTableAction( final ProfilSWTTableView tableView, final String text, final ImageDescriptor image )
  {
    super( text, image );
    
    m_tableView = tableView;
  }

  protected final TableViewer getViewer( )
  {
    return m_tableView.getViewer();
  }
}
