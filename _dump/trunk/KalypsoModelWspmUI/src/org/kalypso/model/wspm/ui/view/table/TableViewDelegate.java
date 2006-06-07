package org.kalypso.model.wspm.ui.view.table;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;


public abstract class TableViewDelegate implements IViewActionDelegate
{
  private final String m_tableViewActionID;

  private IAction m_action;

  public TableViewDelegate( final String tableViewActionID )
  {
    m_tableViewActionID = tableViewActionID;
  }
  
  public void init( final IViewPart view )
  {
    if( view instanceof TableView )
      m_action = ((TableView)view).getTableView().getAction( m_tableViewActionID );
  }

  public void run( final IAction action )
  {
    if( m_action != null )
      m_action.run();
  }

  public void selectionChanged( final IAction action, final ISelection selection )
  {
  }
}
