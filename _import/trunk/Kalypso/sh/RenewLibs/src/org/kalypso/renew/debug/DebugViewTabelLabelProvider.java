package org.kalypso.renew.debug;

import java.rmi.RemoteException;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import de.renew.workflow.WorkItem;

public class DebugViewTabelLabelProvider extends LabelProvider implements ITableLabelProvider
{

  public Image getColumnImage( Object element, int columnIndex )
  {
    return null;
  }

  public String getColumnText( Object element, int columnIndex )
  {
    if( element instanceof WorkItem )
    {
      WorkItem wi = (WorkItem) element;
      try
      {
        switch( columnIndex )
        {
          case 0:
            return wi.getTask().getName();
          case 1:
            return wi.getTask().getClassName();
          case 2:
            return wi.getTask().getSettings().toString();
          case 3:
            return wi.getParameter() != null ? wi.getParameter().toString() : "";
          case 4:
            return "" + wi.getPriority();
        }
      }
      catch( RemoteException e )
      {
        e.printStackTrace();
      }
    }
    return null;
  }
}
