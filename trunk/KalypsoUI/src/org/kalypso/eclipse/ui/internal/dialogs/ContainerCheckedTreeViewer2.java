package org.kalypso.eclipse.ui.internal.dialogs;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.internal.dialogs.ContainerCheckedTreeViewer;
import org.kalypso.eclipse.ui.IViewable;

/**
 * ContainerCheckedTreeViewer2 extends <code>ContainerCheckedTreeViewer</code>
 * and allows to update the check state of the items. Objects lying
 * behind the items displayed in this viewer should (but must not)
 * implement the <code>IViewable</code> interface.
 * 
 * @author schlienger
 */
public class ContainerCheckedTreeViewer2 extends ContainerCheckedTreeViewer
{
  public ContainerCheckedTreeViewer2( final Composite parent )
  {
    super( parent );
  }

  public ContainerCheckedTreeViewer2( final Composite parent, final int style )
  {
    super( parent, style );
  }

  public ContainerCheckedTreeViewer2( final Tree tree )
  {
    super( tree );
  }

  /**
   * @see org.eclipse.jface.viewers.TreeViewer#doUpdateItem(org.eclipse.swt.widgets.Item, java.lang.Object)
   */
  protected void doUpdateItem( final Item item, final Object element )
  {
    super.doUpdateItem( item, element );
    
    if( element instanceof IViewable )
    {
      final IViewable v = (IViewable) element;

      setChecked( item, v.isShown() );
    }
  }
}
