package org.kalypso.ui.wizard.ocs.idtable;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * IdTableLabelProvider
 * 
 * @author schlienger
 */
public class IdTableLabelProvider implements ITableLabelProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object,
   *      int)
   */
  public Image getColumnImage( Object element, int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object,
   *      int)
   */
  public String getColumnText( Object element, int columnIndex )
  {
    final IdStruct ids = (IdStruct) element;

    switch( columnIndex )
    {
      case 0:
        return ids.getFile().getName();
      case 1:
        return ids.getId();
      default:
        return "";
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( ILabelProviderListener listener )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose( )
  {
    //  empty
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
   *      java.lang.String)
   */
  public boolean isLabelProperty( Object element, String property )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( ILabelProviderListener listener )
  {
    //  empty
  }
}