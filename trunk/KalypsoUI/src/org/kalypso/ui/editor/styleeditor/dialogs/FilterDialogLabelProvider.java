package org.kalypso.ui.editor.styleeditor.dialogs;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * @author F.Lindemann
 */
public class FilterDialogLabelProvider implements ILabelProvider
{

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( Object element )
  {
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( Object element )
  {
    if( element != null )
    {
      return ( (FilterDialogTreeNode)element ).getName();
    }
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( ILabelProviderListener listener )
  {/**/
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
  /**/
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
   *            java.lang.String)
   */
  public boolean isLabelProperty( Object element, String property )
  {
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( ILabelProviderListener listener )
  {
  /**/
  }

}