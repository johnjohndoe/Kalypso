package org.kalypso.editor.mapeditor;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.xml.types.ILayerlistProvider;

/**
 * <p>Ein Content Provder für die Layer-View</p>
 * <p>Das InputElement muss vom Typ {@link org.kalypso.editor.mapeditor.GisMapEditor} sein
 * 
 * @author gernot
 */
public class LayerlistContentProvider implements IStructuredContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
    // nix zun tun?
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    // nix tun, wird immer on demand geholt
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final List layers = ((ILayerlistProvider)inputElement).getLayerlist();
    
    if( layers == null )
      return new Object[] {};

    return layers.toArray(  );
  }
}
