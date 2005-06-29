package org.kalypso.ui.editor.gmleditor.ui;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class FeatureTreeContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  /**/
  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
  // nichts tun
  }

  /**
   * @see ITreeContentProvider#getChildren(Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof Feature )
    {}
    else if( parentElement instanceof String )
    {}
    else if( parentElement instanceof List )
    {}
    else if( parentElement instanceof FeatureAssociationTypeProperty )
    {}

    return ( (IModel)parentElement ).getChildren();
  }

  /**
   * @see ITreeContentProvider#getParent(Object)
   */
  public Object getParent( final Object element )
  {
    // wir wissen nicht, wo das feature drin hängt
    return null;
  }

  /**
   * @see ITreeContentProvider#hasChildren(Object)
   */
  public boolean hasChildren( final Object element )
  {
    return ( (IModel)element ).hasChildren();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final GMLWorkspace workspace = (GMLWorkspace)inputElement;
    
    return new Object[] { workspace.getRootFeature() };
  }
}