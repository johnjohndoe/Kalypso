package org.kalypso.ogc.gml.table;

import org.deegree.model.feature.FeatureList;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;

/**
 * @author bce
 */
public class LayerTableContentProvider implements IStructuredContentProvider
{
  /**
   * Input muss ein IKalypsoFeatureTheme sein
   * Output sind die Features
   * 
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement == null || !(inputElement instanceof IKalypsoFeatureTheme ) )
      return new Object[] {};

    final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)inputElement;
    
    final FeatureList featureList = kft.getFeatureList();
    
    return featureList == null ? new Object[] {} : featureList.toFeatures();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, Object oldInput, Object newInput )
  {
    //
  }

}
