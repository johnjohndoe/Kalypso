package org.kalypso.ogc.gml.util;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;

/**
 * @author Belger
 */
public class FeatureLabelProvider extends LabelProvider
{
  private final IFeatureModifier m_modifier;

  public FeatureLabelProvider( final IFeatureModifier modifier )
  {
    m_modifier = modifier;
  }
  
  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
    // nix zu disposen  
  }
  
  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( final Object element )
  {
    final Feature feature = (Feature)element;
    return m_modifier.getImage( feature );
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( final Object element )
  {
    final Feature feature = (Feature)element;

    return m_modifier.getLabel( feature ).toString();
  }
}
