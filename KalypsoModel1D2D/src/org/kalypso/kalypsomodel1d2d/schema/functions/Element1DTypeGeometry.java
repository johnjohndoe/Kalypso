package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Patrice Congo
 */
public class Element1DTypeGeometry extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature, org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final IFE1D2DEdge edge = ((IElement1D)feature).getEdge();
    if( edge == null )
    {
      return null;
    }
    return edge.getGeometry();
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature, org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }

}
