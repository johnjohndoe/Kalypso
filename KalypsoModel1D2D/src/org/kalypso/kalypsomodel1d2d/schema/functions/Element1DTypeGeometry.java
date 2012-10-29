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
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final IFE1D2DEdge edge = ((IElement1D)feature).getEdge();
    if( edge == null )
      return null;

    return edge.getGeometry();
  }

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }
}