package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Gernot Belger
 */
public class FE1D2D_2DElementTypeGeometry extends FeaturePropertyFunction
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
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    if( !GeometryCalcControl.doCalcElement )
      return null;

    final IFE1D2DElement element = (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
    if( element != null )
    {
      try
      {
        return element.recalculateElementGeometry();
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }
    else
    {
      // System.out.println(
      //          "Cannot get feature prop:"+ //$NON-NLS-1$
      //          "\n\tfeature="+feature+ //$NON-NLS-1$
      //          "\n\tproperty="+pt.getQName()+ //$NON-NLS-1$
      //          "currentValue="+currentValue); //$NON-NLS-1$
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: change underlying node geometry?
    return valueToSet;
  }

}
