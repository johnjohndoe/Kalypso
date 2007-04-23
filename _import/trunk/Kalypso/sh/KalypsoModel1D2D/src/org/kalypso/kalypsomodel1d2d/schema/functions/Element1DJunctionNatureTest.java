package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Function property that implements a test to check
 * whether a 1D element is a juntion element
 * 
 * @author Patrice Congo
 */
public class Element1DJunctionNatureTest extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    //yes i know i am empty
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final IElement1D<IFE1D2DComplexElement<IFE1D2DElement>, IFE1D2DEdge> element = 
            (IElement1D) feature.getAdapter( IElement1D.class );
    if( element != null )
    {
      IFeatureWrapperCollection<IFE1D2DComplexElement<IFE1D2DElement>> containers = element.getContainers();
      for(IFE1D2DComplexElement complexElement:containers)
      {
        if(TypeInfo.isJuntionContext(complexElement))
        {
          return Boolean.TRUE;
        }
      }
      return Boolean.FALSE;
      
    }
    else
    {
      System.out.println(
          "Cannot get feature prop:"+
          "\n\tfeature="+feature+
          "\n\tproperty="+pt.getQName()+
          "currentValue="+currentValue);
    }

    return Boolean.FALSE;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }

}
