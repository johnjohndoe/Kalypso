package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Function property which compute the geometry of a junction context
 * 
 * @author Patrice Congo
 */
public class JunctionContextGeometry extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    // nothing to do
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final IJunctionContext1DToCLine element = 
            (IJunctionContext1DToCLine) feature.getAdapter( IJunctionContext1DToCLine.class );
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
      System.out.println(
          "Cannot get feature prop:"+
          "\n\tfeature="+feature+
          "\n\tproperty="+pt.getQName()+
          "currentValue="+currentValue);
    }

    return null;
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
