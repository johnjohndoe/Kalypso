package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Returns the bounding box of the static model 
 * as {@link org.kalypsodeegree.model.geometry.GM_Surface}
 * 
 * @author Patrice Congo
 */
public class StaticModelBBoxFunc extends FeaturePropertyFunction
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
    
    
    final IStaticModel1D2D staticModel = 
            (IStaticModel1D2D) feature.getAdapter( IStaticModel1D2D.class );
    if( staticModel != null )
    {
      try
      {
        IFEDiscretisationModel1d2d discrModel = staticModel.getDiscretisationModel();
        final GM_Surface surface;
        if( discrModel != null )
        {
          IFeatureWrapperCollection<IFE1D2DNode> nodes = discrModel.getNodes();
          if( nodes.isEmpty() )
          {
            surface = null;
          }
          else
          {
            GM_Envelope bbox = nodes.getWrappedList().getBoundingBox();
            
             surface = 
                GeometryFactory.createGM_Surface( 
                    bbox, nodes.get( 0 ).getPoint().getCoordinateSystem() );
            
          }
        }
        else
        {
          surface = null;
        }
        
        System.out.println(
            "Cannot get feature prop:"+
            "\n\tfeature="+feature+
            "\n\tproperty="+pt.getQName()+
            "\n\tcurrentValue="+surface );
        return surface;
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
    return null;
  }

}
