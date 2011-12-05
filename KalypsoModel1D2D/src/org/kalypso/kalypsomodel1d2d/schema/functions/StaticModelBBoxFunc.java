package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Returns the bounding box of the static model as {@link org.kalypsodeegree.model.geometry.GM_Surface}
 *
 * @author Patrice Congo
 */
public class StaticModelBBoxFunc extends FeaturePropertyFunction
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

    final IStaticModel1D2D staticModel = (IStaticModel1D2D) feature.getAdapter( IStaticModel1D2D.class );
    if( staticModel != null )
    {
      try
      {
        final IFEDiscretisationModel1d2d discrModel = staticModel.getDiscretisationModel();
        final GM_Curve geometry;
        if( discrModel != null )
        {

          final IFeatureWrapperCollection<IFE1D2DNode> nodes = discrModel.getNodes();
          if( nodes.isEmpty() )
          {
            geometry = null;
          }
          else
          {
            final GM_Envelope bbox = nodes.getWrappedList().getBoundingBox();
            String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();
            if( crs == null )
              crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
            geometry = GeometryUtilities.toGM_Curve( bbox, crs );

            // GeometryFactory.createGM_Surface(bbox, crs );
            // TODO Patrice Complete
          }
        }
        else
        {
          geometry = null;
        }

        //        System.out.println( "got feature prop:" + "\n\tfeature=" + feature + "\n\tproperty=" + pt.getQName() + "\n\tcurrentValue=" + geometry ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        return geometry;
      }
      catch( final Throwable e )
      {
        e.printStackTrace();
      }
    }
    else
    {
      //      System.out.println( "Cannot get feature prop:" + "\n\tfeature=" + feature + "\n\tadapter=" + staticModel + "\n\tproperty=" + pt.getQName() + "\n\tcurrentValue=" + currentValue ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
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
    return null;
  }

}
