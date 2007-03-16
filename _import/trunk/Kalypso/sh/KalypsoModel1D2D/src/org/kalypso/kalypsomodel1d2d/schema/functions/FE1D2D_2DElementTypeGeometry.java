package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.FEJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEJunction1D2D;
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
    if(!GeometryCalcControl.doCalcEdge)
    {
      return null;
    }
    
    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      final FE1D2D_2DElement element;
      QName featureQName = featureType.getQName();
      if(featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ))
      {
        element = new FE1D2D_2DElement( feature );
        return element.recalculateElementGeometry();        
      }
      else if( GMLSchemaUtilities.substitutes( featureType, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine ) )
      {
        element = new FE1D2DContinuityLine( feature );
        return  element.recalculateElementGeometry();
      }
      else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D ) )
      {
        IFEJunction1D2D junction1D2D=new FEJunction1D2D(feature);
        return ModelGeometryBuilder.computeJunction1D2DGeometry( junction1D2D );
      }
      else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_ABSTRACT_JUNCTION1D2D ) )
      {
        // TODO: check if needed. Happens if we have an empty theme of thesse elements and the legend is active.
        
//        IFEJunction1D2D junction1D2D=new FEJunction1D2D(feature);
//        return ModelGeometryBuilder.computeJunction1D2DGeometry( junction1D2D );
        return null;
      }
      else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D ) )
      {
        IElement1D element1D  = new Element1D(feature);
        return ModelGeometryBuilder.computeElement1DGeometry( element1D );
      }
      else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE ) )
      {
        IFEEdgeToEdgeJunction1D2D junction1D2D=
          (IFEEdgeToEdgeJunction1D2D) feature.getAdapter( IFEEdgeToEdgeJunction1D2D.class );
       
        return ModelGeometryBuilder.computeEdgeToEdgeJunction1D2DGeometry( junction1D2D );
      }
      else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_CLINE ) )
      {
        IFEEdgeToCLineJunction1D2D junction1D2D=
          (IFEEdgeToCLineJunction1D2D) feature.getAdapter( IFEEdgeToCLineJunction1D2D.class );
       
        return ModelGeometryBuilder.computeEdgeToCLineJunction1D2DGeometry( junction1D2D );
      }
      else // TODO: test if its a ELEMENT_2D ?? because they may be other things around....
      {
        //TODO Patrice remove me
        element = new FE1D2D_2DElement( feature );
        return element.recalculateElementGeometry();
      }
    }
    catch( final GM_Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      return null;
    }
    
//    return currentValue;
  }

 

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: change underlying node geometry?
    return valueToSet;
  }

}
