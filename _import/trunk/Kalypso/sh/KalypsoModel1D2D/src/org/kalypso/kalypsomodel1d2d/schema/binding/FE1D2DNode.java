/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class FE1D2DNode extends AbstractFeatureBinder
{
  public final static QName QNAME_FE1D2DNode = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DNode" );

  public final static QName QNAME_PROP_POINT = new QName( NS.GML3, "pointProperty" );

  private final FE1D2DDiscretisationModel m_discretisationModel;

  public FE1D2DNode( final Feature featureToBind )
  {
    super( featureToBind, QNAME_FE1D2DNode );

    m_discretisationModel = new FE1D2DDiscretisationModel( getFeature().getParent() );
  }

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( QNAME_PROP_POINT );
  }

  public void setPoint( final GM_Point point )
  {
    getFeature().setProperty( QNAME_PROP_POINT, point );
  }
  
  public static FE1D2DNode createNode( final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType nodeType = parentFeature.getFeatureType().getGMLSchema().getFeatureType( QNAME_FE1D2DNode );
    final Feature nodeFeature = parentFeature.getWorkspace().createFeature( parentFeature, nodeType );
    return new FE1D2DNode( nodeFeature );
  }

  public FE1D2DDiscretisationModel getDiscretisationModel( )
  {
    return m_discretisationModel;
  }
}
