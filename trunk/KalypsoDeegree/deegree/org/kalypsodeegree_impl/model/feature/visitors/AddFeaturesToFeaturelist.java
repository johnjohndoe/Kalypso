package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.Properties;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Fügt neue Features in eine {@link org.kalypsodeegree.model.feature.FeatureList} ein.
 * Dabei wird für jedes besuchte Feature ein neues (Default) Feature erzeugt in welches
 * Werte aus dem besuchten Feature via {@link org.kalypsodeegree_impl.model.feature.FeatureHelper#copyProperties(Feature, Feature, Properties)}
 * übertragen werden. 
 * 
 * @author bce
 */
public class AddFeaturesToFeaturelist implements FeatureVisitor
{
  private final FeatureList m_list;
  private final Properties m_propertyMap;
  private final FeatureType m_featureType;

  public AddFeaturesToFeaturelist( final FeatureList list, final Properties propertyMap )
  {
    this( list, propertyMap, ((Feature)list.get( 0 )).getFeatureType() );
  }

  public AddFeaturesToFeaturelist( final FeatureList list, final Properties propertyMap, final FeatureType featureType )
  {
    m_list = list;
    m_featureType = featureType;
    m_propertyMap = propertyMap;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Feature feature = FeatureFactory.createDefaultFeature( m_featureType, false );
    FeatureHelper.copyProperties( f, feature, m_propertyMap );
    m_list.add( feature );
    
    return true;
  }

}
