package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class RasterizationControlModel extends AbstractFeatureBinder implements IRasterizationControlModel
{
  private FeatureList m_landuseClassesFeatureList;
  
  private List<ILanduseClass> m_landuseClasses;

  public RasterizationControlModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterizationControlModel.QNAME );
    m_landuseClassesFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
    m_landuseClasses = new ArrayList<ILanduseClass>();
    for( final Object object : m_landuseClassesFeatureList )
      m_landuseClasses.add( (ILanduseClass) ((Feature)object).getAdapter( ILanduseClass.class ) );
  }

  public List<ILanduseClass> getLanduseClassesList( )
  {
    return m_landuseClasses;
  }

  public ILanduseClass createNewLanduseClass( )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_landuseClassesFeatureList, IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER, ILanduseClass.QNAME );
      final ILanduseClass landuseClass = (ILanduseClass) feature.getAdapter( ILanduseClass.class );
      m_landuseClasses.add( landuseClass );
      return landuseClass;
    }
    catch( GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // TODO Auto-generated method stub
    return null;
  }
}
