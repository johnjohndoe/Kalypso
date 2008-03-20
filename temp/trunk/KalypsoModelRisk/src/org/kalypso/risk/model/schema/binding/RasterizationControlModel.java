package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.UnversionedModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class RasterizationControlModel extends UnversionedModel implements IRasterizationControlModel
{
  private final FeatureList m_landuseClassesFeatureList;

  private final FeatureList m_assetValueClassesFeatureList;

  private final FeatureList m_damageFunctionsFeatureList;

  private final FeatureList m_riskZoneDefinitionsFeatureList;

  private final List<ILanduseClass> m_landuseClasses;

  private final List<IAssetValueClass> m_assetValueClasses;

  private final List<IDamageFunction> m_damageFunctions;

  private final List<IRiskZoneDefinition> m_riskZoneDefinitions;

  public RasterizationControlModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterizationControlModel.QNAME );
    m_landuseClassesFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
    m_landuseClasses = new ArrayList<ILanduseClass>();
    for( final Object object : m_landuseClassesFeatureList )
      m_landuseClasses.add( (ILanduseClass) ((Feature) object).getAdapter( ILanduseClass.class ) );

    m_assetValueClassesFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
    m_assetValueClasses = new ArrayList<IAssetValueClass>();
    for( final Object object : m_assetValueClassesFeatureList )
      m_assetValueClasses.add( (IAssetValueClass) ((Feature) object).getAdapter( IAssetValueClass.class ) );

    m_damageFunctionsFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );
    m_damageFunctions = new ArrayList<IDamageFunction>();
    for( final Object object : m_damageFunctionsFeatureList )
      m_damageFunctions.add( (IDamageFunction) ((Feature) object).getAdapter( IDamageFunction.class ) );

    m_riskZoneDefinitionsFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_RISKZONE_DEFINITION_MEMBER );
    m_riskZoneDefinitions = new ArrayList<IRiskZoneDefinition>();
    for( final Object object : m_riskZoneDefinitionsFeatureList )
      m_riskZoneDefinitions.add( (IRiskZoneDefinition) ((Feature) object).getAdapter( IRiskZoneDefinition.class ) );
  }

  public List<ILanduseClass> getLanduseClassesList( )
  {
    return m_landuseClasses;
  }

  public ILanduseClass createNewLanduseClass( )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_landuseClassesFeatureList, ILanduseClass.QNAME, -1 );
      final ILanduseClass landuseClass = (ILanduseClass) feature.getAdapter( ILanduseClass.class );
      m_landuseClasses.add( landuseClass );
      return landuseClass;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  public IDamageFunction createNewDamageFunction( )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_damageFunctionsFeatureList, IDamageFunction.QNAME, -1 );
      final IDamageFunction damageFunction = (IDamageFunction) feature.getAdapter( IDamageFunction.class );
      m_damageFunctions.add( damageFunction );
      return damageFunction;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  public IAssetValueClass createNewAssetValueClass( final Double value, final String name, final String description )
  {
    try
    {
      /* create new asset value feature */
      final Feature assetValueFeature = FeatureHelper.createFeatureForListProp( m_assetValueClassesFeatureList, IAssetValueClass.QNAME, -1 );
      final IAssetValueClass assetValueClass = (IAssetValueClass) assetValueFeature.getAdapter( IAssetValueClass.class );

      assetValueClass.setAssetValue( value );
      assetValueClass.setName( name );
      assetValueClass.setDescription( description );

      m_assetValueClasses.add( assetValueClass );

      return assetValueClass;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  public List<IAssetValueClass> getAssetValueClassesList( )
  {
    return m_assetValueClasses;
  }

  public List<IDamageFunction> getDamageFunctionsList( )
  {
    return m_damageFunctions;
  }

  public List<IRiskZoneDefinition> getRiskZoneDefinitionsList( )
  {
    return m_riskZoneDefinitions;
  }

  public boolean containsLanduseClass( final String landuseClassName )
  {
    for( final ILanduseClass landuseClass : m_landuseClasses )
      if( landuseClass.getName().equals( landuseClassName ) )
        return true;
    return false;
  }

  public int getNextAvailableLanduseClassOrdinalNumber( )
  {
    int maxOrdinal = 0;
    for( final ILanduseClass landuseClass : m_landuseClasses )
    {
      final int ordinalNumber = landuseClass.getOrdinalNumber();
      if( ordinalNumber > maxOrdinal )
        maxOrdinal = ordinalNumber;
    }
    return ++maxOrdinal;
  }

  // TODO: check if needed
  public List<String> getLanduseClassID( final String landuseClassName )
  {
    final List<String> list = new ArrayList<String>();
    for( final ILanduseClass landuseClass : m_landuseClasses )
      if( landuseClass.getName().equals( landuseClassName ) )
        list.add( landuseClass.getGmlID() );
    return list;
  }

  public void resetStatistics( )
  {
    for( final ILanduseClass landuseClass : m_landuseClasses )
    {
      landuseClass.setMinAnnualDamage( Double.POSITIVE_INFINITY );
      landuseClass.setMaxAnnualDamage( Double.NEGATIVE_INFINITY );
      landuseClass.setAverageAnnualDamage( 0.0 );
      landuseClass.setTotalDamage( 0.0 );
    }
  }

  // TODO: what is that good for? when gets the damage value negative? this should never happen! Maybe it is better to
  // check the values while processing / creating them!
  public void fixStatisticsForShowingToUser( )
  {
    for( final ILanduseClass landuseClass : m_landuseClasses )
    {
      if( landuseClass.getMaxAnnualDamage() < 0.0 )
        landuseClass.setMaxAnnualDamage( 0.0 );
      if( landuseClass.getMinAnnualDamage() > landuseClass.getMaxAnnualDamage() )
        landuseClass.setMinAnnualDamage( 0.0 );
    }
  }

  /**
   * checks if a damageFunction with the same name is already existing and returns the existing one. Otherwise null is
   * returned
   */
  public IDamageFunction getDamageFunction( final String name )
  {
    for( IDamageFunction damageFunction : m_damageFunctions )
    {
      if( damageFunction.getName().equals( name ) )
      {
        return damageFunction;
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getStatisticObs()
   */
  @SuppressWarnings("unchecked")
  public IObservation<TupleResult> getStatisticObs( )
  {
    return (IObservation<TupleResult>) getFeature().getProperty( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );
  }

}
