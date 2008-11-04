package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.UnversionedModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class RasterizationControlModel extends UnversionedModel implements IRasterizationControlModel
{
  private final FeatureList m_landuseClassesFeatureList;

  private final List<ILanduseClass> m_landuseClasses;

  private final FeatureWrapperCollection<IAssetValueClass> m_assetValueClasses;

  private final FeatureWrapperCollection<IDamageFunction> m_damageFunctions;

  private final FeatureWrapperCollection<IRiskZoneDefinition> m_riskZoneDefinitions;

  public RasterizationControlModel( final Feature featureToBind )
  {
    super( featureToBind, IRasterizationControlModel.QNAME );
    m_landuseClassesFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
    m_landuseClasses = new ArrayList<ILanduseClass>();
    for( final Object object : m_landuseClassesFeatureList )
      m_landuseClasses.add( (ILanduseClass) ((Feature) object).getAdapter( ILanduseClass.class ) );

    m_assetValueClasses = new FeatureWrapperCollection<IAssetValueClass>( getFeature(), IAssetValueClass.class, IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
    m_damageFunctions = new FeatureWrapperCollection<IDamageFunction>( getFeature(), IDamageFunction.class, IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );
    m_riskZoneDefinitions = new FeatureWrapperCollection<IRiskZoneDefinition>( getFeature(), IRiskZoneDefinition.class, IRasterizationControlModel.PROPERTY_RISKZONE_DEFINITION_MEMBER );

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
    return m_damageFunctions.addNew( IDamageFunction.QNAME );
  }

  public IAssetValueClass getAssetValueClass( final Double value, final String name, final String description )
  {
    for( final IAssetValueClass assetClass : m_assetValueClasses )
    {
      if( assetClass.getName().equals( name ) && assetClass.getAssetValue().equals( value ) && assetClass.getDescription().equals( description ) )
        return assetClass;
    }

    final IAssetValueClass assetValueClass = createNewAssetValueClass();

    assetValueClass.setAssetValue( value );
    assetValueClass.setName( name );
    assetValueClass.setDescription( description );

    return assetValueClass;

  }

  public IAssetValueClass createNewAssetValueClass( )
  {
    return m_assetValueClasses.addNew( IAssetValueClass.QNAME );
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
   * checks if a damageFunction with the same name, description and value (function) is already existing and returns the
   * existing one. Otherwise null is returned
   */
  public IDamageFunction getDamageFunction( final String name, final String value, final String description )
  {
    for( final IDamageFunction damageFunction : m_damageFunctions )
    {
      if( damageFunction.getName().equals( name ) && damageFunction.getDescription().equals( description ) && damageFunction.getFunction().equals( value ) )
        return damageFunction;
    }
    return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getStatisticObs()
   */
  public IObservation<TupleResult> getStatisticObs( )
  {
    // final Feature feature = getStatisticObsFeature();
    // FIXME
    throw new NotImplementedException();
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getStatisticObsFeature()
   */
  public Feature getStatisticObsFeature( )
  {
    return (Feature) getFeature().getProperty( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getRiskCalculationType()
   */
  @Override
  public RISK_CALCULATION_TYPE getRiskCalculationType( )
  {
    final Object value = getFeature().getProperty( IRasterizationControlModel.PROPERTY_RISK_CALCULATION_TYPE );
    if( value == null )
      return RISK_CALCULATION_TYPE.VALUE_NOT_SET;
    for( final RISK_CALCULATION_TYPE type : RISK_CALCULATION_TYPE.values() )
      if( type.value().equals( value ) )
        return type;
    return RISK_CALCULATION_TYPE.VALUE_NOT_SET;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#setRiskCalculationType(org.kalypso.risk.model.schema.binding.IRasterizationControlModel.RISK_CALCULATION_TYPE)
   */
  @Override
  public void setRiskCalculationType( RISK_CALCULATION_TYPE riskCalculationType )
  {
    getFeature().setProperty( IRasterizationControlModel.PROPERTY_RISK_CALCULATION_TYPE, riskCalculationType.value() );
  }
}
