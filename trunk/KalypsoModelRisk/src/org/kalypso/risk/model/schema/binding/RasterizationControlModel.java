package org.kalypso.risk.model.schema.binding;

import java.util.List;

import org.apache.xmlbeans.impl.common.Levenshtein;
import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

public class RasterizationControlModel extends UnversionedModel implements IRasterizationControlModel
{
  public RasterizationControlModel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_landuseClasses = new FeatureBindingCollection<>( this, ILanduseClass.class, IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
    m_assetValueClasses = new FeatureBindingCollection<>( this, IAssetValueClass.class, IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
    m_damageFunctions = new FeatureBindingCollection<>( this, IDamageFunction.class, IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );
    m_riskZoneDefinitions = new FeatureBindingCollection<>( this, IRiskZoneDefinition.class, IRasterizationControlModel.PROPERTY_RISKZONE_DEFINITION_MEMBER );
  }

  private final FeatureBindingCollection<ILanduseClass> m_landuseClasses;

  private final FeatureBindingCollection<IAssetValueClass> m_assetValueClasses;

  private final FeatureBindingCollection<IDamageFunction> m_damageFunctions;

  private final FeatureBindingCollection<IRiskZoneDefinition> m_riskZoneDefinitions;

  @Override
  public List<ILanduseClass> getLanduseClassesList( )
  {
    return m_landuseClasses;
  }

  @Override
  public ILanduseClass createNewLanduseClass( )
  {
    final ILanduseClass landuseClass = m_landuseClasses.addNew( LanduseClass.QNAME );
    return landuseClass;
  }

  @Override
  public IDamageFunction createNewDamageFunction( )
  {
    return m_damageFunctions.addNew( IDamageFunction.QNAME );
  }

  @Override
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

  @Override
  public IAssetValueClass createNewAssetValueClass( )
  {
    return m_assetValueClasses.addNew( IAssetValueClass.QNAME );
  }

  @Override
  public List<IAssetValueClass> getAssetValueClassesList( )
  {
    return m_assetValueClasses;
  }

  @Override
  public List<IDamageFunction> getDamageFunctionsList( )
  {
    return m_damageFunctions;
  }

  @Override
  public List<IRiskZoneDefinition> getRiskZoneDefinitionsList( )
  {
    return m_riskZoneDefinitions;
  }

  @Override
  public boolean containsLanduseClass( final String landuseClassName )
  {
    for( final ILanduseClass landuseClass : m_landuseClasses )
      if( landuseClass.getName().equals( landuseClassName ) )
        return true;
    return false;
  }

  @Override
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

  /**
   * checks if a damageFunction with the same name, description and value (function) is already existing and returns the
   * existing one. Otherwise null is returned
   */
  @Override
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
  @Override
  public IObservation<TupleResult> getStatisticObs( )
  {
    // final Feature feature = getStatisticObsFeature();
    // FIXME
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getStatisticObsFeature()
   */
  @Override
  public Feature getStatisticObsFeature( )
  {
    return (Feature) getProperty( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getSuggestedAssetValueClass(java.lang.String)
   */
  @Override
  public IAssetValueClass getSuggestedAssetValueClass( final String name )
  {
    int minLevenshteinDistance = Integer.MAX_VALUE;
    IAssetValueClass mostSimilarByLevenshtein = null;
    IAssetValueClass mostSimilarBySubstring = null;
    for( final IAssetValueClass assetValueClass : m_assetValueClasses )
    {
      final String className = assetValueClass.getName();
      if( name.equalsIgnoreCase( className ) )
        return assetValueClass;
      final int levenshteinDistance = Levenshtein.distance( name, className );
      if( levenshteinDistance < minLevenshteinDistance )
      {
        minLevenshteinDistance = levenshteinDistance;
        mostSimilarByLevenshtein = assetValueClass;
      }
      if( name.indexOf( className ) > -1 || className.indexOf( name ) > -1 )
        mostSimilarBySubstring = assetValueClass;
    }
    if( mostSimilarBySubstring != null )
      return mostSimilarBySubstring;
    if( minLevenshteinDistance < 3 )
      return mostSimilarByLevenshtein;
    return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRasterizationControlModel#getSuggestedDamageFunction(java.lang.String)
   */
  @Override
  public IDamageFunction getSuggestedDamageFunction( final String name )
  {
    int minLevenshteinDistance = Integer.MAX_VALUE;
    IDamageFunction mostSimilarByLevenshtein = null;
    IDamageFunction mostSimilarBySubstring = null;
    for( final IDamageFunction damageFunction : m_damageFunctions )
    {
      final String className = damageFunction.getName();
      if( name.equalsIgnoreCase( className ) )
        return damageFunction;
      final int levenshteinDistance = Levenshtein.distance( name, className );
      if( levenshteinDistance < minLevenshteinDistance )
      {
        minLevenshteinDistance = levenshteinDistance;
        mostSimilarByLevenshtein = damageFunction;
      }
      if( name.indexOf( className ) > -1 || className.indexOf( name ) > -1 )
        mostSimilarBySubstring = damageFunction;
    }
    if( mostSimilarBySubstring != null )
      return mostSimilarBySubstring;
    if( minLevenshteinDistance < 3 )
      return mostSimilarByLevenshtein;
    return null;
  }
}
