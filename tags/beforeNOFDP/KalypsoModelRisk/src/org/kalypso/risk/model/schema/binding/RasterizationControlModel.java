package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

public class RasterizationControlModel extends UnversionedModel implements IRasterizationControlModel
{
  private final FeatureList m_landuseClassesFeatureList;

  private final FeatureList m_assetValueClassesFeatureList;

  private final FeatureList m_damageFunctionsFeatureList;

  private final FeatureList m_administrationUnitsFeatureList;

  private final FeatureList m_riskZoneDefinitionsFeatureList;

  private final List<ILanduseClass> m_landuseClasses;

  private final List<IAssetValueClass> m_assetValueClasses;

  private final List<IDamageFunction> m_damageFunctions;

  private final List<IAdministrationUnit> m_administrationUnits;

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

    m_administrationUnitsFeatureList = (FeatureList) getFeature().getProperty( IRasterizationControlModel.PROPERTY_ADMINISTRATION_UNIT_MEMBER );
    m_administrationUnits = new ArrayList<IAdministrationUnit>();
    for( final Object object : m_administrationUnitsFeatureList )
      m_administrationUnits.add( (IAdministrationUnit) ((Feature) object).getAdapter( IAdministrationUnit.class ) );

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

  public IAdministrationUnit createNewAdministrationUnit( final String name, final String description )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_administrationUnitsFeatureList, IAdministrationUnit.QNAME, -1 );
      final IAdministrationUnit administrationUnit = (IAdministrationUnit) feature.getAdapter( IAdministrationUnit.class );
      m_administrationUnits.add( administrationUnit );
      administrationUnit.setName( name );
      administrationUnit.setDescription( description );
      return administrationUnit;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  public IAssetValueClass getAssetValueClass( final String landuseClassGmlID, final String administrationUnitGmlID, final boolean createIfNotExists )
  {
    for( final IAssetValueClass assetValueClass : m_assetValueClasses )
      if( assetValueClass.getLanduseClassGmlID().equals( landuseClassGmlID ) && assetValueClass.getAdministrationUnitGmlID().equals( administrationUnitGmlID ) )
        return assetValueClass;
    if( createIfNotExists )
      return createNewAssetValueClass( landuseClassGmlID, administrationUnitGmlID, null, "" );
    return null;
  }

  public IAssetValueClass createNewAssetValueClass( final String landuseClassGmlID, final String administrationUnitGmlID, final Double value, final String description )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_assetValueClassesFeatureList, IAssetValueClass.QNAME, -1 );
      final IAssetValueClass assetValueClass = (IAssetValueClass) feature.getAdapter( IAssetValueClass.class );
      final String landusePath = IRasterizationControlModel.MODEL_NAME + "#" + landuseClassGmlID;
      final String administrationUnitPath = IRasterizationControlModel.MODEL_NAME + "#" + administrationUnitGmlID;
      Feature linkedLanduseClass = null;
      for( final ILanduseClass landuseClass : m_landuseClasses )
        if( landuseClass.getGmlID().equals( landuseClassGmlID ) )
        {
          linkedLanduseClass = landuseClass.getFeature();
          break;
        }
      if( linkedLanduseClass == null )
        return null;
      Feature linkedAdministrationUnit = null;
      for( final IAdministrationUnit administrationUnit : m_administrationUnits )
        if( administrationUnit.getGmlID().equals( administrationUnitGmlID ) )
        {
          linkedAdministrationUnit = administrationUnit.getFeature();
          break;
        }
      if( linkedAdministrationUnit == null )
        return null;
      final XLinkedFeature_Impl linkedLanduseClassXFeature = new XLinkedFeature_Impl( feature, linkedLanduseClass.getParentRelation(), linkedLanduseClass.getFeatureType(), landusePath, "", "", "", "", "" );
      feature.setProperty( IAssetValueClass.PROP_LANDUSE_CLASS_LINK, linkedLanduseClassXFeature );
      final XLinkedFeature_Impl linkedAdministrationUnitXFeature = new XLinkedFeature_Impl( feature, linkedAdministrationUnit.getParentRelation(), linkedAdministrationUnit.getFeatureType(), administrationUnitPath, "", "", "", "", "" );
      feature.setProperty( IAssetValueClass.PROP_ADMINISTRATION_UNIT_LINK, linkedAdministrationUnitXFeature );
      feature.setProperty( IAssetValueClass.PROP_ASSET_VALUE, value );
      feature.setProperty( IAssetValueClass.PROP_DESCRIPTION, description );
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

  public List<IAdministrationUnit> getAdministrationUnits( )
  {
    return m_administrationUnits;
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
      landuseClass.setMinDamage( Double.POSITIVE_INFINITY );
      landuseClass.setMaxDamage( Double.NEGATIVE_INFINITY );
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
      if( landuseClass.getMaxDamage() < 0.0 )
        landuseClass.setMaxDamage( 0.0 );
      if( landuseClass.getMinDamage() > landuseClass.getMaxDamage() )
        landuseClass.setMinDamage( 0.0 );
    }
  }
}
