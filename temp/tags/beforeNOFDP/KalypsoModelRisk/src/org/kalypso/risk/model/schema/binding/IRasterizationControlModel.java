package org.kalypso.risk.model.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

public interface IRasterizationControlModel extends IModel
{
  public static final String MODEL_NAME = "RasterizationControlModel.gml";

  public static final double RISKZONE_URBANAREA_LOW = 1.0;

  public static final double RISKZONE_URBANAREA_MIDDLE = 2.0;

  public static final double RISKZONE_URBANAREA_HIGH = 3.0;

  public static final double RISKZONE_NONURBANAREA_LOW = 4.0;

  public static final double RISKZONE_NONURBANAREA_MIDDLE = 5.0;

  public static final double RISKZONE_NONURBANAREA_HIGH = 6.0;

  public static final QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RasterizationControlModel" );

  public static final QName PROPERTY_LANDUSE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" );

  public static final QName PROPERTY_ASSET_VALUE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "assetValueClassMember" );

  public static final QName PROPERTY_ADMINISTRATION_UNIT_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "administrationUnitMember" );

  public static final QName PROPERTY_DAMAGE_FUNCTION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionMember" );

  public static final QName PROPERTY_RISKZONE_DEFINITION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskZoneDefinitionMember" );

  public List<ILanduseClass> getLanduseClassesList( );

  public List<IAssetValueClass> getAssetValueClassesList( );

  public List<IDamageFunction> getDamageFunctionsList( );

  public List<IAdministrationUnit> getAdministrationUnits( );

  public ILanduseClass createNewLanduseClass( );

  public IDamageFunction createNewDamageFunction( );

  public List<IRiskZoneDefinition> getRiskZoneDefinitionsList( );

  /**
   * Created new asset value class with the given values.
   * 
   * @param landuseClassGmlID
   * @param administrationUnitGmlID
   * @param value
   * @param string
   * @return new IAssetValueClass, or <code>null</code> if landuse class or administration unit with the given ID does
   *         not exists
   */
  public IAssetValueClass createNewAssetValueClass( final String landuseClassGmlID, final String administrationUnitGmlID, final Double value, final String description );

  public IAssetValueClass getAssetValueClass( final String landuseClassGmlID, final String administrationUnitGmlID, final boolean createIfNotExists );

  public boolean containsLanduseClass( final String landuseClassName );

  public int getNextAvailableLanduseClassOrdinalNumber( );

  public IAdministrationUnit createNewAdministrationUnit( final String name, final String description );

  /**
   * The list of GmlIDs of the Landuse classes with the given name (because there is no guarantee that landuse name is
   * unique)
   * 
   * @param landuseClassName
   *            The name of landuse class
   * @return The list of landuse classes IDs; if no ID is found, returns empty list
   */
  public List<String> getLanduseClassID( final String landuseClassName );

  public void resetStatistics( );

  public void fixStatisticsForShowingToUser( );

}
