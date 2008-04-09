package org.kalypso.risk.model.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;

public interface IRasterizationControlModel extends IModel
{
  public static final String MODEL_NAME = "RasterizationControlModel.gml"; //$NON-NLS-1$

  public static final double RISKZONE_URBANAREA_LOW = 1.0;

  public static final double RISKZONE_URBANAREA_MIDDLE = 2.0;

  public static final double RISKZONE_URBANAREA_HIGH = 3.0;

  public static final double RISKZONE_NONURBANAREA_LOW = 4.0;

  public static final double RISKZONE_NONURBANAREA_MIDDLE = 5.0;

  public static final double RISKZONE_NONURBANAREA_HIGH = 6.0;

  public static final QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RasterizationControlModel" ); //$NON-NLS-1$

  public static final QName PROPERTY_LANDUSE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_ASSET_VALUE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "assetValueClassMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_DAMAGE_FUNCTION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_RISKZONE_DEFINITION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskZoneDefinitionMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_STATISTIC_OBS = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskStatisticResultMember" ); //$NON-NLS-1$

  public List<ILanduseClass> getLanduseClassesList( );

  public List<IAssetValueClass> getAssetValueClassesList( );

  public List<IDamageFunction> getDamageFunctionsList( );

  public ILanduseClass createNewLanduseClass( );

  public IDamageFunction createNewDamageFunction( );

  public List<IRiskZoneDefinition> getRiskZoneDefinitionsList( );

  /**
   * Creates a new asset value class with the specified values.
   * 
   */
  public IAssetValueClass createNewAssetValueClass( );

  public IAssetValueClass getAssetValueClass( final Double value, final String name, final String description );

  public boolean containsLanduseClass( final String landuseClassName );

  public int getNextAvailableLanduseClassOrdinalNumber( );

  /**
   * The list of GmlIDs of the landuse classes with the given name (because there is no guarantee that landuse name is
   * unique)
   * 
   * @param landuseClassName
   *            The name of landuse class
   * @return The list of landuse classes IDs; if no ID is found, returns empty list
   */
  public List<String> getLanduseClassID( final String landuseClassName );

  public void resetStatistics( );

  public void fixStatisticsForShowingToUser( );

  public IDamageFunction getDamageFunction( final String name, final String value, final String description );

  public IObservation<TupleResult> getStatisticObs( );

  public Feature getStatisticObsFeature( );

}
