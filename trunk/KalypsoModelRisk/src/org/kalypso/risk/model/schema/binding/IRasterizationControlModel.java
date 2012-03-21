package org.kalypso.risk.model.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IModel;

public interface IRasterizationControlModel extends IModel
{
  String MODEL_NAME = "RasterizationControlModel"; //$NON-NLS-1$

  String MODEL_ID = "org.kalypso.risk.model.schema.binding.IRasterizationControlModel"; //$NON-NLS-1$

  String MODEL_FILENAME_GML = "RasterizationControlModel.gml"; //$NON-NLS-1$

  double RISKZONE_URBANAREA_LOW = 1.0;

  double RISKZONE_URBANAREA_MIDDLE = 2.0;

  double RISKZONE_URBANAREA_HIGH = 3.0;

  double RISKZONE_NONURBANAREA_LOW = 4.0;

  double RISKZONE_NONURBANAREA_MIDDLE = 5.0;

  double RISKZONE_NONURBANAREA_HIGH = 6.0;

  QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "RasterizationControlModel" ); //$NON-NLS-1$

  QName PROPERTY_RISK_CALCULATION_TYPE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskCalculationType" ); //$NON-NLS-1$

  QName PROPERTY_LANDUSE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" ); //$NON-NLS-1$

  QName PROPERTY_ASSET_VALUE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "assetValueClassMember" ); //$NON-NLS-1$

  QName PROPERTY_DAMAGE_FUNCTION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionMember" ); //$NON-NLS-1$

  QName PROPERTY_RISKZONE_DEFINITION_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskZoneDefinitionMember" ); //$NON-NLS-1$

  QName PROPERTY_STATISTIC_OBS = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "riskStatisticResultMember" ); //$NON-NLS-1$

  List<ILanduseClass> getLanduseClassesList( );

  List<IAssetValueClass> getAssetValueClassesList( );

  List<IDamageFunction> getDamageFunctionsList( );

  ILanduseClass createNewLanduseClass( );

  IDamageFunction createNewDamageFunction( );

  List<IRiskZoneDefinition> getRiskZoneDefinitionsList( );

  /**
   * Creates a new asset value class with the specified values.
   */
  IAssetValueClass createNewAssetValueClass( );

  IAssetValueClass getAssetValueClass( Double value, String name, String description );

  boolean containsLanduseClass( String landuseClassName );

  int getNextAvailableLanduseClassOrdinalNumber( );

  /**
   * Returns the best-match IDamageFunction based on name similarity with the given string (small Levenshtein distance or substring), or null if cannot decide
   */
  IDamageFunction getSuggestedDamageFunction( String name );

  /**
   * Returns the best-match IAssetValueClass based on name similarity with the given string (small Levenshtein distance or substring), or null if cannot decide
   */
  IAssetValueClass getSuggestedAssetValueClass( String name );

  IDamageFunction getDamageFunction( String name, String value, String description );

  IObservation<TupleResult> getStatisticObs( );

  Feature getStatisticObsFeature( );
}
