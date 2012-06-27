package org.kalypso.risk.model.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

/**
 * @author Dejan Antanaskovic
 */
public interface ILanduseClass extends IColorStyledFeatureWrapper
{
  static QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "LanduseClass" ); //$NON-NLS-1$

  static QName PROP_NAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "name" ); //$NON-NLS-1$

  static QName PROP_DESCRIPTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "description" ); //$NON-NLS-1$

  static QName PROP_ORDINAL_NUMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "ordinalNumber" ); //$NON-NLS-1$

  static QName PROP_COLOR_STYLE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "colorStyle" ); //$NON-NLS-1$

  static QName PROP_DAMAGE_FUNCTION_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionLink" ); //$NON-NLS-1$

  static QName PROP_ASSET_VALUE_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "AssetValueClassLink" ); //$NON-NLS-1$

  static QName PROP_RISK_LANDUSE_CATEGORY_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseRiskTypeLink" ); //$NON-NLS-1$

  static QName PROP_MIN_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "minDamage" ); //$NON-NLS-1$

  static QName PROP_MAX_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "maxDamage" ); //$NON-NLS-1$

  static QName PROP_TOTAL_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "totalDamage" ); //$NON-NLS-1$

  static QName PROP_ANNUAL_AVERAGE_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "averageAnnualDamage" ); //$NON-NLS-1$

  static QName PROP_DAMAGE_STATISTIC_LIST = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statisticClassMember" ); //$NON-NLS-1$

  void setOrdinalNumber( int value );

  void setAverageAnnualDamage( double value );

  void setTotalDamage( double value );

  void setAssetValue( IAssetValueClass assetValueClass );

  IAssetValueClass getAssetValue( );

  double getAverageAnnualDamage( );

  String getDamageFunctionGmlID( );

  void setDamageFunction( IDamageFunction damageFunction );

  void updateStatistic( int returnPeriod );

  IRiskLanduseStatistic getStatistic( int returnPeriod );

  IRiskLanduseStatistic createNewStatisticEntry( );

  void clearStatisticEntries( );

  boolean containsStatisticEntry( int returnPeriod );

  List<IRiskLanduseStatistic> getLanduseStatisticList( );
}
