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
  public static final QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "LanduseClass" ); //$NON-NLS-1$

  public static final QName PROP_NAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "name" ); //$NON-NLS-1$

  public static final QName PROP_DESCRIPTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "description" ); //$NON-NLS-1$

  public static final QName PROP_ORDINAL_NUMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "ordinalNumber" ); //$NON-NLS-1$

  public static final QName PROP_COLOR_STYLE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "colorStyle" ); //$NON-NLS-1$

  public static final QName PROP_DAMAGE_FUNCTION_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionLink" ); //$NON-NLS-1$

  public static final QName PROP_ASSET_VALUE_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "AssetValueClassLink" ); //$NON-NLS-1$

  public static final QName PROP_RISK_LANDUSE_CATEGORY_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseRiskTypeLink" ); //$NON-NLS-1$

  public static final QName PROP_MIN_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "minDamage" ); //$NON-NLS-1$

  public static final QName PROP_MAX_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "maxDamage" ); //$NON-NLS-1$

  public static final QName PROP_TOTAL_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "totalDamage" ); //$NON-NLS-1$

  public static final QName PROP_ANNUAL_AVERAGE_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "averageAnnualDamage" ); //$NON-NLS-1$

  public static final QName PROP_DAMAGE_STATISTIC_LIST = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statisticClassMember" ); //$NON-NLS-1$

  @Override
  public void setName( final String name );

  @Override
  public void setDescription( final String description );

  public void setOrdinalNumber( final int value );

  public void setMinAnnualDamage( final double value );

  public void setMaxAnnualDamage( final double value );

  public void setAverageAnnualDamage( final double value );

  public void setTotalDamage( final double value );

  public void setAssetValue( final IAssetValueClass assetValueClass );

  public IAssetValueClass getAssetValue( );

  public double getMinAnnualDamage( );

  public double getMaxAnnualDamage( );

  public double getAverageAnnualDamage( );

  public double getTotalDamage( );

  public String getDamageFunctionGmlID( );

  public void setDamageFunction( final IDamageFunction damageFunction );

  @Override
  public String getDescription( );

  @Override
  public String getName( );

  public void updateStatistic( final int returnPeriod );

  public IRiskLanduseStatistic getStatistic( final int returnPeriod );

  public IRiskLanduseStatistic createNewStatisticEntry( );

  public void clearStatisticEntries( );

  public boolean containsStatisticEntry( final int returnPeriod );

  public List<IRiskLanduseStatistic> getLanduseStatisticList( );

  public void updateStatisticsAverageAnnualDamage( final double value );

  public void setCellSize( final double size );

  public double getCellSize( );

}
