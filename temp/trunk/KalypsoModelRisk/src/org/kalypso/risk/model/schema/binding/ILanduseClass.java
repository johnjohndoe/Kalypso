package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;

/**
 * @author Dejan Antanaskovic
 */
public interface ILanduseClass extends IColorStyledFeatureWrapper
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "LanduseClass" );

  public QName PROP_ORDINAL_NUMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "ordinalNumber" );

  public QName PROP_COLOR_STYLE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "colorStyle" );

  public QName PROP_DAMAGE_FUNCTION_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionLink" );

  public QName PROP_RISK_LANDUSE_CATEGORY_LINK = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseRiskTypeLink" );

  public QName PROP_MIN_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "minDamage" );

  public QName PROP_MAX_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "maxDamage" );

  public QName PROP_TOTAL_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "totalDamage" );

  void setColorStyle( final RGB rgb );

  void setOrdinalNumber( final int value );

  public void setMinDamage( final double value );

  public void setMaxDamage( final double value );

  public void setTotalDamage( final double value );

  public double getMinDamage( );

  public double getMaxDamage( );

  public double getTotalDamage( );

  public String getDamageFunctionGmlID( );

  public void setDamageFunction( final IDamageFunction damageFunction );
}
