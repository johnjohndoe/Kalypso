package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

public interface IDamageFunction extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "DamageFunction" );

  public QName PROP_NAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "name" );

  public QName PROP_DESCRIPTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "description" );

  public QName PROP_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "function" );

  public String getFunction( );

  public void setName( final String name );

  public String getName( );

  public void setFunction( final String function );

  public void setDescription( final String description );

  public String getDescription( );

}
