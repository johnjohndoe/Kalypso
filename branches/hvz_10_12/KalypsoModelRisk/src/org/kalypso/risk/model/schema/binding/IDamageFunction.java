package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

public interface IDamageFunction extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "DamageFunction" ); //$NON-NLS-1$

  public QName PROP_NAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "name" ); //$NON-NLS-1$

  public QName PROP_DESCRIPTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "description" ); //$NON-NLS-1$

  public QName PROP_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "function" ); //$NON-NLS-1$

  public String getFunction( );

  @Override
  public void setName( final String name );

  @Override
  public String getName( );

  public void setFunction( final String function );

  @Override
  public void setDescription( final String description );

  @Override
  public String getDescription( );

}
