package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

public interface IAdministrationUnit extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "AdministrationUnit" ); //$NON-NLS-1$

  @Override
  public void setName( final String name );

  @Override
  public void setDescription( final String description );
}
