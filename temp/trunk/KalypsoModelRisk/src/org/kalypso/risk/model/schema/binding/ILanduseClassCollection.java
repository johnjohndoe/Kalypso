package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface ILanduseClassCollection extends IFeatureWrapperCollection<ILanduseClass>
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "LanduseClassCollection" );

  public QName PROPERTY_LANDUSE_CLASS_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" );
}
