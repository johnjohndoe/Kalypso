package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.RiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface ILandusePolygonCollection extends IFeatureWrapperCollection<ILandusePolygon>
{
  public QName QNAME = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "PolygonCollection" );

  public QName QNAME_PROPERTY_POLYGON = new QName( RiskSchemaCatalog.NS_LANDUSEVECTORMODEL, "polygonMember" );
}
