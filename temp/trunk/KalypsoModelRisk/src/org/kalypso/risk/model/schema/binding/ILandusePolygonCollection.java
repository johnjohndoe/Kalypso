package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface ILandusePolygonCollection extends IFeatureWrapperCollection<ILandusePolygon>
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "PolygonCollection" );

  public QName PROPERTY_POLYGON_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "polygonMember" );

  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( );
}
