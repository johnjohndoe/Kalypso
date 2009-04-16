package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface ILandusePolygonCollection extends IFeatureWrapperCollection<ILandusePolygon>
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "PolygonCollection" ); //$NON-NLS-1$

  public QName PROPERTY_POLYGON_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "polygonMember" ); //$NON-NLS-1$

  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( );
}
