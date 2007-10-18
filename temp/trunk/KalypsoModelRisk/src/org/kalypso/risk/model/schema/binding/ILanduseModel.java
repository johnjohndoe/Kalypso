package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface ILanduseModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "LanduseModel" );

  public QName PROPERTY_LANDUSE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_VECTOR_MODEL, "landusePolygonCollection" );

  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( );
}
