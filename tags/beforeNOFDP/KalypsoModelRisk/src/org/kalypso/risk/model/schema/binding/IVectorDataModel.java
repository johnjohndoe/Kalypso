package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface IVectorDataModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "VectorDataModel" );

  public QName PROPERTY_LANDUSE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "landusePolygonCollection" );

  public QName PROPERTY_ADMINISTRATION_UNIT_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "administrationUnitsPolygonCollection" );

  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( );
}
