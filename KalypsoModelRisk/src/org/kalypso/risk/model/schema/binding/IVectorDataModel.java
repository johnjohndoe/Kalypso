package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface IVectorDataModel extends IModel
{
  public static final String MODEL_NAME = "VectorDataModel"; //$NON-NLS-1$

  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "VectorDataModel" ); //$NON-NLS-1$

  public QName PROPERTY_LANDUSE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "landusePolygonCollection" ); //$NON-NLS-1$

  public QName PROPERTY_ADMINISTRATION_UNIT_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "administrationUnitsPolygonCollection" ); //$NON-NLS-1$

  public IFeatureWrapperCollection<ILandusePolygon> getLandusePolygonCollection( );
}
