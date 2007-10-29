package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public interface ILanduseCoverageModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_COVERAGE_MODEL, "LanduseCoverageModel" );

  public QName PROPERTY_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_LANDUSE_COVERAGE_MODEL, "coveragesMember" );

  public ICoverageCollection getCoverageCollection( );
  
}
