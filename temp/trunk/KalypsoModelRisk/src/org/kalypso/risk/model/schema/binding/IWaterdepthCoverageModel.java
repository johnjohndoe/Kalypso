package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface IWaterdepthCoverageModel extends IModel
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "WaterdepthCoverageModel" );

  public QName PROPERTY_WATERDEPTH_COVERAGE_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "waterdepthCoverageCollection" );

  public IFeatureWrapperCollection<IWaterdepthCoverage> getWaterdepthCoverageCollection( );
  
}
