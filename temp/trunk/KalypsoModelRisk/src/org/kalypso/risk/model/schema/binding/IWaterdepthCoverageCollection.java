package org.kalypso.risk.model.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public interface IWaterdepthCoverageCollection extends IFeatureWrapperCollection<IWaterdepthCoverage>
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "WaterdepthCoverageCollection" );

  public QName PROPERTY_WATERDEPTH_COVERAGE_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_WATERDEPTH_COVERAGE_MODEL, "waterdepthCoverageMember" );

  public IFeatureWrapperCollection<IWaterdepthCoverage> getWaterdepthCoverageCollection( );
}
