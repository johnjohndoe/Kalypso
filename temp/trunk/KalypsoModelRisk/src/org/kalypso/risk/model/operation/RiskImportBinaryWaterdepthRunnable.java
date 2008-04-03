package org.kalypso.risk.model.operation;

import java.util.ArrayList;
import java.util.List;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.actions.dataImport.waterdepth.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

/**
 * Difference RiskImportWaterdepthRunnable: Ascii raster files are already converted to bin files
 * 
 * @author Thomas Jung
 * @author Dirk Kuch
 */
public final class RiskImportBinaryWaterdepthRunnable implements ICoreRunnableWithProgress
{

  private static final String BIN_EXTENSION = "bin"; //$NON-NLS-1$

  private final IRasterDataModel m_rasterDataModel;

  private final AsciiRasterInfo[] m_rasters;

  public RiskImportBinaryWaterdepthRunnable( final IRasterDataModel rasterDataModel, final AsciiRasterInfo[] rasters )
  {
    m_rasterDataModel = rasterDataModel;
    m_rasters = rasters;
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$

      final GMLWorkspace workspace = m_rasterDataModel.getFeature().getWorkspace();
      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = m_rasterDataModel.getWaterlevelCoverageCollection();

      for( final AsciiRasterInfo raster : m_rasters )
      {
        monitor.subTask( " HQ " + raster.getReturnPeriod() ); //$NON-NLS-1$

        // range set
        // @hack: append .bin to path to get the binary ascii grid file
        final String path = raster.getSourceFile().getAbsolutePath() + "." + BIN_EXTENSION; //$NON-NLS-1$
        final String binFileName = raster.getSourceFile().getName() + "." + BIN_EXTENSION; //$NON-NLS-1$

        final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
        rangeSetFile.setFileName( path ); //$NON-NLS-1$
        rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$

        final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
        rangeSet.setFile( rangeSetFile );

        // remove existing (invalid) coverage collections from the model
        final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();

        for( final IAnnualCoverageCollection existingAnnualCoverage : waterdepthCoverageCollection )
          if( existingAnnualCoverage.getReturnPeriod() == raster.getReturnPeriod() )
            coveragesToRemove.add( existingAnnualCoverage );

        for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
          waterdepthCoverageCollection.remove( coverageToRemove );

        final IAnnualCoverageCollection annualCoverageCollection = waterdepthCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "HQ " + raster.getReturnPeriod() );
        annualCoverageCollection.setReturnPeriod( raster.getReturnPeriod() );
        final IFeatureType rgcFeatureType = workspace.getGMLSchema().getFeatureType( RectifiedGridCoverage.QNAME );
        final IRelationType parentRelation = (IRelationType) annualCoverageCollection.getFeature().getFeatureType().getProperty( IAnnualCoverageCollection.PROP_COVERAGE );
        final Feature coverageFeature = workspace.createFeature( annualCoverageCollection.getFeature(), parentRelation, rgcFeatureType );

        final RectifiedGridCoverage coverage = new RectifiedGridCoverage( coverageFeature );
        coverage.setRangeSet( rangeSet );
        coverage.setGridDomain( raster.getGridDomain() );
        coverage.setName( binFileName );
        coverage.setDescription( "Imported from " + raster.getSourceFile().getName() );

        annualCoverageCollection.add( coverage );
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Fliesstiefen-Import fehlgeschlagen" );
    }
  }
}