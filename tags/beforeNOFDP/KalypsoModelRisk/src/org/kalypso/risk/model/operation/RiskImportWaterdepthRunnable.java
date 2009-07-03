package org.kalypso.risk.model.operation;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
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
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

/**
 * @author Thomas Jung
 * 
 */
public final class RiskImportWaterdepthRunnable implements ICoreRunnableWithProgress
{

  private final IFolder m_scenarioFolder;

  private final List<AsciiRasterInfo> m_rasterInfos;

  private final IRasterDataModel m_rasterDataModel;

  public RiskImportWaterdepthRunnable( final IRasterDataModel rasterDataModel, final List<AsciiRasterInfo> rasterInfos, final IFolder scenarioFolder )
  {
    m_rasterDataModel = rasterDataModel;
    m_rasterInfos = rasterInfos;
    m_scenarioFolder = scenarioFolder;
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$

      final GMLWorkspace workspace = m_rasterDataModel.getFeature().getWorkspace();

      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = m_rasterDataModel.getWaterlevelCoverageCollection();
      for( final AsciiRasterInfo asciiRasterInfo : m_rasterInfos )
      {
        monitor.subTask( " HQ " + asciiRasterInfo.getReturnPeriod() ); //$NON-NLS-1$

        final String binFileName = asciiRasterInfo.getSourceFile().getName() + ".bin"; //$NON-NLS-1$
        final String dstFileName = "models/raster/input/" + binFileName; //$NON-NLS-1$
        final IFile dstRasterIFile = m_scenarioFolder.getFile( dstFileName );
        final File dstRasterFile = dstRasterIFile.getRawLocation().toFile();

        // TODO Thomas: nofdpIDSS already have converted all .asc-files to .bin(ary) files
        RiskModelHelper.importAsBinaryRaster( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );

        // copy( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );
        final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
        rangeSetFile.setFileName( "raster/input/" + binFileName ); //$NON-NLS-1$
        rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$
        final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
        rangeSet.setFile( rangeSetFile );

        // remove existing (invalid) coverage collections from the model
        final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();
        for( final IAnnualCoverageCollection existingAnnualCoverage : waterdepthCoverageCollection )
          if( existingAnnualCoverage.getReturnPeriod() == asciiRasterInfo.getReturnPeriod() )
            coveragesToRemove.add( existingAnnualCoverage );
        for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
          waterdepthCoverageCollection.remove( coverageToRemove );

        final IAnnualCoverageCollection annualCoverageCollection = waterdepthCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "HQ " + asciiRasterInfo.getReturnPeriod() );
        annualCoverageCollection.setReturnPeriod( asciiRasterInfo.getReturnPeriod() );
        final IFeatureType rgcFeatureType = workspace.getGMLSchema().getFeatureType( RectifiedGridCoverage.QNAME );
        final IRelationType parentRelation = (IRelationType) annualCoverageCollection.getFeature().getFeatureType().getProperty( IAnnualCoverageCollection.PROP_COVERAGE );
        final Feature coverageFeature = workspace.createFeature( annualCoverageCollection.getFeature(), parentRelation, rgcFeatureType );
        final RectifiedGridCoverage coverage = new RectifiedGridCoverage( coverageFeature );
        annualCoverageCollection.add( coverage );
        coverage.setRangeSet( rangeSet );
        coverage.setGridDomain( asciiRasterInfo.getGridDomain() );
        coverage.setName( binFileName );
        coverage.setDescription( "Imported from " + asciiRasterInfo.getSourceFile().getName() );
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