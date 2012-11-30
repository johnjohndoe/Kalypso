package org.kalypso.risk.model.operation;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.ConvertAscii2Binary;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.coverage.RangeSetFile;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

/**
 * @author Thomas Jung
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

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$

      final GMLWorkspace workspace = m_rasterDataModel.getWorkspace();

      final IFeatureBindingCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = m_rasterDataModel.getWaterlevelCoverageCollection();
      for( final AsciiRasterInfo asciiRasterInfo : m_rasterInfos )
      {
        monitor.subTask( " HQ " + asciiRasterInfo.getReturnPeriod() ); //$NON-NLS-1$

        final String binFileName = asciiRasterInfo.getSourceFile().getName() + ".bin"; //$NON-NLS-1$
        final String dstFileName = "models/raster/input/" + binFileName; //$NON-NLS-1$
        final IFile dstRasterIFile = m_scenarioFolder.getFile( dstFileName );
        final File dstRasterFile = dstRasterIFile.getRawLocation().toFile();

        final ConvertAscii2Binary ascii2Binary = new ConvertAscii2Binary( asciiRasterInfo.getSourceFile().toURI().toURL(), dstRasterFile, 2, asciiRasterInfo.getCoordinateSystem() );
        ascii2Binary.doConvert( monitor );

        // copy( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );
        final RangeSetFile rangeSetFile = new RangeSetFile( "raster/input/" + binFileName ); //$NON-NLS-1$
        rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$

        // remove existing (invalid) coverage collections from the model
        final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<>();
        for( final IAnnualCoverageCollection existingAnnualCoverage : waterdepthCoverageCollection )
          if( existingAnnualCoverage.getReturnPeriod() == asciiRasterInfo.getReturnPeriod() )
            coveragesToRemove.add( existingAnnualCoverage );
        for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
          waterdepthCoverageCollection.remove( coverageToRemove );

        final IAnnualCoverageCollection annualCoverageCollection = waterdepthCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( Messages.getString( "org.kalypso.risk.model.operation.RiskImportWaterdepthRunnable.0" ) + asciiRasterInfo.getReturnPeriod() ); //$NON-NLS-1$
        annualCoverageCollection.setReturnPeriod( asciiRasterInfo.getReturnPeriod() );
        final IFeatureType rgcFeatureType = GMLSchemaUtilities.getFeatureTypeQuiet( RectifiedGridCoverage.QNAME );
        final IRelationType parentRelation = (IRelationType) annualCoverageCollection.getFeatureType().getProperty( IAnnualCoverageCollection.PROP_COVERAGE );
        final Feature coverageFeature = workspace.createFeature( annualCoverageCollection, parentRelation, rgcFeatureType );
        final RectifiedGridCoverage coverage = (RectifiedGridCoverage) coverageFeature;
        annualCoverageCollection.getCoverages().add( coverage );
        coverage.setRangeSet( rangeSetFile );
        coverage.setGridDomain( ascii2Binary.getGridDomain() );
        coverage.setName( binFileName );
        coverage.setDescription( Messages.getString( "org.kalypso.risk.model.operation.RiskImportWaterdepthRunnable.1" ) + asciiRasterInfo.getSourceFile().getName() ); //$NON-NLS-1$

        dstRasterIFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.operation.RiskImportWaterdepthRunnable.2" ) ); //$NON-NLS-1$
    }
  }
}