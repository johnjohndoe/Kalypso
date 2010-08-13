package org.kalypso.risk.model.operation;

import java.util.ArrayList;
import java.util.List;

import ogc31.www.opengis.net.gml.FileType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
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

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$

      final GMLWorkspace workspace = m_rasterDataModel.getFeature().getWorkspace();
      final IFeatureBindingCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = m_rasterDataModel.getWaterlevelCoverageCollection();

      for( final AsciiRasterInfo raster : m_rasters )
      {
        monitor.subTask( " HQ " + raster.getReturnPeriod() ); //$NON-NLS-1$

        // range set
        // @hack: append .bin to path to get the binary ascii grid file
        final IFile iFile = raster.getiSourceFile();

        final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();

        final String binFileName = "platform:/resource//" + iFile.getProject().getName() + "/" + iFile.getProjectRelativePath().toString() + "." + BIN_EXTENSION; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        rangeSetFile.setFileName( binFileName );
        rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$

        // remove existing (invalid) coverage collections from the model
        final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();

        for( final IAnnualCoverageCollection existingAnnualCoverage : waterdepthCoverageCollection )
          if( existingAnnualCoverage.getReturnPeriod() == raster.getReturnPeriod() )
            coveragesToRemove.add( existingAnnualCoverage );

        for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
          waterdepthCoverageCollection.remove( coverageToRemove );

        final IAnnualCoverageCollection annualCoverageCollection = waterdepthCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "HQ " + raster.getReturnPeriod() ); //$NON-NLS-1$
        annualCoverageCollection.setReturnPeriod( raster.getReturnPeriod() );
        final IFeatureType rgcFeatureType = workspace.getGMLSchema().getFeatureType( RectifiedGridCoverage.QNAME );
        final IRelationType parentRelation = (IRelationType) annualCoverageCollection.getFeatureType().getProperty( IAnnualCoverageCollection.PROP_COVERAGE );
        final Feature coverageFeature = workspace.createFeature( annualCoverageCollection, parentRelation, rgcFeatureType );

        final RectifiedGridCoverage coverage = (RectifiedGridCoverage) coverageFeature;
        coverage.setRangeSet( rangeSetFile );
        coverage.setGridDomain( raster.getGridDomain() );
        coverage.setName( binFileName );
        coverage.setDescription( Messages.getString( "org.kalypso.risk.model.operation.RiskImportBinaryWaterdepthRunnable.4" ) + raster.getSourceFile().getName() ); //$NON-NLS-1$

        annualCoverageCollection.getCoverages().add( coverage );
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.operation.RiskImportBinaryWaterdepthRunnable.5" ) ); //$NON-NLS-1$
    }
  }

  /** REMARK: copies only files - not foldes or recursive through folders */
  public void copy( final IFolder srcFolder, final IFolder destFolder, final String fileNamePart ) throws CoreException
  {
    srcFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    destFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );

    if( !srcFolder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.risk.model.operation.RiskImportBinaryWaterdepthRunnable.6" ) ) ); //$NON-NLS-1$
    if( !destFolder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.risk.model.operation.RiskImportBinaryWaterdepthRunnable.7" ) ) ); //$NON-NLS-1$

    srcFolder.accept( new IResourceVisitor()
    {
      @Override
      public boolean visit( final IResource resource ) throws CoreException
      {
        if( resource instanceof IFile )
        {
          if( resource.getName().contains( fileNamePart ) )
            resource.copy( destFolder.getFullPath().append( resource.getName() ), true, new NullProgressMonitor() );
        }

        return true;
      }
    } );
  }
}