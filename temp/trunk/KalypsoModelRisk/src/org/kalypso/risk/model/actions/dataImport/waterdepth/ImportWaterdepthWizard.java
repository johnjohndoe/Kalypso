/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.ConvertAscii2Binary;
import org.kalypso.grid.GeoGridException;
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWaterdepthWizard extends Wizard implements INewWizard
{
  protected ImportWaterdepthPage m_page;

  public ImportWaterdepthWizard( )
  {
    super();
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "ImportWaterdepthWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportWaterdepthPage();
    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_page.isPageComplete();
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "ImportWaterdepthWizard.8" ) ); //$NON-NLS-1$
      return false;
    }
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final CascadingKalypsoTheme parentKalypsoTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, "Wassertiefen" ); //$NON-NLS-1$
    parentKalypsoTheme.setVisible( true );
    try
    {
      final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class );
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InterruptedException
        {
          monitor.beginTask( Messages.getString( "ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          try
          {
            monitor.subTask( Messages.getString( "ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$
            final List<AsciiRasterInfo> rasterInfos = m_page.getRasterInfos();
            final IRasterDataModel rasterDataModel = scenarioDataProvider.getModel( IRasterDataModel.class );
            final IFeatureWrapperCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = rasterDataModel.getWaterlevelCoverageCollection();
            for( final AsciiRasterInfo asciiRasterInfo : rasterInfos )
            {
              monitor.subTask( " HQ " + asciiRasterInfo.getReturnPeriod() ); //$NON-NLS-1$
              final String binFileName = asciiRasterInfo.getSourceFile().getName() + ".bin"; //$NON-NLS-1$
              final String dstFileName = "models/raster/input/" + binFileName; //$NON-NLS-1$
              final IFile dstRasterIFile = scenarioFolder.getFile( dstFileName );
              final File dstRasterFile = dstRasterIFile.getRawLocation().toFile();
              importAsBinaryRaster( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );
              // copy( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );
              final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
              rangeSetFile.setFileName( "raster/input/" + binFileName ); //$NON-NLS-1$
              rangeSetFile.setMimeType( "image/bin" ); //$NON-NLS-1$
              final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
              rangeSet.setFile( rangeSetFile );

              // remove existing (invalid) coverages from the model
              final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();
              for( final IAnnualCoverageCollection existingAnnualCoverage : waterdepthCoverageCollection )
                if( existingAnnualCoverage.getReturnPeriod() == asciiRasterInfo.getReturnPeriod() )
                  coveragesToRemove.add( existingAnnualCoverage );
              for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
                waterdepthCoverageCollection.remove( coverageToRemove );

              final IAnnualCoverageCollection annualCoverageCollection = waterdepthCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
              annualCoverageCollection.setName( "HQ " + asciiRasterInfo.getReturnPeriod() ); //$NON-NLS-1$
              annualCoverageCollection.setReturnPeriod( asciiRasterInfo.getReturnPeriod() );
              final IFeatureType rgcFeatureType = workspace.getGMLSchema().getFeatureType( RectifiedGridCoverage.QNAME );
              final IRelationType parentRelation = (IRelationType) annualCoverageCollection.getFeature().getFeatureType().getProperty( IAnnualCoverageCollection.PROP_COVERAGE );
              final Feature coverageFeature = workspace.createFeature( annualCoverageCollection.getFeature(), parentRelation, rgcFeatureType );
              final RectifiedGridCoverage coverage = new RectifiedGridCoverage( coverageFeature );
              annualCoverageCollection.add( coverage );
              coverage.setRangeSet( rangeSet );
              coverage.setGridDomain( asciiRasterInfo.getGridDomain() );
              coverage.setName( binFileName );
              coverage.setDescription( "Imported from " + asciiRasterInfo.getSourceFile().getName() ); //$NON-NLS-1$

              // remove themes that are showing invalid coverages
              final String layerName = "HQ " + asciiRasterInfo.getReturnPeriod(); //$NON-NLS-1$
              final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
              final List<IKalypsoTheme> themesToRemove = new ArrayList<IKalypsoTheme>();
              for( int i = 0; i < childThemes.length; i++ )
                if( childThemes[i].getName().equals( layerName ) )
                  themesToRemove.add( childThemes[i] );
              for( final IKalypsoTheme themeToRemove : themesToRemove )
                parentKalypsoTheme.removeTheme( themeToRemove );

              final StyledLayerType layer = new StyledLayerType();
              layer.setName( layerName );
              layer.setFeaturePath( "#fid#" + annualCoverageCollection.getGmlID() + "/coverageMember" ); //$NON-NLS-1$ //$NON-NLS-2$
              layer.setLinktype( "gml" ); //$NON-NLS-1$
              layer.setType( "simple" ); //$NON-NLS-1$
              layer.setVisible( true );
              layer.setActuate( "onRequest" ); //$NON-NLS-1$
              layer.setHref( "project:/" + scenarioFolder.getProjectRelativePath() + "/models/RasterDataModel.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
              layer.setVisible( true );
              final Property layerPropertyDeletable = new Property();
              layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
              layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$
              final Property layerPropertyThemeInfoId = new Property();
              layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
              layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Wassertiefe %.2f m" ); //$NON-NLS-1$
              final List<Property> layerPropertyList = layer.getProperty();
              layerPropertyList.add( layerPropertyDeletable );
              layerPropertyList.add( layerPropertyThemeInfoId );
              final List<Style> styleList = layer.getStyle();
              final Style style = new Style();
              style.setLinktype( "sld" ); //$NON-NLS-1$
              style.setStyle( "Kalypso style" ); //$NON-NLS-1$
              style.setActuate( "onRequest" ); //$NON-NLS-1$
              style.setHref( "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
              style.setType( "simple" ); //$NON-NLS-1$
              styleList.add( style );

              parentKalypsoTheme.addLayer( layer );

              // fireModellEvent to redraw a map...
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, waterdepthCoverageCollection.getFeature(), new Feature[] { annualCoverageCollection.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            }
            scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

            // Undoing this operation is not possible because old raster files are deleted...
            scenarioDataProvider.saveModel( new NullProgressMonitor() );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
            throw new InterruptedException( e.getLocalizedMessage() );
          }
        }
      } );
    }
    catch( final Exception e )
    {
      ErrorDialog.openError( getShell(), Messages.getString( "ImportWaterdepthWizard.6" ), "", StatusUtilities.statusFromThrowable( e ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private void importAsBinaryRaster( final File srcFile, final File dstFile, final IProgressMonitor monitor ) throws IOException, CoreException, GeoGridException
  {
    final ConvertAscii2Binary ascii2Binary = new ConvertAscii2Binary( srcFile.toURL(), dstFile, 2 );
    ascii2Binary.doConvert( monitor );
  }

}
