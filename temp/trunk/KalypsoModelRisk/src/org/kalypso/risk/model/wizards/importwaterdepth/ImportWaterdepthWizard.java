/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.wizards.importwaterdepth;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
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
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

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

  public void init( IWorkbench workbench, IStructuredSelection selection )
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
      StatusUtilities.createWarningStatus( "Kartenansicht nicht geöffnet. Es können keine Themen hinzugefügt werden." );
      return false;
    }
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    try
    {
      final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IWaterdepthCoverageModel.class );
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InterruptedException
        {
          monitor.beginTask( Messages.getString( "ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          try
          {
            monitor.subTask( Messages.getString( "ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$
            final List<AsciiRasterInfo> rasterInfos = m_page.getRasterInfos();
            final IWaterdepthCoverageModel waterdepthCoverageModel = scenarioDataProvider.getModel( IWaterdepthCoverageModel.class );
            final IFeatureWrapperCollection<IWaterdepthCoverage> waterdepthCoverageCollection = waterdepthCoverageModel.getWaterdepthCoverageCollection();
            for( final AsciiRasterInfo asciiRasterInfo : rasterInfos )
            {
              final IWaterdepthCoverage waterdepthCoverage = waterdepthCoverageCollection.addNew( IWaterdepthCoverage.QNAME );

              final String dstFileName = "models/raster/input/" + asciiRasterInfo.getSourceFile().getName();
              final IFile dstRasterFile = scenarioFolder.getFile( dstFileName );
              copy( asciiRasterInfo.getSourceFile(), dstRasterFile, monitor );
              final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
              rangeSetFile.setFileName( "raster/input/" + asciiRasterInfo.getSourceFile().getName() );
              rangeSetFile.setMimeType( "text/asc" );
              final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
              rangeSet.setFile( rangeSetFile );

              waterdepthCoverage.setReturnPeriod( asciiRasterInfo.getReturnPeriod() );
              waterdepthCoverage.setRangeSet( rangeSet );
              waterdepthCoverage.setGridDomain( asciiRasterInfo.getGridDomain() );

              // TODO create map layer
              monitor.subTask( Messages.getString( "ImportWaterdepthWizard.10" ) ); //$NON-NLS-1$

              // final String sourceFileNameWithoutExtension = FileUtilities.nameWithoutExtension(
              // asciiRasterInfo.getSourceFile().getName() );
              // final String sldFileName = scenarioFolder.getRawLocation().toPortableString()+ "/raster/input/" +
              // sourceFileNameWithoutExtension + ".sld"; //$NON-NLS-1$ //$NON-NLS-2$
              // final Color lightBlue = new Color( 150, 150, 255 );
              // final int numOfCategories = 5;
              // createRasterStyle( sldFileName, "Raster style", asciiRasterInfo.getGridCoverage(), lightBlue,
              // numOfCategories );
              // scenarioFolder.refreshLocal( IFolder.DEPTH_INFINITE, new NullProgressMonitor() );
              // final String layerName = asciiRasterInfo.getReturnPeriod() + " year flood";

              final StyledLayerType layer = new StyledLayerType();
              layer.setName( asciiRasterInfo.getReturnPeriod() + " year flood" );
              layer.setFeaturePath( "#fid#" + waterdepthCoverage.getGmlID() );
              layer.setLinktype( "gml" );
              layer.setType( "simple" );
              layer.setActuate( "onRequest" );
              layer.setHref( "project:/" + scenarioFolder.getProjectRelativePath() + "/models/WaterdepthCoverageModel.gml" );
              layer.setVisible( true );
              final List<Style> styleList = layer.getStyle();
              final Style style = new Style();
              style.setLinktype( "sld" );
              style.setStyle( "Raster style" );
              style.setActuate( "onRequest" );
              style.setHref( "../styles/WaterdepthCoverageModel.sld" );
              style.setType( "simple" );
              styleList.add( style );
              mapModell.addTheme( layer );

              // final AddThemeCommand command = new AddThemeCommand( mapModell, asciiRasterInfo.getReturnPeriod() + "
              // year flood", "gml", "#fid#" + waterdepthCoverage.getGmlID(), "project:/"
              // + scenarioFolder.getProjectRelativePath() + "/models/WaterdepthCoverageModel.gml", "sld", "Raster
              // style", "../styles/WaterdepthCoverageModel.sld", "simple" );
              // mapView.postCommand( command, null );

              // fireModellEvent to redraw a map...
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, waterdepthCoverageCollection.getWrappedFeature(), new Feature[] { waterdepthCoverage.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            }
            scenarioDataProvider.postCommand( IWaterdepthCoverageModel.class, new EmptyCommand( "Get dirty!", false ) );
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

  // private void createRasterStyle( final String resultFileName, final String styleName, final RectifiedGridCoverage2
  // grid, Color color, final int numOfCategories ) throws Exception
  // {
  // final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<Double, ColorMapEntry>();
  // final ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999, "no value" );
  // //$NON-NLS-1$
  // colorMap.put( new Double( -9999 ), colorMapEntry_noData );
  //
  // // final DoubleRaster dr = new RectifiedGridCoverageDoubleRaster( grid.getFeature() );
  // // final MinMaxRasterWalker walker = new MinMaxRasterWalker();
  // // dr.walk( walker, null );
  // final double min = 0.0; // We don't use walker.getMin() because min is -9999.0, and we need 0.0 water level as min
  // final double max = 20.0;
  //
  // final NumberFormat format = NumberFormat.getNumberInstance();
  // format.setMaximumFractionDigits( 4 );
  // final double intervalStep = (max - min) / numOfCategories;
  // for( int i = 0; i < numOfCategories; i++ )
  // {
  // final double topBoundary = Double.parseDouble( format.format( min + i * intervalStep ) );
  // final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 0.5, topBoundary, "" ); //$NON-NLS-1$
  // color = color.darker();
  // colorMap.put( topBoundary, colorMapEntry );
  // }
  // color = color.darker();
  // final ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( color, 0.5, max, "" ); //$NON-NLS-1$
  // colorMap.put( new Double( max ), colorMapEntry_max );
  // final RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
  // final Symbolizer[] symbolizers = new Symbolizer[] { rasterSymbolizer };
  // final FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
  // final double minScaleDenominator = 0;
  // final double maxScaleDenominator = 1.8;
  // final Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator,
  // maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  // featureTypeStyle.addRule( rule );
  // final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
  // final org.kalypsodeegree.graphics.sld.Style[] styles = new org.kalypsodeegree.graphics.sld.Style[] { new
  // UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
  // final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] {
  // SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
  // final StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
  //
  // final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
  // final Source source = new DOMSource( doc );
  // OutputStream os = null;
  // try
  // {
  // os = new FileOutputStream( resultFileName );
  // final StreamResult result = new StreamResult( os );
  // final Transformer t = TransformerFactory.newInstance().newTransformer();
  // t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
  // t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
  // t.transform( source, result );
  // }
  // finally
  // {
  // IOUtils.closeQuietly( os );
  // }
  // }

  private void copy( final File src, final IFile dstFileImage, final IProgressMonitor monitor ) throws CoreException, IOException
  {
    InputStream in = null;
    try
    {
      in = new BufferedInputStream( new FileInputStream( src ) );
      if( dstFileImage.exists() )
      {
        dstFileImage.setContents( in, false, true, monitor );
      }
      else
      {
        dstFileImage.create( in, false, monitor );
      }
    }
    finally
    {
      IOUtils.closeQuietly( in );
    }
  }

}
