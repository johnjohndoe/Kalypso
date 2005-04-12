/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

/*
 * Created on 31.01.2005
 *  
 */
package org.kalypso.wizard;

import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.bind.Marshaller;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.util.zip.ZipUtilities;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.w3c.dom.Document;

/**
 * @author N. Peiler
 *  
 */
public class KalypsoFloodRiskProjectWizard extends Wizard implements INewWizard
{

  static final String PROJECT_PAGE = "page_type:createNewProject";

  private WizardNewProjectCreationPage createProjectPage;

  private IPath workspacePath;

  private IProject projectHandel;

  private final String m_resourceBase = "resources/.projecttemplate.zip";

  private SelectLanduseWizardPage selectLanduseWizardPage;

  private SelectWaterlevelWizardPage selectWaterlevelWizardPage;

  private List layerList;

  private org.kalypso.template.gismapview.ObjectFactory mapTemplateOF;

  private org.kalypso.template.types.ObjectFactory typeOF;

  private IProgressMonitor monitor;

  public KalypsoFloodRiskProjectWizard()
  {
    super();
  }

  public void addPages()
  {
    try
    {
      createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
      createProjectPage
          .setDescription( "Dieser Dialog erstellt ein neues Hochwasserrisiko Projekt." );
      createProjectPage.setTitle( "Neues Hochwasserrisiko Projekt" );
      createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( createProjectPage );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    selectLanduseWizardPage = new SelectLanduseWizardPage();
    addPage( selectLanduseWizardPage );

    selectWaterlevelWizardPage = new SelectWaterlevelWizardPage();
    addPage( selectWaterlevelWizardPage );

  }

  /**
   * This method creates the new Project
   * 
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    ProgressMonitorDialog progressMonitor = new ProgressMonitorDialog( this.getShell() );
    progressMonitor.open();
    progressMonitor.setCancelable( true );
    monitor = progressMonitor.getProgressMonitor();
    int totalWork = 100;
    monitor.beginTask( "Erstelle Hochwasserrisiko Projekt...", totalWork );

    workspacePath = createProjectPage.getLocationPath();
    projectHandel = createProjectPage.getProjectHandle();

    try
    {
      projectHandel.create( null );
      projectHandel.open( null );
      //set charSet for the new project to the UTF-8 standard
      projectHandel.setDefaultCharset( "UTF-8", null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    //  copy all the resources to the workspace into the new created project
    copyResourcesToProject( workspacePath.append( projectHandel.getFullPath() ) );
    monitor.worked( 10 );
    if( monitor.isCanceled() )
      performCancle();

    try
    {
      String path = workspacePath.append(
          projectHandel.getFullPath() + "/Waterlevel/Waterlevel.gmt" ).toFile().toString();

      typeOF = new org.kalypso.template.types.ObjectFactory();
      mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();
      Gismapview gismapview = mapTemplateOF.createGismapview();
      LayersType layers = mapTemplateOF.createGismapviewTypeLayersType();
      ExtentType extent = typeOF.createExtentType();
      layerList = layers.getLayer();

      copyLanduseShape();
      Layer landuseLayer = createLanduseLayer( selectLanduseWizardPage.getLanduseDataFile() );
      layerList.add( landuseLayer );
      layers.setActive( landuseLayer );

      GM_Envelope bbox = createDummyLanduseTheme().getBoundingBox();
      extent.setLeft( bbox.getMin().getX() );
      extent.setBottom( bbox.getMin().getY() );
      extent.setRight( bbox.getMax().getX() );
      extent.setTop( bbox.getMax().getY() );

      monitor.worked( 40 );
      if( monitor.isCanceled() )
        performCancle();

      createWaterlevelGrids();

      gismapview.setExtent( extent );
      gismapview.setLayers( layers );

      Marshaller marshaller = mapTemplateOF.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      FileWriter fw = new FileWriter( path );
      marshaller.marshal( gismapview, fw );
      fw.close();
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
    }

    try
    {
      projectHandel.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( CoreException e1 )
    {
      e1.printStackTrace();
    }

    progressMonitor.close();
    monitor.done();

    return true;
  }

  private void copyResourcesToProject( IPath path )
  {
    final String resource = m_resourceBase;
    System.out.print( "resource: " + resource + "\n" );
    InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    try
    {
      ZipUtilities.unzip( resourceAsStream, path.toFile() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( resourceAsStream );

    }
  }

  private void copyLanduseShape() throws IOException
  {
    String landuseSourceBase = FileUtilities.nameWithoutExtension( selectLanduseWizardPage
        .getLanduseDataFile().toString() );
    File targetDir = ( workspacePath.append( projectHandel.getFullPath().append( "/Landuse/" ) ) )
        .toFile();

    File shp = new File( landuseSourceBase + ".shp" );
    FileUtils.copyFileToDirectory( shp, targetDir );

    File dbf = new File( landuseSourceBase + ".dbf" );
    FileUtils.copyFileToDirectory( dbf, targetDir );

    File shx = new File( landuseSourceBase + ".shx" );
    FileUtils.copyFileToDirectory( shx, targetDir );

  }

  private IKalypsoTheme createDummyLanduseTheme() throws GmlSerializeException
  {
    String shapeBase = ( workspacePath.append( projectHandel.getFullPath().append(
        "/Landuse/"
            + FileUtilities.nameWithoutExtension( selectLanduseWizardPage.getLanduseDataFile()
                .getName().toString() ) ) ) ).toString();
    GMLWorkspace shapeWS = ShapeSerializer.deserialize( shapeBase, selectLanduseWizardPage
        .getSelectedCoordinateSystem(), null );
    return new KalypsoFeatureTheme( new CommandableWorkspace( shapeWS ), "featureMember",
        "Landnutzung" );
  }

  private void createWaterlevelGrids() throws Exception
  {
    Vector waterlevelGrids = selectWaterlevelWizardPage.getWaterlevelGrids();
    int workedPart = 50 / waterlevelGrids.size();
    for( int i = 0; i < waterlevelGrids.size(); i++ )
    {
      File sourceFile = (File)waterlevelGrids.get( i );
      RectifiedGridCoverage grid = GridUtils.importGridArc( sourceFile );
      String sourceFileNameWithoutExtension = FileUtilities.nameWithoutExtension( sourceFile
          .getName() );
      File targetFile = ( workspacePath.append( projectHandel.getFullPath().append(
          "/Waterlevel/" + sourceFileNameWithoutExtension + ".gml" ) ) ).toFile();
      GridUtils.writeRasterData( targetFile, grid );
      File sldFile = ( workspacePath.append( projectHandel.getFullPath().append(
          "/.styles/" + sourceFileNameWithoutExtension + ".sld" ) ) ).toFile();
      createRasterStyle( sldFile, sourceFileNameWithoutExtension, grid );
      layerList.add( createWaterlevelLayer( targetFile, sourceFileNameWithoutExtension ) );
      grid = null;
      monitor.worked( workedPart );
      if( monitor.isCanceled() )
        performCancle();
    }
  }

  private void createRasterStyle( File resultFile, String styleName, RectifiedGridCoverage grid )
      throws Exception
  {
    TreeMap colorMap = new TreeMap();
    ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999,
        "Keine Daten" );
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );
    double min = grid.getRangeSet().getMinValue();
    double max = grid.getRangeSet().getMaxValue();
    int numberOfCategories = 5;
    double intervalStep = ( max - min ) / numberOfCategories;
    Color lightBlue = new Color( 150, 150, 255 );
    Color color = lightBlue;
    for( int i = 0; i < numberOfCategories; i++ )
    {
      double quantity = GridUtils.round( ( min + ( i * intervalStep ) ), 4,
          BigDecimal.ROUND_HALF_EVEN );
      ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 1, quantity, "" );
      color = color.darker();
      colorMap.put( new Double( quantity ), colorMapEntry );
    }
    ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( Color.WHITE, 1, max, "" );
    colorMap.put( new Double( max ), colorMapEntry_max );
    RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    Symbolizer[] symbolizers = new Symbolizer[]
    { rasterSymbolizer };
    FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    double minScaleDenominator = 0;
    double maxScaleDenominator = 1.8;
    Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default",
        minScaleDenominator, maxScaleDenominator );
    featureTypeStyle.addRule( rule );
    FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[]
    { featureTypeStyle };
    Style[] styles = new Style[]
    { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" );
    Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld )
        .exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( resultFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );
    t.transform( source, result );
  }

  private org.kalypso.template.gismapview.GismapviewType.LayersType.Layer createLanduseLayer(
      File sourceFile ) throws Exception
  {
    org.kalypso.template.gismapview.GismapviewType.LayersType.Layer newLayer = mapTemplateOF
        .createGismapviewTypeLayersTypeLayer();

    //set attributes for the layer
    newLayer.setName( "Landnutzung" );
    newLayer.setVisible( true );
    newLayer.setFeaturePath( "featureMember" );
    newLayer.setHref( "project:/Landuse/"
        + FileUtilities.nameWithoutExtension( sourceFile.getName() ) + "#"
        + selectLanduseWizardPage.getSelectedCoordinateSystem().getName() );
    newLayer.setType( "simple" );
    newLayer.setLinktype( "shape" );
    newLayer.setActuate( "onRequest" );
    newLayer.setId( "ID_1" );

    List styleList = newLayer.getStyle();
    StyleType style = typeOF.createStyledLayerTypeStyleType();

    //set attributes for the style
    style.setLinktype( "sld" );
    style.setStyle( "Landnutzung" );
    style.setActuate( "onRequest" );
    style.setHref( "../.styles/landuse.sld" );
    style.setType( "simple" );

    //add the style to the layer
    styleList.add( style );
    return newLayer;
  }

  int id = 2;

  private org.kalypso.template.gismapview.GismapviewType.LayersType.Layer createWaterlevelLayer(
      File sourceFile, String styleName ) throws Exception
  {

    org.kalypso.template.gismapview.GismapviewType.LayersType.Layer newLayer = mapTemplateOF
        .createGismapviewTypeLayersTypeLayer();

    //set attributes for the layer
    newLayer.setName( styleName );
    newLayer.setVisible( false );
    newLayer.setFeaturePath( "RectifiedGridCoverageMember" );
    newLayer.setHref( "../Waterlevel/" + sourceFile.getName() );
    newLayer.setType( "simple" );
    newLayer.setLinktype( "gml" );
    newLayer.setActuate( "onRequest" );
    newLayer.setId( "ID_" + id );
    id = id + 1;

    List styleList = newLayer.getStyle();
    StyleType style = typeOF.createStyledLayerTypeStyleType();

    //set attributes for the style
    style.setLinktype( "sld" );
    style.setStyle( styleName );
    style.setActuate( "onRequest" );
    style.setHref( "../.styles/" + styleName + ".sld" );
    style.setType( "simple" );

    //add the style to the layer
    styleList.add( style );

    return newLayer;
  }

  public boolean performCancle()
  {
    try
    {
      projectHandel.delete( true, false, null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  //nothing
  }
}