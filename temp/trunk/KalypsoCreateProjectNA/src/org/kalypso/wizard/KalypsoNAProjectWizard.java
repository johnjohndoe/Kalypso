/*
 * Created on 31.01.2005
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.wizard;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.NotImplementedException;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.gml.schema.GMLSchemaCache;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.util.url.UrlResolver;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class KalypsoNAProjectWizard extends Wizard implements INewWizard
{

  //Constants
  static final String CATCHMENT_PAGE = "page_type:catchment";

  static final String HYDROTOP_PAGE = "page_type:hydrotop";

  static final String NODE_PAGE = "page_type:node";

  static final String RIVER_PAGE = "page_type:river";

  static final String PROJECT_PAGE = "page_type:createNewProject";

  static final FeatureType dummyFeatureType = FeatureFactory.createFeatureType( "Gewässer",
      "wizard.kalypso.na", new FeatureTypeProperty[]
      {
          FeatureFactory.createFeatureTypeProperty( "Ort", "wizard.kalypso.na", GM_LineString.class
              .getName(), false, null ),
          FeatureFactory.createFeatureTypeProperty( "name", "wizard.kalypso.na", String.class
              .getName(), false, null ),
          FeatureFactory.createFeatureTypeProperty( "description", "wizard.kalypso.na",
              String.class.getName(), false, null ),
          FeatureFactory.createFeatureTypeProperty( "inum", "wizard.kalypso.na", Integer.class
              .getName(), false, null ),
          FeatureFactory.createFeatureTypeProperty( "StrangArt", "wizard.kalypso.na", Integer.class
              .getName(), false, null ) }, new int[] { 1, 1 }, new int[] { 1,  1 }, null, null );

  private KalypsoNAProjectWizardPage createMappingCatchmentPage;

  private KalypsoNAFileImportPage createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage createMappingNodePage;

  private KalypsoNAProjectWizardPage createMappingRiverPage;

  private WizardNewProjectCreationPage createProjectPage;

  private GMLSchema modelSchema;

  private URL sourceURL;

  private URL modelSchemaURL = getClass().getResource(
      "../../../resources/.model/schema/namodell.xsd" );

  private GMLWorkspace modelWS;

  IPath workspacePath;

  IProject projectHandel;

  IPath modelPath;

  ISelection selection;

  //	IStructuredSelection structSelection;

  /**
   *  
   */
  public KalypsoNAProjectWizard()
  {
    super();
    try
    {
      modelSchema = GMLSchemaCache.getSchema( modelSchemaURL );
      setNeedsProgressMonitor( true );
    }
    catch( MalformedURLException e )
    {
      // TODO Auto-generated catch block
      System.out.print( "Schema URL nicht valide" );
      e.printStackTrace();
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }

  }

  public void addPages()
  {
    createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
    createProjectPage.setDescription( "Dieser Dialog erstellt ein neues NA-Modell Projekt." );
    createProjectPage.setTitle( "Neues NA-Modell Projekt" );
    createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    addPage( createProjectPage );

    createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE,
        "Einzugsgebiet einlesen", ImageProvider.IMAGE_KALYPSO_ICON_BIG,
        getFeatureType( "Catchment" ) );

    addPage( createMappingCatchmentPage );

    createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, "Gewässer einlesen",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, dummyFeatureType );
    addPage( createMappingRiverPage );

    createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, "Knoten einlesen",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Node" ) );
    addPage( createMappingNodePage );
    createMappingHydrotopPage = new KalypsoNAFileImportPage( HYDROTOP_PAGE,
        "Hydrotopdatei in den Workspace importieren", ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    //		createMappingHydrotopPage = new KalypsoNAProjectWizardPage(
    //				HYDROTOP_PAGE, "Hydrotope einlesen",
    //				ImageProvider.IMAGE_KALYPSO_ICON_BIG,
    //				getFeatureType("_Hydrotop"));
    addPage( createMappingHydrotopPage );
  }

  private FeatureType getFeatureType( String featureName )
  {
    FeatureType ft = modelSchema.getFeatureType( featureName );
    return ft;
  }

  /**
   * We will accept the selection in the workbench to see if we can initialize
   * from it.
   * 
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    this.selection = selection;
  }

  /**
   * This method creates the new Project and all the necessary , performs the
   * mapping and writes the new modell.gml file .
   * 
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    workspacePath = createProjectPage.getLocationPath();
    projectHandel = createProjectPage.getProjectHandle();
    try
    {
      projectHandel.create( null );
      projectHandel.open( null );
      //set charSet for the new project to the UTF-8 standard
      projectHandel.setDefaultCharset("UTF-8", null); 
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    //copy all the resources to the workspace into the new create project
    copyResourcesToProject( workspacePath.append( projectHandel.getFullPath() ) );

    //open modell.gml file to write imported feature
    try
    {
      modelPath = new Path( projectHandel.getFullPath().append( "/modell.gml" ).toString() );
      URL modelURL = new URL( ResourceUtilities.createURLSpec( modelPath ) );
      modelWS = GmlSerializer.createGMLWorkspace( modelURL, new UrlResolver() );
    }
    catch( Exception e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
    //map catchment shape file
    HashMap catchmentMapping = createMappingCatchmentPage.getMapping();

    if( catchmentMapping != null && catchmentMapping.size() != 0 )
    {
      List catchmentFeatureList = createMappingCatchmentPage.getFeatureList();
      mapCatchment( catchmentFeatureList, catchmentMapping );
    }
    //map river shape file
    HashMap riverMapping = createMappingRiverPage.getMapping();
    if( riverMapping != null && riverMapping.size() != 0 )
    {
      List riverFeatureList = createMappingRiverPage.getFeatureList();
      mapRiver( riverFeatureList, riverMapping );
    }
    //	map river shape file
    HashMap nodeMapping = createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      List nodeFeatureList = createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }
    //		write all new imported features to the modell.gml fiel in the
    // workspace
    try
    {
      IPath modelPath2 = workspacePath.append( modelPath );
      Writer writer = new FileWriter( modelPath2.toFile() );
      GmlSerializer.serializeWorkspace( writer, modelWS );
      writer.close();
    }
    catch( Exception e3 )
    {
      e3.printStackTrace();
      return false;
    }

    //copy Hydrotop file to workspace into the new project and add it as a
    // layer to the map temlate

    sourceURL = createMappingHydrotopPage.getFileURL();
    if( sourceURL != null )
    {
      try
      {
        File source = FileUtils.toFile( sourceURL );
        String shapeBaseSource = FileUtilities.nameWithoutExtension( source.toString() );
        String fileName = FileUtilities.nameWithoutExtension( source.getName() );
        String shapeBaseTarget = ( ( workspacePath.append( projectHandel.getFullPath().append(
            "/Shapes/" + fileName ) ) ).toFile() ).toString();

        //				ResourceUtilities.findFileFromURL()

        ResourcesPlugin.getWorkspace().build( IncrementalProjectBuilder.FULL_BUILD, null );
        //				String sourceFile = createMappingHydrotopPage.getFileURL()
        //						.getPath().substring(1);
        //				int index = sourceFile.lastIndexOf("/");
        //				String shapeBase = sourceFile.substring(index + 1);
        GMLWorkspace shapeWS = ShapeSerializer.deserialize( shapeBaseSource,
            createMappingHydrotopPage.getCoordinateSystem(), null );
        ShapeSerializer.serialize( shapeWS, shapeBaseTarget );
        addLayer( source );
        projectHandel.refreshLocal( IResource.DEPTH_INFINITE, null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        return false;
      }
    }//if
    createMappingCatchmentPage.dispose();
    createMappingRiverPage.dispose();
    createMappingNodePage.dispose();
    createMappingHydrotopPage.dispose();
    return true;
  }

  private void addLayer( File sourceFile ) throws Exception
  {
    String path = workspacePath.append( projectHandel.getFullPath() + "/Modell_Karten/Karten.gmt" )
        .toFile().toString();
    InputStream inputStream = getClass().getResourceAsStream(
        "../../../resources/Modell_Karten/Karte.gmt" );
    ObjectFactory typeOF = new ObjectFactory();
    org.kalypso.template.gismapview.ObjectFactory mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();
    Unmarshaller unmarshaller = mapTemplateOF.createUnmarshaller();
    Gismapview gismapview = (Gismapview)unmarshaller.unmarshal( inputStream );
    LayersType layers = gismapview.getLayers();
    List layerList = layers.getLayer();
    Layer newLayer = mapTemplateOF.createGismapviewTypeLayersTypeLayer();

    //set attributes for the layer
    newLayer.setName( "Hydrotope" );
    newLayer.setVisible( true );
    newLayer.setFeaturePath( "featureMember" );
    newLayer.setHref( "project:/Shapes/"
        + FileUtilities.nameWithoutExtension( sourceFile.getName() ) + "#EPSG:31467" );
    newLayer.setType( "simple" );
    newLayer.setLinktype( "shape" );
    newLayer.setActuate( "onRequest" );
    newLayer.setId( "ID_6" );

    List styleList = newLayer.getStyle();
    StyleType style = typeOF.createStyledLayerTypeStyleType();

    //set attributes for the style
    style.setLinktype( "sld" );
    style.setStyle( "hydrotop" );
    style.setActuate( "onRequest" );
    style.setHref( "../.styles/hydrotop.sld" );
    style.setType( "simple" );

    //add the style to the layer
    styleList.add( style );
    layerList.add( newLayer );

    //gismapview.setLayers(layers);
    // create new layer:
    //		IPath projectPath = createProjectPage.getLocationPath();
    Marshaller marshaller = mapTemplateOF.createMarshaller();
    //		IPath mapViewFile = projectPath.append("/Modell_Karten/Karte.gmt");
    FileWriter fw = new FileWriter( path );
    marshaller.marshal( gismapview, fw );
  }

  private void copyResourcesToProject( IPath path )
  {
    //get path for all resources
    IPath root = path;

    IPath schemaDir = path.append( "/.model/schema" );
    schemaDir.toFile().mkdirs();

    IPath modelDir = path.append( "/.model" );

    IPath stylesDir = path.append( "/.styles" );
    stylesDir.toFile().mkdirs();

    IPath templatesCalcCaseDir = path.append( "/.templates/calcCase" );
    templatesCalcCaseDir.toFile().mkdirs();

    IPath templatesDir = path.append( "/.templates" );

    IPath modelMapDir = path.append( "/Modell_Karten" );
    modelMapDir.toFile().mkdirs();

    IPath modelTableDir = path.append( "/Modell_Tabellen" );
    modelTableDir.toFile().mkdirs();

    IPath modelTreeDir = path.append( "/Modell_Baumansicht" );
    modelTreeDir.toFile().mkdirs();

    IPath sourceDir = path.append( "/Shapes" );
    sourceDir.toFile().mkdirs();

    //get resouces as input stream
    final InputStream is1 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/control.xsd" );
    final InputStream is2 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/feature.xsd" );
    final InputStream is3 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/geometry.xsd" );
    final InputStream is4 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/nacontrol.xsd" );
    final InputStream is5 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/namodell.xsd" );
    final InputStream is6 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/obslink.xsd" );
    //		final InputStream is7 = getClass().getResourceAsStream(
    //				"../../../resources/.model/schema/ombrometer.xsd");
    final InputStream is8 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/xlink.xsd" );
    final InputStream is10 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/xlinks.xsd" );
    final InputStream is11 = getClass().getResourceAsStream(
        "../../../resources/.model/schema/xlinksext.xsd" );

    final InputStream is12 = getClass().getResourceAsStream(
        "../../../resources/.model/.calculation.template" );
    final InputStream is13 = getClass().getResourceAsStream(
        "../../../resources/.model/.calculation.view" );
    //		final InputStream is14 = getClass().getResourceAsStream(
    //				"../../../resources/.model/info.txt");
    final InputStream is15 = getClass().getResourceAsStream(
        "../../../resources/.model/calcCaseConfig.xml" );
    final InputStream is16 = getClass().getResourceAsStream(
        "../../../resources/.model/calcWizard.xml" );
    final InputStream is17 = getClass().getResourceAsStream(
        "../../../resources/.model/modelspec.xml" );

    final InputStream is18 = getClass().getResourceAsStream(
        "../../../resources/.styles/RHBChannel.sld" );
    final InputStream is19 = getClass().getResourceAsStream(
        "../../../resources/.styles/expertCatchment.sld" );
    final InputStream is20 = getClass().getResourceAsStream(
        "../../../resources/.styles/expertKMChannel.sld" );
    final InputStream is21 = getClass().getResourceAsStream(
        "../../../resources/.styles/expertNode.sld" );
    final InputStream is22 = getClass().getResourceAsStream(
        "../../../resources/.styles/expertVChannel.sld" );
    final InputStream is23 = getClass().getResourceAsStream(
        "../../../resources/.styles/exportNode.sld" );
    final InputStream is24 = getClass().getResourceAsStream(
        "../../../resources/.styles/KMChannel.sld" );
    final InputStream is25 = getClass().getResourceAsStream( "../../../resources/.styles/Node.sld" );
    //		final InputStream is26 = getClass().getResourceAsStream(
    //				"../../../resources/.styles/Pegel.sld");
    final InputStream is27 = getClass().getResourceAsStream(
        "../../../resources/.styles/Subcatchments.sld" );
    final InputStream is28 = getClass().getResourceAsStream(
        "../../../resources/.styles/hydrotop.sld" );
    final InputStream is29 = getClass().getResourceAsStream(
        "../../../resources/.styles/VChannel.sld" );
    //		final InputStream is30 = getClass().getResourceAsStream(
    //				"../../../resources/.styles/Zufluss.sld");

    final InputStream is31 = getClass().getResourceAsStream(
        "../../../resources/.templates/steuerparameter.gft" );

    final InputStream is32 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/calcCase.gml" );
    final InputStream is33 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/control.gmt" );
    final InputStream is34 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/control.odt" );
    final InputStream is35 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertControl.gml" );
    final InputStream is36 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertControl.gtt" );
    final InputStream is37 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertKarte.gmt" );
    final InputStream is38 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertTabelleGewaesser.gtt" );
    final InputStream is39 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertTabelleKnoten.gtt" );
    final InputStream is40 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/expertTabelleTeilgebiete.gtt" );
    final InputStream is41 = getClass().getResourceAsStream(
        "../../../resources/.templates/calcCase/sce.xml" );

    final InputStream is42 = getClass().getResourceAsStream(
        "../../../resources/Modell_Baumansicht/modell.gmv" );

    final InputStream is43 = getClass().getResourceAsStream(
        "../../../resources/Modell_Tabellen/Tabelle_KMGewaesser.gtt" );
    final InputStream is44 = getClass().getResourceAsStream(
        "../../../resources/Modell_Tabellen/Tabelle_Knoten.gtt" );
    final InputStream is45 = getClass().getResourceAsStream(
        "../../../resources/Modell_Tabellen/Tabelle_Teilgebiete.gtt" );

    final InputStream is46 = getClass().getResourceAsStream( "../../../resources/modell.gml" );
    final InputStream is47 = getClass().getResourceAsStream( "../../../resources/.metadata" );
    //copy files to workspace
    try
    {
      
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/control.xsd" ).toFile(), is1 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/feature.xsd" ).toFile(), is2 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/geometry.xsd" ).toFile(), is3 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/nacontrol.xsd" ).toFile(), is4 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/namodell.xsd" ).toFile(), is5 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/obslink.xsd" ).toFile(), is6 );
      //			FileUtilities.makeFileFromStream(false, schemaDir.append(
      //					"/ombrometer.xsd").toFile(), is7);
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/xlink.xsd" ).toFile(), is8 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/xlinks.xsd" ).toFile(), is10 );
      FileUtilities.makeFileFromStream( false, schemaDir.append( "/xlinksext.xsd" ).toFile(), is11 );

      FileUtilities.makeFileFromStream( false,
          modelDir.append( "/.calculation.template" ).toFile(), is12 );
      FileUtilities.makeFileFromStream( false, modelDir.append( "/.calculation.view" ).toFile(),
          is13 );
      //			FileUtilities.makeFileFromStream(false, modelDir
      //					.append("/info.txt").toFile(), is14);
      FileUtilities.makeFileFromStream( false, modelDir.append( "/calcCaseConfig.xml" ).toFile(),
          is15 );
      FileUtilities.makeFileFromStream( false, modelDir.append( "/calcWizard.xml" ).toFile(), is16 );
      FileUtilities.makeFileFromStream( false, modelDir.append( "/modelspec.xml" ).toFile(), is17 );

      FileUtilities
          .makeFileFromStream( false, stylesDir.append( "/RHBChannel.sld" ).toFile(), is18 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/expertCatchment.sld" ).toFile(),
          is19 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/expertKMChannel.sld" ).toFile(),
          is20 );
      FileUtilities
          .makeFileFromStream( false, stylesDir.append( "/expertNode.sld" ).toFile(), is21 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/expertVChannel.sld" ).toFile(),
          is22 );
      FileUtilities
          .makeFileFromStream( false, stylesDir.append( "/exportNode.sld" ).toFile(), is23 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/KMChannel.sld" ).toFile(), is24 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/Node.sld" ).toFile(), is25 );
      //			FileUtilities.makeFileFromStream(false, stylesDir.append(
      //					"/Pegel.sld").toFile(), is26);
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/Subcatchments.sld" ).toFile(),
          is27 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/hydrotop.sld" ).toFile(), is28 );
      FileUtilities.makeFileFromStream( false, stylesDir.append( "/VChannel.sld" ).toFile(), is29 );
      //			FileUtilities.makeFileFromStream(false, stylesDir.append(
      //					"/Zufluss.sld").toFile(), is30);

      FileUtilities.makeFileFromStream( false, templatesDir.append( "/steuerparameter.gft" )
          .toFile(), is31 );

      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/calcCase.gml" )
          .toFile(), is32 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/control.gmt" )
          .toFile(), is33 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/control.odt" )
          .toFile(), is34 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/expertControl.gml" )
          .toFile(), is35 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/expertControl.gtt" )
          .toFile(), is36 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/expertKarte.gmt" )
          .toFile(), is37 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append(
          "/expertTabelleGewaesser.gtt" ).toFile(), is38 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append(
          "/expertTabelleKnoten.gtt" ).toFile(), is39 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append(
          "/expertTabelleTeilgebiete.gtt" ).toFile(), is40 );
      FileUtilities.makeFileFromStream( false, templatesCalcCaseDir.append( "/sce.xml" ).toFile(),
          is41 );

      FileUtilities.makeFileFromStream( false, modelTreeDir.append( "/modell.gmv" ).toFile(), is42 );
      FileUtilities.makeFileFromStream( false, modelTableDir.append( "/Tabelle_KMGewaesser.gtt" )
          .toFile(), is43 );
      FileUtilities.makeFileFromStream( false, modelTableDir.append( "/Tabelle_Knoten.gtt" )
          .toFile(), is44 );
      FileUtilities.makeFileFromStream( false, modelTableDir.append( "/Tabelle_Teilgebiete.gtt" )
          .toFile(), is45 );

      FileUtilities.makeFileFromStream( false, root.append( "/modell.gml" ).toFile(), is46 );
      FileUtilities.makeFileFromStream( false, root.append( "/.metadata" ).toFile(), is47 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }//copyResourcesToProject

  public void mapCatchment( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = modelWS.getRootFeature();
    FeatureType modelFT = getFeatureType( "Catchment" );
    Feature catchmentCollectionFE = (Feature)rootFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)catchmentCollectionFE.getProperty( "catchmentMember" );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      Feature targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), modelFT );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        String targetkey = (String)it.next();
        String sourcekey = (String)mapping.get( targetkey );
        Object so = sourceFeature.getProperty( sourcekey );
        //The area property of the catchment is set at this point, to check if
        // this is not redundant ??
        //because the area can always be calculatet from the GM_Surface object.
        if( so instanceof GM_Surface )
        {
          Long area = new Long( Double.doubleToLongBits( ( (GM_Surface)so ).getArea() ) );
          FeatureProperty fpArea = FeatureFactory.createFeatureProperty( "flaech", area );
          targetFeature.setProperty( fpArea );
        }

        FeatureProperty fp = FeatureFactory.createFeatureProperty( targetkey, so );
        targetFeature.setProperty( fp );

      }
      catchmentList.add( targetFeature );
    }
  }

  public void mapNode( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = modelWS.getRootFeature();
    FeatureType modelFT = getFeatureType( "Node" );
    Feature nodeCollectionFE = (Feature)rootFeature.getProperty( "NodeCollectionMember" );
    List nodeList = (List)nodeCollectionFE.getProperty( "nodeMember" );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      Feature targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), modelFT );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        String targetkey = (String)it.next();
        String sourcekey = (String)mapping.get( targetkey );
        Object so = sourceFeature.getProperty( sourcekey );

        FeatureProperty fp = FeatureFactory.createFeatureProperty( targetkey, so );
        targetFeature.setProperty( fp );

      }
      nodeList.add( targetFeature );
    }
  }

  public void mapRiver( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = modelWS.getRootFeature();

    Feature channelCollectionFE = (Feature)rootFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCollectionFE.getProperty( "channelMember" );
    //StrangArt is defined in dummyFeatureType (member variable)
    String typeKey = (String)mapping.get( "StrangArt" );
    //remove the channel type mapping (just needed once)
    mapping.remove( typeKey );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      Object o = sourceFeature.getProperty( typeKey );
      int channelType = 0;
      if( o instanceof String )
        channelType = ( new Integer( (String)o ) ).intValue();
      else if( o instanceof Integer )
        channelType = ( (Integer)o ).intValue();
      else
        throw new NumberFormatException(
            "Channel type must be Integer or String, problem mapping shape file attribute." );

      Feature targetFeature = null;
      switch( channelType )
      {
      case 0:
      {
        FeatureType vFT = getFeatureType( "VirtualChannel" );
        targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), vFT );
        break;
      }
      case 1:
      {

        FeatureType kmFT = getFeatureType( "KMChannel" );
        targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), kmFT );
        break;
      }
      case 2:
      {
        FeatureType storageFT = getFeatureType( "StorageChannel" );
        targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), storageFT );
        break;
      }
      case 3:
      {
        throw new NotImplementedException( "reservoir with evaporation (rht) not implemented yet" );
      }
      default:
      {
        break;
      }
      }//switch
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        String targetkey = (String)it.next();
        String sourcekey = (String)mapping.get( targetkey );
        Object so = sourceFeature.getProperty( sourcekey );

        FeatureProperty fp = FeatureFactory.createFeatureProperty( targetkey, so );
        targetFeature.setProperty( fp );

      }
      channelList.add( targetFeature );

    }//for i

  }//mapRiver

  public GMLSchema getModelSchema()
  {
    return modelSchema;
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
}