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

import java.io.File;
import java.io.FileInputStream;
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
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.internal.resources.ProjectDescription;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
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
import org.kalypso.java.util.zip.ZipUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.util.url.UrlResolver;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author kuepfer
 *  
 */
public class KalypsoNAProjectWizard extends Wizard implements INewWizard
{

  //Constants
  static final String CATCHMENT_PAGE = "page_type:catchment";

  static final String HYDROTOP_PAGE = "page_type:hydrotop";

  static final String NODE_PAGE = "page_type:node";

  static final String RIVER_PAGE = "page_type:river";

  static final String PROJECT_PAGE = "page_type:createNewProject";

  private final String m_resourceBase = "resources/.projecttemplate.zip";

  static final FeatureType dummyFeatureType = FeatureFactory.createFeatureType( "Gew‰sser",
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
              .getName(), false, null ) }, new int[]
      {
          1,
          1 }, new int[]
      {
          1,
          1 }, null, null );

  private KalypsoNAProjectWizardPage createMappingCatchmentPage;

  private KalypsoNAFileImportPage createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage createMappingNodePage;

  private KalypsoNAProjectWizardPage createMappingRiverPage;

  private WizardNewProjectCreationPage createProjectPage;

  private GMLSchema m_modelSchema;

  private URL sourceURL;

  private URL m_modelSchemaURL = getClass().getResource( "resources/.model/schema/namodell.xsd" );

  private GMLWorkspace modelWS;

  IPath workspacePath;

  IProject projectHandel;

  IPath modelPath;

  ISelection selection;

  IWorkspace workspace;

  //	IStructuredSelection structSelection;

  /**
   *  
   */
  public KalypsoNAProjectWizard()
  {
    super();
    try
    {
      //      TODO: jh, schemata an zentrale speichern und von dort aufrufen, damit
      // hier nicht st‰ndig aktualisiert werden muss.
      m_modelSchema = GMLSchemaCache.getSchema( m_modelSchemaURL );
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
    try
    {
      createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
      createProjectPage.setDescription( "Dieser Dialog erstellt ein neues NA-Modell Projekt." );
      createProjectPage.setTitle( "Neues NA-Modell Projekt" );
      createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( createProjectPage );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE,
        "Teilgebiete einlesen", ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Catchment" ) );

    addPage( createMappingCatchmentPage );

    createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, "Str‰nge einlesen",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, dummyFeatureType );
    addPage( createMappingRiverPage );

    createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, "Knoten einlesen",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Node" ) );
    addPage( createMappingNodePage );
    createMappingHydrotopPage = new KalypsoNAFileImportPage( HYDROTOP_PAGE,
        "Hydrotopdatei in den Workspace importieren", ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    //		creaSteMappingHydrotopPage = new KalypsoNAProjectWizardPage(
    //				HYDROTOP_PAGE, "Hydrotope einlesen",
    //				ImageProvider.IMAGE_KALYPSO_ICON_BIG,
    //				getFeatureType("_Hydrotop"));
    addPage( createMappingHydrotopPage );
  }

  private FeatureType getFeatureType( String featureName )
  {
    FeatureType ft = m_modelSchema.getFeatureType( featureName );
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
      IProjectDescription description = new ProjectDescription();
      String[] nanature =
      { "org.kalypso.ui.ModelNature" };
      description.setNatureIds( nanature );
      projectHandel.create( description, null );
      //      projectHandel.create( null );
      projectHandel.open( null );
      //set charSet for the new project to the UTF-8 standard
      projectHandel.setDefaultCharset( "UTF-8", null );
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    //copy all the resources to the workspace into the new created project
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
    //		write all new imported features to the modell.gml file in the
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
    String path = workspacePath.append( projectHandel.getFullPath() + "/BasisKarten/Hydrotope.gmt" )
        .toFile().toString();
    File mapfile = workspacePath.append( projectHandel.getFullPath() + "/BasisKarten/Hydrotope.gmt" )
        .toFile();
    InputStream inputStream = new FileInputStream( mapfile );

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
    //		IPath mapViewFile = projectPath.append("/BasisKarten/Hydrotope.gmt");
    FileWriter fw = new FileWriter( path );
    marshaller.marshal( gismapview, fw );
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
    return m_modelSchema;
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