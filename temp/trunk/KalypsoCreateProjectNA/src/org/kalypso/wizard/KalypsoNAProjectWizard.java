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

/*
 * Created on 31.01.2005
 *  
 */
package org.kalypso.wizard;

import java.io.FileWriter;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.internal.resources.ProjectDescription;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends Wizard implements INewWizard
{
  // Constants
  static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  static final String PREFERENCE_PAGE = "page_type:preferences"; //$NON-NLS-1$

  private final String m_resourceBase = WizardMessages.getString( "KalypsoNAProjectWizard.ResourcePath" ); //$NON-NLS-1$

  final HashMap<String, Feature> m_IDMap = new HashMap<String, Feature>();

  // static final IFeatureType m_dummyFeatureType = FeatureFactory.createFeatureType( "Gew�sser", "wizard.kalypso.na",
  // //$NON-NLS-1$ //$NON-NLS-2$

  // new IPropertyType[]{
  // FeatureFactory.createFeatureTypeProperty( new QName( "wizard.kalypso.na", "Ort" ),
  // GeometryUtilities.getLineStringClass(),
  // false, null ),
  // FeatureFactory.createFeatureTypeProperty( new QName( "wizard.kalypso.na", "name" ), String.class, false,
  // //$NON-NLS-1$ //$NON-NLS-2$
  // null ),
  // FeatureFactory.createFeatureTypeProperty( new QName( "wizard.kalypso.na", "description" ), String.class,
  // //$NON-NLS-1$ //$NON-NLS-2$
  // false, null ),

  // FeatureFactory.createFeatureTypeProperty( "inum", "wizard.kalypso.na", Integer.class.getName(), false,
  // //$NON-NLS-1$
  // //$NON-NLS-2$
  // null ),

  // static final IFeatureType m_dummyFeatureType;

  private IFeatureType createGewaesserFT( )
  {

    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    final ITypeHandler lineStringTH = registry.getTypeHandlerForClassName( GeometryUtilities.getLineStringClass() );
    final IPropertyType pt1 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "Ort" ), lineStringTH.getTypeName()[0], lineStringTH, 0, 1 );

    final ITypeHandler stringTH = registry.getTypeHandlerForClassName( String.class );
    final IPropertyType pt2 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "name" ), stringTH.getTypeName()[0], stringTH, 1, 1 );

    final IPropertyType pt3 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "description" ), stringTH.getTypeName()[0], stringTH, 1, 1 );

    final ITypeHandler integerTH = registry.getTypeHandlerForClassName( Integer.class );
    final IPropertyType pt4 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "StrangArt" ), integerTH.getTypeName()[0], integerTH, 0, 1 );
    final IPropertyType[] pts = new IPropertyType[] { pt1, pt2, pt3, pt4 };

    return GMLSchemaFactory.createFeatureType( new QName( "wizard.kalypso.na", "Gew�sser" ), pts );
  }

  private KalypsoNAProjectWizardPage m_createMappingCatchmentPage;

  private KalypsoNAProjectWizardPage m_createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage m_createMappingNodePage;

  private KalypsoNAProjectWizardPage m_createMappingRiverPage;

  private KalypsoNAProjectPreferences m_createPreferencePage;

  private WizardNewProjectCreationPage m_createProjectPage;

  private final GMLSchema m_modelSchema;

  private GMLWorkspace m_modelWS;

  private IPath m_workspacePath;

  private IProject m_projectHandel;

  private IPath m_modelPath;

  private GMLSchema m_hydrotopSchema;

  private Path m_hydPath;

  private GMLWorkspace m_hydWS;

  // IStructuredSelection structSelection;

  public KalypsoNAProjectWizard( )
  {
    GMLSchema schema = null;
    try
    {
      schema = GMLSchemaCatalog.getSchema( "http://www.tuhh.de/kalypsoNA" ); //$NON-NLS-1$
      m_hydrotopSchema = GMLSchemaCatalog.getSchema( "http://www.tuhh.de/hydrotop" ); //$NON-NLS-1$
      setNeedsProgressMonitor( true );
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }
    m_modelSchema = schema;

  }

  @Override
  public void addPages( )
  {
    try
    {
      m_createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
      m_createProjectPage.setDescription( WizardMessages.getString( "KalypsoNAProjectWizard.DescriptionNewProjectPage" ) ); //$NON-NLS-1$
      m_createProjectPage.setTitle( WizardMessages.getString( "KalypsoNAProjectWizard.TitleNewProjectPage" ) ); //$NON-NLS-1$
      m_createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( m_createProjectPage );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    m_createPreferencePage = new KalypsoNAProjectPreferences( PREFERENCE_PAGE, m_modelSchema );
    addPage( m_createPreferencePage );

    m_createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE, WizardMessages.getString( "KalypsoNAProjectWizard.CatchmentPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Catchment" ) ); //$NON-NLS-1$

    addPage( m_createMappingCatchmentPage );
    final IFeatureType gewaesserFT = createGewaesserFT();
    m_createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, WizardMessages.getString( "KalypsoNAProjectWizard.ChannelPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, gewaesserFT );
    addPage( m_createMappingRiverPage );

    m_createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, WizardMessages.getString( "KalypsoNAProjectWizard.NodePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Node" ) ); //$NON-NLS-1$
    addPage( m_createMappingNodePage );
    m_createMappingHydrotopPage = new KalypsoNAProjectWizardPage( HYDROTOP_PAGE, WizardMessages.getString( "KalypsoNAProjectWizard.HydrotopePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Hydrotop" ) ); //$NON-NLS-1$
    addPage( m_createMappingHydrotopPage );
  }

  private IFeatureType getFeatureType( String featureName )
  {
    IFeatureType ft = null;
    ft = m_modelSchema.getFeatureType( featureName );
    if( ft == null )
      ft = m_hydrotopSchema.getFeatureType( featureName );
    return ft;

  }

  /**
   * We will accept the selection in the workbench to see if we can initialize from it.
   * 
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  }

  /**
   * This method creates the new Project and all the necessary , performs the mapping and writes the new modell.gml file .
   * 
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    m_workspacePath = m_createProjectPage.getLocationPath();
    m_projectHandel = m_createProjectPage.getProjectHandle();

    try
    {
      final IProjectDescription description = new ProjectDescription();
      String[] nanature = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
      description.setNatureIds( nanature );
      m_projectHandel.create( description, null );
      m_projectHandel.open( null );
      // set charSet for the new project to the UTF-8 standard
      m_projectHandel.setDefaultCharset( "UTF-8", null ); //$NON-NLS-1$
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // copy all the resources to the workspace into the new created project
    copyResourcesToProject( m_workspacePath.append( m_projectHandel.getFullPath() ) );
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
      // open modell.gml and hydrotop.gml file to write imported feature
      m_modelPath = new Path( m_projectHandel.getFullPath().append( "/modell.gml" ).toString() ); //$NON-NLS-1$
      URL modelURL = new URL( ResourceUtilities.createURLSpec( m_modelPath ) );
      m_modelWS = GmlSerializer.createGMLWorkspace( modelURL, new UrlResolver() );
      m_hydPath = new Path( m_projectHandel.getFullPath().append( "/hydrotop.gml" ).toString() ); //$NON-NLS-1$
      URL hydURL = new URL( ResourceUtilities.createURLSpec( m_hydPath ) );
      m_hydWS = GmlSerializer.createGMLWorkspace( hydURL, new UrlResolver() );

    }
    catch( Exception e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
    // map catchment shape file
    HashMap catchmentMapping = m_createMappingCatchmentPage.getMapping();

    if( catchmentMapping != null && catchmentMapping.size() != 0 )
    {
      List catchmentFeatureList = m_createMappingCatchmentPage.getFeatureList();
      mapCatchment( catchmentFeatureList, catchmentMapping );
    }
    // map river shape file
    HashMap riverMapping = m_createMappingRiverPage.getMapping();
    if( riverMapping != null && riverMapping.size() != 0 )
    {
      List riverFeatureList = m_createMappingRiverPage.getFeatureList();
      mapRiver( riverFeatureList, riverMapping );
    }
    // map node shape file
    HashMap nodeMapping = m_createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      List nodeFeatureList = m_createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }

    // map hydrotop shape file
    HashMap hydMapping = m_createMappingHydrotopPage.getMapping();
    if( hydMapping != null && hydMapping.size() != 0 )
    {
      List hydFeatureList = m_createMappingHydrotopPage.getFeatureList();
      mapHyd( hydFeatureList, hydMapping );
    }

    // write all new imported features to the modell.gml and hydrotop.gml file
    // in the
    // workspace
    try
    {
      // model.gml
      IPath modelPath2 = m_workspacePath.append( m_modelPath );
      OutputStreamWriter modelWriter = new FileWriter( modelPath2.toFile() );
      GmlSerializer.serializeWorkspace( modelWriter, m_modelWS );
      modelWriter.close();
      // hydrotop.gml
      IPath hydPath = m_workspacePath.append( m_hydPath );
      OutputStreamWriter hydrotopWriter = new FileWriter( hydPath.toFile() );
      GmlSerializer.serializeWorkspace( hydrotopWriter, m_hydWS );
      hydrotopWriter.close();
    }
    catch( Exception e3 )
    {
      e3.printStackTrace();
      return false;
    }
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( CoreException e2 )
    {
      e2.printStackTrace();
      return false;
    }
    m_createMappingCatchmentPage.dispose();
    m_createMappingRiverPage.dispose();
    m_createMappingNodePage.dispose();
    m_createMappingHydrotopPage.dispose();
    return true;
  }

  private void mapHyd( List sourceFeatureList, HashMap mapping )
  {
    Feature rootFeature = m_hydWS.getRootFeature();
    IFeatureType hydFT = getFeatureType( "Hydrotop" ); //$NON-NLS-1$
    List hydList = (List) rootFeature.getProperty( "hydrotopMember" ); //$NON-NLS-1$

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      Feature targetFeature = FeatureFactory.createFeature( rootFeature, sourceFeature.getId(), hydFT, true );
      final IPropertyType flaechPT = hydFT.getProperty( "area" );
      final IPropertyType fakVersPT = hydFT.getProperty( "corrSealing" );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = hydFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        final Object so = sourceFeature.getProperty( sourcekey );

        final double area;
        if( so instanceof GM_Object )
        {
          area = GeometryUtilities.calcArea( (GM_Object) so );
          targetFeature.setProperty( flaechPT, new Double( area ) );
        }
        targetFeature.setProperty( targetPT, so );

      }
      // TODO: delete if default values in schema!
      // if factor of the sealing rate isn�t set, then the factor will be set as 1.0 (instead of default 0.0)
      if( !mapping.keySet().contains( "corrSealing" ) ) //$NON-NLS-1$
      {
        final double fak_vers = 1.0;
        targetFeature.setProperty( fakVersPT, new Double( fak_vers ) );
      }
      hydList.add( targetFeature );
    }
  }

  private void copyResourcesToProject( IPath path )
  {
    final String resource = m_resourceBase;
    // System.out.print( "resource: " + resource + "\n" );
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

  /**
   * generates an ID based on the FeatureType. If the idColKey variable is set, then use this field to generate the ID
   * and check if the ID doesn�t exist in the idMap. if the id ColKey is not set, use the ID of the sourceFeature (shape
   * file).
   * 
   * @param idColKey
   * @param sourceFeature
   * @param IDText
   * @return fid
   */
  private String getId( final String idColKey, Feature sourceFeature, String IDText )
  {
    String fid;
    if( idColKey != null )
    {
      String idKey = (sourceFeature.getProperty( idColKey )).toString();
      if( !m_IDMap.containsKey( IDText + idKey ) )
      {
        fid = IDText + idKey;
        m_IDMap.put( fid, sourceFeature );
      }
      else
      {
        System.out.println( "Name des Elementes existiert bereits (Name: " + IDText + idKey + ")." );
        fid = sourceFeature.getId();
        m_IDMap.put( fid, sourceFeature );
        System.out.println( "Die interne ID wurde aus diesem Grund automatisch vergeben (Id: " + fid + ")." );
      }
    }
    else
      fid = sourceFeature.getId();
    return fid;
  }

  public void mapCatchment( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = m_modelWS.getRootFeature();
    IFeatureType modelFT = getFeatureType( "Catchment" ); //$NON-NLS-1$
    Feature catchmentCollectionFE = (Feature) rootFeature.getProperty( "CatchmentCollectionMember" ); //$NON-NLS-1$
    List catchmentList = (List) catchmentCollectionFE.getProperty( "catchmentMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String) mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;
    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final String fid = getId( idColKey, sourceFeature, "TG" );
      final Feature targetFeature = FeatureFactory.createFeature( catchmentCollectionFE, fid, modelFT, true );
      final IPropertyType flaechPT = modelFT.getProperty( "flaech" );
      final IRelationType bodenkorrekturMemberRT = (IRelationType) modelFT.getProperty( "bodenkorrekturmember" );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = modelFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        final Object so = sourceFeature.getProperty( sourcekey );
        if( so instanceof GM_Surface )
        {
          final Long area = new Long( (long) ((GM_Surface) so).getArea() );
          targetFeature.setProperty( flaechPT, area );
        }
        targetFeature.setProperty( targetPT, so );
      }
      // Bodenkorrekturparameter erstellen
      final List list = FeatureFactory.createFeatureList( targetFeature, bodenkorrekturMemberRT );
      targetFeature.setProperty( "bodenkorrekturmember", list ); //$NON-NLS-1$
      int soilLayerNo = Integer.parseInt( m_createPreferencePage.getSoilLayerNo() );
      for( int j = 0; j < soilLayerNo; j++ )
      {
        final IRelationType bodFtProp = (IRelationType) modelFT.getProperty( "bodenkorrekturmember" ); //$NON-NLS-1$
        final IFeatureType bodenKorrekturFT = bodFtProp.getTargetFeatureType();
        Feature newFeature = m_modelWS.createFeature( targetFeature, bodenKorrekturFT );
        try
        {

          m_modelWS.addFeatureAsComposition( targetFeature, bodenkorrekturMemberRT, j, newFeature ); //$NON-NLS-1$
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
      catchmentList.add( targetFeature );
    }
  }

  public void mapNode( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = m_modelWS.getRootFeature();
    IFeatureType modelFT = getFeatureType( "Node" ); //$NON-NLS-1$
    Feature nodeCollectionFE = (Feature) rootFeature.getProperty( "NodeCollectionMember" ); //$NON-NLS-1$
    List nodeList = (List) nodeCollectionFE.getProperty( "nodeMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String) mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final String fid = getId( idColKey, sourceFeature, "K" );
      Feature targetFeature = FeatureFactory.createFeature( nodeCollectionFE, fid, modelFT, true );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = modelFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        final Object so = sourceFeature.getProperty( sourcekey );
        targetFeature.setProperty( targetPT, so );
      }
      nodeList.add( targetFeature );
    }
  }

  public void mapRiver( List sourceFeatureList, HashMap mapping )
  {

    Feature rootFeature = m_modelWS.getRootFeature();

    Feature channelCollectionFE = (Feature) rootFeature.getProperty( "ChannelCollectionMember" ); //$NON-NLS-1$
    List channelList = (List) channelCollectionFE.getProperty( "channelMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String) mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;

    // StrangArt is defined in dummyFeatureType (member variable)
    String typeKey = (String) mapping.get( "StrangArt" ); //$NON-NLS-1$
    // remove the channel type mapping (just needed once)
    mapping.remove( typeKey );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      Object o = sourceFeature.getProperty( typeKey );
      int channelType = 0;
      try
      {
        channelType = ((Integer) SpecialPropertyMapper.map( o.getClass(), Integer.class, o )).intValue();
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new NumberFormatException( WizardMessages.getString( "KalypsoNAProjectWizard.ExceptionStrangArt" ) ); //$NON-NLS-1$
      }

      Feature targetFeature = null;
      final String fid = getId( idColKey, sourceFeature, "S" );
      switch( channelType )
      {
        case 0:
        {

          IFeatureType vFT = getFeatureType( "VirtualChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, fid, vFT, true );
          break;
        }
        case 1:
        {

          IFeatureType kmFT = getFeatureType( "KMChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, fid, kmFT, true );

          IRelationType parameterMemberRT = (IRelationType) kmFT.getProperty( "KMParameterMember" );
          final List list = FeatureFactory.createFeatureList( targetFeature, parameterMemberRT );
          targetFeature.setProperty( parameterMemberRT, list ); //$NON-NLS-1$
          int channelNo = Integer.parseInt( m_createPreferencePage.getKMChannelNo() );
          for( int j = 0; j < channelNo; j++ )
          {
            final IFeatureType kmParameterFT = parameterMemberRT.getTargetFeatureType( );
            final Feature newFeature = m_modelWS.createFeature( targetFeature, kmParameterFT );
            try
            {
              m_modelWS.addFeatureAsComposition( targetFeature, parameterMemberRT, j, newFeature ); //$NON-NLS-1$
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }

          break;
        }
        case 2:
        {
          IFeatureType storageFT = getFeatureType( "StorageChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, fid, storageFT, true );
          break;
        }
        case 3:
        {
          throw new NotImplementedException( WizardMessages.getString( "KalypsoNAProjectWizard.ExceptionNotImplementedRHT" ) ); //$NON-NLS-1$
        }
        default:
        {
          break;
        }
      }// switch
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final String sourcekey = (String) mapping.get( targetkey );
        if( "StrangArt".equals( targetkey ) )
          continue;
        final Object so = sourceFeature.getProperty( sourcekey );
        targetFeature.setProperty( targetkey, so );

      }
      channelList.add( targetFeature );

    }// for i

  }// mapRiver

  public GMLSchema getModelSchema( )
  {
    return m_modelSchema;
  }

  public boolean performCancle( )
  {
    try
    {
      m_projectHandel.delete( true, false, null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }
}