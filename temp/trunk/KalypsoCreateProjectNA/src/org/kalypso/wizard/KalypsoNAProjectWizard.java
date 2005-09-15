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

import java.io.FileWriter;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.internal.resources.ProjectDescription;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
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
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends Wizard implements INewWizard
{
  //Constants
  static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  private final String m_resourceBase = WizardMessages.getString( "KalypsoNAProjectWizard.ResourcePath" ); //$NON-NLS-1$

  static final FeatureType dummyFeatureType = FeatureFactory
      .createFeatureType(
          "Gew‰sser", "wizard.kalypso.na", //$NON-NLS-1$ //$NON-NLS-2$
          new FeatureTypeProperty[]
          {
              FeatureFactory.createFeatureTypeProperty(
                  "Ort", "wizard.kalypso.na", GeometryUtilities.getLineStringClass().getName(), //$NON-NLS-2$
                  false, null ),
              FeatureFactory.createFeatureTypeProperty( "name", "wizard.kalypso.na", String.class.getName(), false, //$NON-NLS-1$ //$NON-NLS-2$
                  null ),
              FeatureFactory.createFeatureTypeProperty( "description", "wizard.kalypso.na", String.class.getName(), //$NON-NLS-1$ //$NON-NLS-2$
                  false, null ),
              //              FeatureFactory.createFeatureTypeProperty( "inum", "wizard.kalypso.na", Integer.class.getName(), false,
              // //$NON-NLS-1$
              // //$NON-NLS-2$
              //                  null ),
              FeatureFactory
                  .createFeatureTypeProperty(
                      WizardMessages.getString( "KalypsoNAProjectWizard.ChannelFeatureTypeProperty" ), "wizard.kalypso.na", Integer.class.getName(), //$NON-NLS-1$//$NON-NLS-2$
                      false, null ) }, new int[]
          {
              1,
              1 }, new int[]
          {
              1,
              1 }, null, null );

  private KalypsoNAProjectWizardPage createMappingCatchmentPage;

  private KalypsoNAProjectWizardPage createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage createMappingNodePage;

  private KalypsoNAProjectWizardPage createMappingRiverPage;

  private WizardNewProjectCreationPage createProjectPage;

  private GMLSchema m_modelSchema;

  private GMLWorkspace modelWS;

  IPath workspacePath;

  IProject projectHandel;

  IPath m_modelPath;

  ISelection m_selection;

  IWorkspace workspace;

  private GMLSchema m_hydrotopSchema;

  private Path m_hydPath;

  private GMLWorkspace m_hydWS;

  //	IStructuredSelection structSelection;

  public KalypsoNAProjectWizard()
  {
    super();
    try
    {
      m_modelSchema = GMLSchemaCatalog.getSchema( "http://www.tuhh.de/kalypsoNA" ); //$NON-NLS-1$
      m_hydrotopSchema = GMLSchemaCatalog.getSchema( "http://www.tuhh.de/hydrotop" ); //$NON-NLS-1$
      setNeedsProgressMonitor( true );
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
      createProjectPage.setDescription( WizardMessages.getString( "KalypsoNAProjectWizard.DescriptionNewProjectPage" ) ); //$NON-NLS-1$
      createProjectPage.setTitle( WizardMessages.getString( "KalypsoNAProjectWizard.TitleNewProjectPage" ) ); //$NON-NLS-1$
      createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( createProjectPage );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE, WizardMessages
        .getString( "KalypsoNAProjectWizard.CatchmentPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Catchment" ) ); //$NON-NLS-1$

    addPage( createMappingCatchmentPage );

    createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, WizardMessages
        .getString( "KalypsoNAProjectWizard.ChannelPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, dummyFeatureType );
    addPage( createMappingRiverPage );

    createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, WizardMessages
        .getString( "KalypsoNAProjectWizard.NodePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Node" ) ); //$NON-NLS-1$
    addPage( createMappingNodePage );
    createMappingHydrotopPage = new KalypsoNAProjectWizardPage( HYDROTOP_PAGE, WizardMessages
        .getString( "KalypsoNAProjectWizard.HydrotopePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Hydrotop" ) ); //$NON-NLS-1$
    addPage( createMappingHydrotopPage );
  }

  private FeatureType getFeatureType( String featureName )
  {
    FeatureType ft = null;
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
    this.m_selection = selection;
  }

  /**
   * This method creates the new Project and all the necessary , performs the mapping and writes the new modell.gml file .
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
      { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
      description.setNatureIds( nanature );
      projectHandel.create( description, null );
      projectHandel.open( null );
      //set charSet for the new project to the UTF-8 standard
      projectHandel.setDefaultCharset( "UTF-8", null ); //$NON-NLS-1$
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    //copy all the resources to the workspace into the new created project
    copyResourcesToProject( workspacePath.append( projectHandel.getFullPath() ) );
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
      //open modell.gml and hydrotop.gml file to write imported feature
      m_modelPath = new Path( projectHandel.getFullPath().append( "/modell.gml" ).toString() ); //$NON-NLS-1$
      URL modelURL = new URL( ResourceUtilities.createURLSpec( m_modelPath ) );
      modelWS = GmlSerializer.createGMLWorkspace( modelURL, new UrlResolver() );
      m_hydPath = new Path( projectHandel.getFullPath().append( "/hydrotop.gml" ).toString() ); //$NON-NLS-1$
      URL hydURL = new URL( ResourceUtilities.createURLSpec( m_hydPath ) );
      m_hydWS = GmlSerializer.createGMLWorkspace( hydURL, new UrlResolver() );

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
    //map node shape file
    HashMap nodeMapping = createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      List nodeFeatureList = createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }

    //map hydrotop shape file
    HashMap hydMapping = createMappingHydrotopPage.getMapping();
    if( hydMapping != null && hydMapping.size() != 0 )
    {
      List hydFeatureList = createMappingHydrotopPage.getFeatureList();
      mapHyd( hydFeatureList, hydMapping );
    }

    //		write all new imported features to the modell.gml and hydrotop.gml file
    // in the
    // workspace
    try
    {
      //model.gml
      IPath modelPath2 = workspacePath.append( m_modelPath );
      OutputStreamWriter modelWriter = new FileWriter( modelPath2.toFile() );
      GmlSerializer.serializeWorkspace( modelWriter, modelWS );
      modelWriter.close();
      //hydrotop.gml
      IPath hydPath = workspacePath.append( m_hydPath );
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
    createMappingCatchmentPage.dispose();
    createMappingRiverPage.dispose();
    createMappingNodePage.dispose();
    createMappingHydrotopPage.dispose();
    return true;
  }

  private void mapHyd( List sourceFeatureList, HashMap mapping )
  {
    Feature rootFeature = m_hydWS.getRootFeature();
    FeatureType hydFT = getFeatureType( "Hydrotop" ); //$NON-NLS-1$
    Feature hydCollectionFE = (Feature)rootFeature.getProperty( "HydrotopCollectionMember" ); //$NON-NLS-1$
    List hydList = (List)hydCollectionFE.getProperty( "HydrotopMember" ); //$NON-NLS-1$

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      Feature targetFeature = FeatureFactory.createFeature( sourceFeature.getId(), hydFT, true );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        String targetkey = (String)it.next();
        String sourcekey = (String)mapping.get( targetkey );
        Object so = sourceFeature.getProperty( sourcekey );

        final double area;
        if( so instanceof GM_Object )
        {
          area = GeometryUtilities.calcArea( (GM_Object)so );
          FeatureProperty fpArea = FeatureFactory.createFeatureProperty( "flaech", new Double( area ) ); //$NON-NLS-1$
          targetFeature.setProperty( fpArea );
        }

        FeatureProperty fp = FeatureFactory.createFeatureProperty( targetkey, so );
        targetFeature.setProperty( fp );

      }
      // TODO: delete if default values in schema!
      // if factor of the sealing rate isn¥t set, then the factor will be set as 1.0 (instead of default 0.0)
      if( !mapping.keySet().contains( "fak_vers" ) ) //$NON-NLS-1$
      {
        final double fak_vers = 1.0;
        FeatureProperty fpFak_vers = FeatureFactory.createFeatureProperty( "fak_vers", new Double( fak_vers ) ); //$NON-NLS-1$
        targetFeature.setProperty( fpFak_vers );
      }
      hydList.add( targetFeature );
    }
  }

  private void copyResourcesToProject( IPath path )
  {
    final String resource = m_resourceBase;
    //    System.out.print( "resource: " + resource + "\n" );
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
    FeatureType modelFT = getFeatureType( "Catchment" ); //$NON-NLS-1$
    Feature catchmentCollectionFE = (Feature)rootFeature.getProperty( "CatchmentCollectionMember" ); //$NON-NLS-1$
    List catchmentList = (List)catchmentCollectionFE.getProperty( "catchmentMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String)mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;
    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      final String fid;
      if( idColKey != null )
        fid = "Catchment" + ( sourceFeature.getProperty( idColKey ) ).toString(); //$NON-NLS-1$
      else
        fid = sourceFeature.getId();
      Feature targetFeature = FeatureFactory.createFeature( fid, modelFT, true );
      Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        String targetkey = (String)it.next();
        String sourcekey = (String)mapping.get( targetkey );
        Object so = sourceFeature.getProperty( sourcekey );
        if( so instanceof GM_Surface )
        {
          Long area = new Long( (long)( (GM_Surface)so ).getArea() );
          FeatureProperty fpArea = FeatureFactory.createFeatureProperty( "flaech", area ); //$NON-NLS-1$
          targetFeature.setProperty( fpArea );
        }

        FeatureProperty fp = FeatureFactory.createFeatureProperty( targetkey, so );
        targetFeature.setProperty( fp );
      }
      // Bodenkorrekturparameter erstellen
      //TODO: Anzahl durch Dialog
      List list = new ArrayList();
      FeatureProperty bodenkorrekturProperty = FeatureFactory.createFeatureProperty( "bodenkorrekturmember", list );
      targetFeature.setProperty( bodenkorrekturProperty );
      for( int j = 0; j < 3; j++ )
      {
        FeatureTypeProperty bodFtProp = modelFT.getProperty( "bodenkorrekturmember" );
        FeatureType bodenKorrekturFT = ( (FeatureAssociationTypeProperty_Impl)bodFtProp ).getAssociationFeatureTypes()[0];
        Feature newFeature = modelWS.createFeature( bodenKorrekturFT );
        try
        {
          modelWS.addFeatureAsComposition( targetFeature, "bodenkorrekturmember", j, newFeature );
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

    Feature rootFeature = modelWS.getRootFeature();
    FeatureType modelFT = getFeatureType( "Node" ); //$NON-NLS-1$
    Feature nodeCollectionFE = (Feature)rootFeature.getProperty( "NodeCollectionMember" ); //$NON-NLS-1$
    List nodeList = (List)nodeCollectionFE.getProperty( "nodeMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String)mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      final String fid;
      if( idColKey != null )
        fid = "Node" + (String)sourceFeature.getProperty( idColKey ); //$NON-NLS-1$
      else
        fid = sourceFeature.getId();
      Feature targetFeature = FeatureFactory.createFeature( fid, modelFT, true );
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

    Feature channelCollectionFE = (Feature)rootFeature.getProperty( "ChannelCollectionMember" ); //$NON-NLS-1$
    List channelList = (List)channelCollectionFE.getProperty( "channelMember" ); //$NON-NLS-1$

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String)mapping.get( "name" ); //$NON-NLS-1$
    }
    else
      idColKey = null;

    //StrangArt is defined in dummyFeatureType (member variable)
    String typeKey = (String)mapping.get( WizardMessages
        .getString( "KalypsoNAProjectWizard.ChannelFeatureTypeProperty" ) ); //$NON-NLS-1$
    //remove the channel type mapping (just needed once)
    mapping.remove( typeKey );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      Feature sourceFeature = (Feature)sourceFeatureList.get( i );
      Object o = sourceFeature.getProperty( typeKey );
      int channelType = 0;
      try
      {
        channelType = ( (Integer)SpecialPropertyMapper.map( o.getClass().getName(), Integer.class.getName(), o ) )
            .intValue();
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new NumberFormatException( WizardMessages.getString( "KalypsoNAProjectWizard.ExceptionStrangArt" ) ); //$NON-NLS-1$
      }

      Feature targetFeature = null;
      switch( channelType )
      {
      case 0:
      {
        final String fid;
        if( idColKey != null )
          fid = "VirtualChannel" + (String)sourceFeature.getProperty( idColKey ); //$NON-NLS-1$
        else
          fid = sourceFeature.getId();

        FeatureType vFT = getFeatureType( "VirtualChannel" ); //$NON-NLS-1$
        targetFeature = FeatureFactory.createFeature( fid, vFT, true );
        break;
      }
      case 1:
      {

        final String fid;
        if( idColKey != null )
          fid = "KMChannel" + (String)sourceFeature.getProperty( idColKey ); //$NON-NLS-1$
        else
          fid = sourceFeature.getId();

        FeatureType kmFT = getFeatureType( "KMChannel" ); //$NON-NLS-1$
        targetFeature = FeatureFactory.createFeature( fid, kmFT, true );

        // KMparameter erstellen
        //TODO: Anzahl durch Dialog
        List list = new ArrayList();
        FeatureProperty kmParameterProperty = FeatureFactory.createFeatureProperty( "KMParameterMember", list );
        targetFeature.setProperty( kmParameterProperty );
        for( int j = 0; j < 5; j++ )
        {
          FeatureTypeProperty kmFtProp = kmFT.getProperty( "KMParameterMember" );
          FeatureType kmParameterFT = ( (FeatureAssociationTypeProperty_Impl)kmFtProp ).getAssociationFeatureTypes()[0];
          Feature newFeature = modelWS.createFeature( kmParameterFT );
          try
          {
            modelWS.addFeatureAsComposition( targetFeature, "KMParameterMember", j, newFeature );
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
        final String fid;
        if( idColKey != null )
          fid = "StorageChannel" + (String)sourceFeature.getProperty( idColKey ); //$NON-NLS-1$
        else
          fid = sourceFeature.getId();

        FeatureType storageFT = getFeatureType( "StorageChannel" ); //$NON-NLS-1$
        targetFeature = FeatureFactory.createFeature( fid, storageFT, true );
        break;
      }
      case 3:
      {
        throw new NotImplementedException( WizardMessages
            .getString( "KalypsoNAProjectWizard.ExceptionNotImplementedRHT" ) ); //$NON-NLS-1$
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