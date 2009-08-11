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
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.wizard.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends Wizard implements INewWizard
{
  // Constants
  public static final String NULL_KEY = "-NULL-"; //$NON-NLS-1$
  
  static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  static final String PREFERENCE_PAGE = "page_type:preferences"; //$NON-NLS-1$

  private final String m_resourceBase = "resources/.projecttemplate.zip"; //$NON-NLS-1$

  final HashMap<String, Feature> m_IDMap = new HashMap<String, Feature>();

  private IFeatureType createGewaesserFT( )
  {

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final QName featureQName = new QName( "wizard.kalypso.na", "Gew‰sser" ); //$NON-NLS-1$

    final IMarshallingTypeHandler lineStringTH = registry.getTypeHandlerForClassName( GeometryUtilities.getLineStringClass() );
    final IPropertyType pt1 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "Ort" ), lineStringTH, 0, 1, false ); //$NON-NLS-1$

    final IMarshallingTypeHandler stringTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );
    final IPropertyType pt2 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "name" ), stringTH, 1, 1, false ); //$NON-NLS-1$

    final IPropertyType pt3 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "description" ), stringTH, 1, 1, false ); //$NON-NLS-1$

    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
    final IPropertyType pt4 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "StrangArt" ), integerTH, 0, 1, false ); //$NON-NLS-1$
    final IPropertyType[] pts = new IPropertyType[] { pt1, pt2, pt3, pt4 };

    return GMLSchemaFactory.createFeatureType( featureQName, pts );
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
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      schema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
      m_hydrotopSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAHYDROTOP, (String) null );
      setNeedsProgressMonitor( true );
    }
    catch( final Exception e1 )
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
      m_createProjectPage.setDescription( Messages.getString( "KalypsoNAProjectWizard.DescriptionNewProjectPage" ) ); //$NON-NLS-1$
      m_createProjectPage.setTitle( Messages.getString( "KalypsoNAProjectWizard.TitleNewProjectPage" ) ); //$NON-NLS-1$
      m_createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( m_createProjectPage );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    m_createPreferencePage = new KalypsoNAProjectPreferences( PREFERENCE_PAGE, m_modelSchema );
    addPage( m_createPreferencePage );

    m_createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE, Messages.getString( "KalypsoNAProjectWizard.CatchmentPageTitle" ), //$NON-NLS-1$
    ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Catchment" ) ); //$NON-NLS-1$

    addPage( m_createMappingCatchmentPage );
    final IFeatureType gewaesserFT = createGewaesserFT();
    m_createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, Messages.getString( "KalypsoNAProjectWizard.ChannelPageTitle" ), //$NON-NLS-1$
    ImageProvider.IMAGE_KALYPSO_ICON_BIG, gewaesserFT );
    addPage( m_createMappingRiverPage );

    m_createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, Messages.getString( "KalypsoNAProjectWizard.NodePageTitle" ), //$NON-NLS-1$
    ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Node" ) ); //$NON-NLS-1$
    addPage( m_createMappingNodePage );
    m_createMappingHydrotopPage = new KalypsoNAProjectWizardPage( HYDROTOP_PAGE, Messages.getString( "KalypsoNAProjectWizard.HydrotopePageTitle" ), //$NON-NLS-1$
    ImageProvider.IMAGE_KALYPSO_ICON_BIG, getFeatureType( "Hydrotop" ) ); //$NON-NLS-1$
    addPage( m_createMappingHydrotopPage );
  }

  private IFeatureType getFeatureType( final String featureName )
  {
    IFeatureType ft = null;
    ft = m_modelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, featureName ) );
    if( ft == null )
      ft = m_hydrotopSchema.getFeatureType( new QName( NaModelConstants.NS_NAHYDROTOP, featureName ) );
    return ft;

  }

  /**
   * We will accept the selection in the workbench to see if we can initialize from it.
   * 
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
  }

  /**
   * This method creates the new Project and all the necessary , performs the mapping and writes the new modell.gml file
   * .
   * 
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // TODO:
    // - put all code into a WorkspaceModifyOperation
    // - run it with RunnableContextHelper on getContainer()
    // - remove cathes and show resulting status in errod dialog
    // - use operation's monitor to show progress (dont give null to Project.create and so on)

    m_workspacePath = m_createProjectPage.getLocationPath();
    m_projectHandel = m_createProjectPage.getProjectHandle();

    try
    {
      m_projectHandel.create( new NullProgressMonitor() );
      m_projectHandel.open( new NullProgressMonitor() );
      final IProjectDescription description = m_projectHandel.getDescription();
//      final IProjectDescription description = new ProjectDescription();
      final String[] nanature = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
      description.setNatureIds( nanature );
      m_projectHandel.setDescription( description, new NullProgressMonitor() );
//      m_projectHandel.create( description, null );
//      m_projectHandel.open( null );
      // set charSet for the new project to the UTF-8 standard
      // TODO: do not do such a thing (at least without comment why).
      // The workspace has its own preference settings
//      m_projectHandel.setDefaultCharset( "UTF-8", new NullProgressMonitor()  ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // copy all the resources to the workspace into the new created project
    copyResourcesToProject( m_workspacePath.append( m_projectHandel.getFullPath() ) );
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor()  );
      // open modell.gml and hydrotop.gml file to write imported feature
      m_modelPath = new Path( m_projectHandel.getFullPath().append( "/modell.gml" ).toString() ); //$NON-NLS-1$
      final URL modelURL = new URL( ResourceUtilities.createURLSpec( m_modelPath ) );
      m_modelWS = GmlSerializer.createGMLWorkspace( modelURL, null );
      m_hydPath = new Path( m_projectHandel.getFullPath().append( "/hydrotop.gml" ).toString() ); //$NON-NLS-1$
      final URL hydURL = new URL( ResourceUtilities.createURLSpec( m_hydPath ) );
      m_hydWS = GmlSerializer.createGMLWorkspace( hydURL, null );

    }
    catch( final Exception e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
    // map catchment shape file
    final HashMap catchmentMapping = m_createMappingCatchmentPage.getMapping();

    if( catchmentMapping != null && catchmentMapping.size() != 0 )
    {
      final List catchmentFeatureList = m_createMappingCatchmentPage.getFeatureList();
      mapCatchment( catchmentFeatureList, catchmentMapping );
    }
    // map river shape file
    final HashMap riverMapping = m_createMappingRiverPage.getMapping();
    if( riverMapping != null && riverMapping.size() != 0 )
    {
      final List riverFeatureList = m_createMappingRiverPage.getFeatureList();
      mapRiver( riverFeatureList, riverMapping );
    }
    // map node shape file
    final HashMap nodeMapping = m_createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      final List nodeFeatureList = m_createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }

    // map hydrotop shape file
    final HashMap hydMapping = m_createMappingHydrotopPage.getMapping();
    if( hydMapping != null && hydMapping.size() != 0 )
    {
      final List hydFeatureList = m_createMappingHydrotopPage.getFeatureList();
      mapHyd( hydFeatureList, hydMapping );
    }

    // write all new imported features to the modell.gml and hydrotop.gml file
    // in the
    // workspace
    try
    {
      // model.gml
      final IPath modelPath2 = m_workspacePath.append( m_modelPath );
      final OutputStreamWriter modelWriter = new FileWriter( modelPath2.toFile() );
      GmlSerializer.serializeWorkspace( modelWriter, m_modelWS );
      modelWriter.close();
      // hydrotop.gml
      final IPath hydPath = m_workspacePath.append( m_hydPath );
      final OutputStreamWriter hydrotopWriter = new FileWriter( hydPath.toFile() );
      GmlSerializer.serializeWorkspace( hydrotopWriter, m_hydWS );
      hydrotopWriter.close();
    }
    catch( final Exception e3 )
    {
      e3.printStackTrace();
      return false;
    }
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final CoreException e2 )
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

  private void mapHyd( final List sourceFeatureList, final HashMap mapping )
  {
    final Feature rootFeature = m_hydWS.getRootFeature();
    final IFeatureType hydFT = getFeatureType( "Hydrotop" ); //$NON-NLS-1$

    final FeatureList hydList = (FeatureList) rootFeature.getProperty( NaModelConstants.HYDRO_MEMBER );
    final IRelationType targetRelation = hydList.getParentFeatureTypeProperty();

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      final Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final Feature targetFeature = FeatureFactory.createFeature( rootFeature, targetRelation, sourceFeature.getId(), hydFT, true );
      final IPropertyType flaechPT = hydFT.getProperty( NaModelConstants.HYDRO_PROP_AREA );
      final IPropertyType fakVersPT = hydFT.getProperty( NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR );
      final Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = hydFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        if( !sourcekey.equalsIgnoreCase( NULL_KEY ) )
        {
          Object so = sourceFeature.getProperty( sourcekey );
          final IPropertyType pt = targetFeature.getFeatureType().getProperty( targetkey );

          if( so instanceof GM_MultiSurface )
          {
            targetFeature.setProperty( targetPT, so );
            final double area = GeometryUtilities.calcArea( (GM_Object) so );
            targetFeature.setProperty( flaechPT, new Double( area ) );
          }
          else if( so instanceof GM_Surface )
          {
            final GM_Surface surface = (GM_Surface) so;
            final GM_Surface[] surfaces = new GM_Surface[] { surface };
            final GM_MultiSurface MultiSurface = GeometryFactory.createGM_MultiSurface( surfaces, surface.getCoordinateSystem() );
            so = MultiSurface;
            targetFeature.setProperty( targetPT, so );
            final double area = GeometryUtilities.calcArea( (GM_Object) so );
            targetFeature.setProperty( flaechPT, new Double( area ) );
          }
          else if( pt instanceof IValuePropertyType )
          {
            final IValuePropertyType vpt = (IValuePropertyType) pt;
            if( so.getClass().equals( vpt.getTypeHandler().getValueClass() ) )
            {
              targetFeature.setProperty( targetkey, so );
            }
            else
            {
              try
              {
                targetFeature.setProperty( targetkey, SpecialPropertyMapper.map( so.getClass(), vpt.getTypeHandler().getValueClass(), so ) );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
              }
            }
          }
        }
      }
      hydList.add( targetFeature );
    }
  }

  private void copyResourcesToProject( final IPath path )
  {
    final String resource = m_resourceBase;
    // System.out.print( "resource: " + resource + "\n" );
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    try
    {
      ZipUtilities.unzip( resourceAsStream, path.toFile() );
    }
    catch( final Exception e )
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
   * and check if the ID doesn¥t exist in the idMap. if the id ColKey is not set, use the ID of the sourceFeature (shape
   * file).
   * 
   * @param idColKey
   * @param sourceFeature
   * @param IDText
   * @return fid
   */
  private String getId( final String idColKey, final Feature sourceFeature, final String IDText )
  {
    String fid;
    if( idColKey != null )
    {
      String idKey = null;
      try
      {
        idKey = SpecialPropertyMapper.map( (sourceFeature.getProperty( idColKey )).getClass(), Integer.class, sourceFeature.getProperty( idColKey ) ).toString();
      }
      catch( final Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      // String idKey = (sourceFeature.getProperty( idColKey )).toString();
      if( !m_IDMap.containsKey( IDText + idKey ) )
      {
        fid = IDText + idKey;
        m_IDMap.put( fid, sourceFeature );
      }
      else
      {
        System.out.println( Messages.getString("org.kalypso.wizard.KalypsoNAProjectWizard.11") + IDText + idKey + Messages.getString("org.kalypso.wizard.KalypsoNAProjectWizard.12") ); //$NON-NLS-1$ //$NON-NLS-2$
        fid = sourceFeature.getId();
        m_IDMap.put( fid, sourceFeature );
        System.out.println( Messages.getString("org.kalypso.wizard.KalypsoNAProjectWizard.13") + fid + Messages.getString("org.kalypso.wizard.KalypsoNAProjectWizard.14") ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    else
      fid = sourceFeature.getId();
    return fid;
  }

  public void mapCatchment( final List sourceFeatureList, final HashMap mapping )
  {
    final Feature rootFeature = m_modelWS.getRootFeature();
    final IFeatureType modelFT = getFeatureType( "Catchment" ); //$NON-NLS-1$
    final Feature catchmentCollectionFE = (Feature) rootFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    final FeatureList catchmentList = (FeatureList) catchmentCollectionFE.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    final IRelationType targetRelation = catchmentList.getParentFeatureTypeProperty();

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
      final Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final String fid = getId( idColKey, sourceFeature, "TG" );
      final Feature targetFeature = FeatureFactory.createFeature( catchmentCollectionFE, targetRelation, fid, modelFT, true );
      final IPropertyType flaechPT = modelFT.getProperty( NaModelConstants.NA_MODEL_FLAECH_PROP );
      final IRelationType bodenkorrekturMemberRT = (IRelationType) modelFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
      final Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
// final IPropertyType targetPT = modelFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        if( !sourcekey.equalsIgnoreCase( NULL_KEY ) )
        {
          final Object so = sourceFeature.getProperty( sourcekey );
          final IPropertyType pt = targetFeature.getFeatureType().getProperty( targetkey );
          if( so instanceof GM_MultiSurface )
          {
            final GM_Surface[] surfaces = new GM_Surface[] { ((GM_MultiSurface) so).getSurfaceAt( 0 ) };
            final Long area = new Long( (long) (surfaces[0]).getArea() );
            targetFeature.setProperty( flaechPT, area );
            targetFeature.setProperty( targetkey, surfaces[0] );
          }
          else if( pt instanceof IValuePropertyType )
          {
            final IValuePropertyType vpt = (IValuePropertyType) pt;
            if( so.getClass().equals( vpt.getTypeHandler().getValueClass() ) )
            {
              targetFeature.setProperty( targetkey, so );
            }
            else
            {
              try
              {
                targetFeature.setProperty( targetkey, SpecialPropertyMapper.map( so.getClass(), vpt.getTypeHandler().getValueClass(), so ) );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
              }
            }
          }
        }
      }
      // Bodenkorrekturparameter erstellen
      final List list = FeatureFactory.createFeatureList( targetFeature, bodenkorrekturMemberRT );
      targetFeature.setProperty( NaModelConstants.BODENKORREKTUR_MEMBER, list );
      final int soilLayerNo = Integer.parseInt( m_createPreferencePage.getSoilLayerNo() );
      for( int j = 0; j < soilLayerNo; j++ )
      {
        final IRelationType bodFtProp = (IRelationType) modelFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
        final IFeatureType bodenKorrekturFT = bodFtProp.getTargetFeatureType();
        final Feature newFeature = m_modelWS.createFeature( targetFeature, bodenkorrekturMemberRT, bodenKorrekturFT );
        try
        {
          m_modelWS.addFeatureAsComposition( targetFeature, bodenkorrekturMemberRT, j, newFeature );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
      catchmentList.add( targetFeature );
    }
  }

  public void mapNode( final List sourceFeatureList, final HashMap mapping )
  {

    final Feature rootFeature = m_modelWS.getRootFeature();
    final IFeatureType modelFT = getFeatureType( "Node" ); //$NON-NLS-1$
    final Feature nodeCollectionFE = (Feature) rootFeature.getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
    final FeatureList nodeList = (FeatureList) nodeCollectionFE.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    final IRelationType targetRelation = nodeList.getParentFeatureTypeProperty();

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
      final Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final String fid = getId( idColKey, sourceFeature, "K" );
      final Feature targetFeature = FeatureFactory.createFeature( nodeCollectionFE, targetRelation, fid, modelFT, true );
      final Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = modelFT.getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        if( !sourcekey.equalsIgnoreCase( NULL_KEY ) )
        {
          final Object so = sourceFeature.getProperty( sourcekey );
          final IPropertyType pt = targetFeature.getFeatureType().getProperty( targetkey );
          if( so instanceof GM_Object )
            targetFeature.setProperty( targetkey, so );
          else if( pt instanceof IValuePropertyType )
          {
            final IValuePropertyType vpt = (IValuePropertyType) pt;
            if( so.getClass().equals( vpt.getTypeHandler().getValueClass() ) )
            {
              targetFeature.setProperty( targetkey, so );
            }
            else
            {
              try
              {
                targetFeature.setProperty( targetkey, SpecialPropertyMapper.map( so.getClass(), vpt.getTypeHandler().getValueClass(), so ) );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
              }
            }
          }
        }
      }
      nodeList.add( targetFeature );
    }
  }

  public void mapRiver( final List sourceFeatureList, final HashMap mapping )
  {

    final Feature rootFeature = m_modelWS.getRootFeature();

    final Feature channelCollectionFE = (Feature) rootFeature.getProperty( NaModelConstants.CHANNEL_COLLECTION_MEMBER_PROP );
    final FeatureList channelList = (FeatureList) channelCollectionFE.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    final IRelationType targetRelation = channelList.getParentFeatureTypeProperty();

    // find column for id
    final String idColKey;
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
    {
      idColKey = (String) mapping.get( "name" ); //$NON-NLS-1$ 
    }
    else
      idColKey = null;

    // StrangArt is defined in dummyFeatureType (member variable)
    final String typeKey = (String) mapping.get( "StrangArt" ); //$NON-NLS-1$
    // remove the channel type mapping (just needed once)
    mapping.remove( typeKey );

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      final Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final Object o = sourceFeature.getProperty( typeKey );
      int channelType = 0;
      try
      {
        channelType = ((Integer) SpecialPropertyMapper.map( o.getClass(), Integer.class, o )).intValue();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new NumberFormatException( Messages.getString( "KalypsoNAProjectWizard.ExceptionStrangArt" ) ); //$NON-NLS-1$
      }

      Feature targetFeature = null;
      final String fid = getId( idColKey, sourceFeature, "S" );
      switch( channelType )
      {
        case 0:
        {

          final IFeatureType vFT = getFeatureType( "VirtualChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, targetRelation, fid, vFT, true );
          break;
        }
        case 1:
        {

          final IFeatureType kmFT = getFeatureType( "KMChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, targetRelation, fid, kmFT, true );

          final IRelationType parameterMemberRT = (IRelationType) kmFT.getProperty( NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER );
          final List list = FeatureFactory.createFeatureList( targetFeature, parameterMemberRT );
          targetFeature.setProperty( parameterMemberRT, list );
          final int channelNo = Integer.parseInt( m_createPreferencePage.getKMChannelNo() );
          for( int j = 0; j < channelNo; j++ )
          {
            final IFeatureType kmParameterFT = parameterMemberRT.getTargetFeatureType();
            final Feature newFeature = m_modelWS.createFeature( targetFeature, targetRelation, kmParameterFT );
            try
            {
              m_modelWS.addFeatureAsComposition( targetFeature, parameterMemberRT, j, newFeature );
            }
            catch( final Exception e )
            {
              e.printStackTrace();
            }
          }

          break;
        }
        case 2:
        {
          final IFeatureType storageFT = getFeatureType( "StorageChannel" ); //$NON-NLS-1$
          targetFeature = FeatureFactory.createFeature( channelCollectionFE, targetRelation, fid, storageFT, true );
          break;
        }
        case 3:
        {
          throw new NotImplementedException( Messages.getString( "KalypsoNAProjectWizard.ExceptionNotImplementedRHT" ) ); //$NON-NLS-1$
        }
        default:
        {
          break;
        }
      }// switch
      final Iterator it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final String sourcekey = (String) mapping.get( targetkey );
        if( "StrangArt".equals( targetkey ) )
          continue;
        if( !sourcekey.equalsIgnoreCase( NULL_KEY ) )
        {
          final Object so = sourceFeature.getProperty( sourcekey );
          final IPropertyType pt = targetFeature.getFeatureType().getProperty( targetkey );
          if( so instanceof GM_MultiCurve )
          {
            final GM_Curve[] curves = new GM_Curve[] { ((GM_MultiCurve) so).getCurveAt( 0 ) };
            targetFeature.setProperty( targetkey, curves[0] );
          }
// if( so instanceof GMLMultiLineString )
// targetFeature.setProperty( targetkey, so );
          else if( pt instanceof IValuePropertyType )
          {
            final IValuePropertyType vpt = (IValuePropertyType) pt;
            if( so.getClass().equals( vpt.getTypeHandler().getValueClass() ) )
            {
              targetFeature.setProperty( targetkey, so );
            }
            else
            {
              try
              {
                targetFeature.setProperty( targetkey, SpecialPropertyMapper.map( so.getClass(), vpt.getTypeHandler().getValueClass(), so ) );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
              }
            }
          }
        }
      }
      channelList.add( targetFeature );

    }// for i

  }// mapRiver

  public IGMLSchema getModelSchema( )
  {
    return m_modelSchema;
  }

  public boolean performCancle( )
  {
    try
    {
      m_projectHandel.delete( true, false, null );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }
}