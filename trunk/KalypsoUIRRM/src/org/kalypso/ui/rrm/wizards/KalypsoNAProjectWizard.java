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
package org.kalypso.ui.rrm.wizards;

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.wizards.NewProjectWizard;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.annotation.DefaultAnnotation;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.Hydrotop;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends NewProjectWizard
{
  public static final String CATEGORY_TEMPLATE = "org.kalypso.model.rrm.templateProjects";//$NON-NLS-1$

  public static final String NULL_KEY = "-NULL-"; //$NON-NLS-1$

  static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  static final String PREFERENCE_PAGE = "page_type:preferences"; //$NON-NLS-1$

  final HashMap<String, Feature> m_IDMap = new HashMap<String, Feature>();

  private KalypsoNAProjectWizardPage m_createMappingCatchmentPage;

  private KalypsoNAProjectWizardPage m_createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage m_createMappingNodePage;

  private KalypsoNAProjectWizardPage m_createMappingRiverPage;

  private KalypsoNAProjectPreferences m_createPreferencePage;

  private final GMLSchema m_modelSchema;

  private GMLWorkspace m_modelWS;

  private GMLSchema m_hydrotopSchema;

  private GMLWorkspace m_hydWS;

  public KalypsoNAProjectWizard( )
  {
    super( CATEGORY_TEMPLATE, true );

    GMLSchema schema = null;
    try
    {
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      schema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
      m_hydrotopSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAHYDROTOP, (String) null );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }
    m_modelSchema = schema;

    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "KalypsoNAProjectWizard.9" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    super.addPages();

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

  private IFeatureType createGewaesserFT( )
  {
    // TODO: set annotations to CustomPropertyTypes in order to translate this stuff
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final QName featureQName = new QName( "wizard.kalypso.na", "Gew‰sser" ); //$NON-NLS-1$ //$NON-NLS-2$

    final IMarshallingTypeHandler lineStringTH = registry.getTypeHandlerForClassName( GeometryUtilities.getLineStringClass() );
    final IAnnotation annoOrt = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.1" ) ); //$NON-NLS-1$
    final IPropertyType pt1 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "Ort" ), lineStringTH, 0, 1, false, annoOrt ); //$NON-NLS-1$ //$NON-NLS-2$

    final IMarshallingTypeHandler stringTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );

    final IAnnotation annoName = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.3" ) ); //$NON-NLS-1$
    final IPropertyType pt2 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "name" ), stringTH, 1, 1, false, annoName ); //$NON-NLS-1$ //$NON-NLS-2$
    final IAnnotation annoDescription = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.5" ) ); //$NON-NLS-1$
    final IPropertyType pt3 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "description" ), stringTH, 1, 1, false, annoDescription ); //$NON-NLS-1$ //$NON-NLS-2$

    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
    final IAnnotation annoStrangArt = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.7" ) ); //$NON-NLS-1$
    final IPropertyType pt4 = GMLSchemaFactory.createValuePropertyType( new QName( "wizard.kalypso.na", "StrangArt" ), integerTH, 0, 1, false, annoStrangArt ); //$NON-NLS-1$ //$NON-NLS-2$
    final IPropertyType[] pts = new IPropertyType[] { pt1, pt2, pt3, pt4 };

    return GMLSchemaFactory.createFeatureType( featureQName, pts );
  }

  @Deprecated
  private IFeatureType getFeatureType( final String featureName )
  {
    IFeatureType ft = null;
    ft = m_modelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, featureName ) );
    if( ft == null )
      ft = m_hydrotopSchema.getFeatureType( new QName( NaModelConstants.NS_NAHYDROTOP, featureName ) );
    return ft;
  }

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#postCreateProject(org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void postCreateProject( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    final IPath modelPath = project.getLocation().append( "/modell.gml" ); //$NON-NLS-1$
    final IPath hydPath = project.getLocation().append( "/hydrotop.gml" ); //$NON-NLS-1$

    try
    {
      // open modell.gml and hydrotop.gml file to write imported feature
      m_modelWS = GmlSerializer.createGMLWorkspace( modelPath.toFile(), null );
      m_hydWS = GmlSerializer.createGMLWorkspace( hydPath.toFile(), null );
    }
    catch( final Exception e1 )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to create model files", e1 );
      throw new CoreException( status );
    }

    // map catchment shape file
    final Map<Object, Object> catchmentMapping = m_createMappingCatchmentPage.getMapping();
    if( catchmentMapping != null && catchmentMapping.size() != 0 )
    {
      final List< ? > catchmentFeatureList = m_createMappingCatchmentPage.getFeatureList();
      mapCatchment( catchmentFeatureList, catchmentMapping );
    }
    // map river shape file
    final Map<Object, Object> riverMapping = m_createMappingRiverPage.getMapping();
    if( riverMapping != null && riverMapping.size() != 0 )
    {
      final List< ? > riverFeatureList = m_createMappingRiverPage.getFeatureList();
      mapRiver( riverFeatureList, riverMapping );
    }
    // map node shape file
    final Map<Object, Object> nodeMapping = m_createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      final List< ? > nodeFeatureList = m_createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }

    // map hydrotop shape file
    final Map<Object, Object> hydMapping = m_createMappingHydrotopPage.getMapping();
    if( hydMapping != null && hydMapping.size() != 0 )
    {
      final List< ? > hydFeatureList = m_createMappingHydrotopPage.getFeatureList();
      mapHyd( hydFeatureList, hydMapping );
    }

    // write all new imported features to the modell.gml and hydrotop.gml file
    // in the workspace model.gml
    try
    {
      final OutputStreamWriter modelWriter = new FileWriter( modelPath.toFile() );
      GmlSerializer.serializeWorkspace( modelWriter, m_modelWS );
      modelWriter.close();
      // hydrotop.gml
      final OutputStreamWriter hydrotopWriter = new FileWriter( hydPath.toFile() );
      GmlSerializer.serializeWorkspace( hydrotopWriter, m_hydWS );
      hydrotopWriter.close();

      project.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to write model files", e );
      throw new CoreException( status );
    }
    catch( final GmlSerializeException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to write model files", e );
      throw new CoreException( status );
    }
    finally
    {
      m_createMappingCatchmentPage.dispose();
      m_createMappingRiverPage.dispose();
      m_createMappingNodePage.dispose();
      m_createMappingHydrotopPage.dispose();
    }
  }

  private void mapHyd( final List< ? > sourceFeatureList, final Map<Object, Object> mapping )
  {
    final NAHydrotop naHydrotop = (NAHydrotop) m_hydWS.getRootFeature();

    final IFeatureBindingCollection<IHydrotope> hydList = naHydrotop.getHydrotopes();

    for( int i = 0; i < sourceFeatureList.size(); i++ )
    {
      final Feature sourceFeature = (Feature) sourceFeatureList.get( i );
      final IHydrotope targetFeature = hydList.addNew( Hydrotop.QNAME, sourceFeature.getId() );
      final Iterator<Object> it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final IPropertyType targetPT = targetFeature.getFeatureType().getProperty( targetkey );
        final String sourcekey = (String) mapping.get( targetkey );
        if( !sourcekey.equalsIgnoreCase( NULL_KEY ) )
        {
          Object so = sourceFeature.getProperty( sourcekey );
          final IPropertyType pt = targetFeature.getFeatureType().getProperty( targetkey );

          if( so instanceof GM_MultiSurface )
          {
            targetFeature.setProperty( targetPT, so );
          }
          else if( so instanceof GM_Surface )
          {
            final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) so;
            final GM_Surface<GM_SurfacePatch>[] surfaces = new GM_Surface[] { surface };
            final GM_MultiSurface MultiSurface = GeometryFactory.createGM_MultiSurface( surfaces, surface.getCoordinateSystem() );
            so = MultiSurface;
            targetFeature.setProperty( targetPT, so );
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
        fid = sourceFeature.getId();
        m_IDMap.put( fid, sourceFeature );
      }
    }
    else
      fid = sourceFeature.getId();
    return fid;
  }

  private void mapCatchment( final List< ? > sourceFeatureList, final Map<Object, Object> mapping )
  {
    final NaModell naModel = (NaModell) m_modelWS.getRootFeature();
    final IFeatureType catchmentFT = getFeatureType( "Catchment" ); //$NON-NLS-1$

    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();

// final IRelationType targetRelation = catchments.getParentFeatureTypeProperty();

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
      final String fid = getId( idColKey, sourceFeature, "TG" ); //$NON-NLS-1$

// final Feature targetFeature = FeatureFactory.createFeature( catchmentCollectionFE, targetRelation, fid, catchmentFT,
// true );

      final Catchment targetFeature = catchments.addNew( Catchment.FEATURE_CATCHMENT, fid );

      final IPropertyType flaechPT = catchmentFT.getProperty( NaModelConstants.NA_MODEL_FLAECH_PROP );
      final IRelationType bodenkorrekturMemberRT = (IRelationType) catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
      final Iterator<Object> it = mapping.keySet().iterator();
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
            final GM_Surface< ? >[] surfaces = new GM_Surface[] { ((GM_MultiSurface) so).getSurfaceAt( 0 ) };
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
      final List< ? > list = FeatureFactory.createFeatureList( targetFeature, bodenkorrekturMemberRT );
      targetFeature.setProperty( NaModelConstants.BODENKORREKTUR_MEMBER, list );
      final int soilLayerNo = Integer.parseInt( m_createPreferencePage.getSoilLayerNo() );
      for( int j = 0; j < soilLayerNo; j++ )
      {
        final IRelationType bodFtProp = (IRelationType) catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
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
// catchmentList.add( targetFeature );
    }
  }

  private void mapNode( final List< ? > sourceFeatureList, final Map<Object, Object> mapping )
  {
    final NaModell naModel = (NaModell) m_modelWS.getRootFeature();
//    final IFeatureType modelFT = getFeatureType( "Node" ); //$NON-NLS-1$

// final Feature nodeCollectionFE = (Feature) rootFeature.getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
// final FeatureList nodeList = (FeatureList) nodeCollectionFE.getProperty( NaModelConstants.NODE_MEMBER_PROP );

    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();

// final IRelationType targetRelation = nodeList.getParentFeatureTypeProperty();

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
      final String fid = getId( idColKey, sourceFeature, "K" ); //$NON-NLS-1$
      final Feature targetFeature = nodes.addNew( Node.FEATURE_NODE, fid );

      for( final Entry<Object, Object> entry : mapping.entrySet() )
      {
        final String targetkey = (String) entry.getKey();
        final String sourcekey = (String) entry.getValue();
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
    }
  }

  private void mapRiver( final List< ? > sourceFeatureList, final Map<Object, Object> mapping )
  {
    final NaModell naModell = (NaModell) m_modelWS.getRootFeature();

    // find column for id
    final String idColKey = findColumnForId( mapping );

    // StrangArt is defined in dummyFeatureType (member variable)
    final String typeKey = (String) mapping.get( "StrangArt" ); //$NON-NLS-1$
    // remove the channel type mapping (just needed once)
    mapping.remove( typeKey );

    final IFeatureBindingCollection<Channel> channels = naModell.getChannels();

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
      final String fid = getId( idColKey, sourceFeature, "S" ); //$NON-NLS-1$
      switch( channelType )
      {
        case 0:
        {
          targetFeature = channels.addNew( VirtualChannel.FEATURE_VIRTUAL_CHANNEL, fid );
          break;
        }
        case 1:
        {
          final IFeatureType kmFT = getFeatureType( "KMChannel" ); //$NON-NLS-1$
          targetFeature = channels.addNew( KMChannel.FEATURE_KM_CHANNEL, fid );

          final KMChannel targetChannel = (KMChannel) targetFeature;
          final IFeatureBindingCollection<KMParameter> parameters = targetChannel.getParameters();
          parameters.clear();
          final int channelNo = Integer.parseInt( m_createPreferencePage.getKMChannelNo() );
          for( int j = 0; j < channelNo; j++ )
            parameters.addNew( KMParameter.FEATURE_KM_PARAMETER );
          break;
        }
        case 2:
        {
          targetFeature = channels.addNew( StorageChannel.FEATURE_STORAGE_CHANNEL, fid );
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

      final Iterator<Object> it = mapping.keySet().iterator();
      while( it.hasNext() )
      {
        final String targetkey = (String) it.next();
        final String sourcekey = (String) mapping.get( targetkey );
        if( "StrangArt".equals( targetkey ) ) //$NON-NLS-1$
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
    }// for i

  }// mapRiver

  private String findColumnForId( final Map<Object, Object> mapping )
  {
    if( mapping.containsKey( "name" ) ) //$NON-NLS-1$
      return (String) mapping.get( "name" ); //$NON-NLS-1$

    return null;
  }
}