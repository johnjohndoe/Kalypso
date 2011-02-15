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

package org.kalypso.ui.rrm.wizards;

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;
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
import org.kalypso.contribs.eclipse.jface.wizard.ProjectTemplatePage;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
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
import org.kalypso.ui.rrm.extension.KalypsoModuleRRM;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GMLConstants;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends NewProjectWizard
{
  private static final QName QNAME_STRANGART = new QName( "wizard.kalypso.na", "StrangArt" ); //$NON-NLS-1$ //$NON-NLS-2$

  /** Mapping option for geometry properties: do nothing, geometry is not changed. */
  private static final int GEO_MAPPING_NONE = 0;

  /** Mapping option for geometry properties: wrap {@link GM_Surface}s into a {@link GM_MultiSurface}. */
  private final static int GEO_MAPPING_SURFACE_2_MULTISURFACE = 0x1 << 0;

  /**
   * Mapping option for geometry properties: convert {@link GM_MultiSurface}s into a {@link GM_Surface} by just taking
   * the first element.
   */
  private final static int GEO_MAPPING_MULTISURFACE_2_SURFACE = 0x1 << 1;

  /**
   * Mapping option for geometry properties: convert {@link GM_MultiCurve}s into a {@link GM_Curve} by just taking the
   * first element.
   */
  private static final int GEO_MAPPING_MULTICURVE_2_CURVE = 0x1 << 2;

  public static final String CATEGORY_TEMPLATE = "org.kalypso.model.rrm.templateProjects";//$NON-NLS-1$

  private static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  private static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  private static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  private static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  private static final String PREFERENCE_PAGE = "page_type:preferences"; //$NON-NLS-1$

  private final Map<String, Feature> m_IDMap = new HashMap<String, Feature>();

  private KalypsoNAProjectWizardPage m_createMappingCatchmentPage;

  private KalypsoNAProjectWizardPage m_createMappingHydrotopPage;

  private KalypsoNAProjectWizardPage m_createMappingNodePage;

  private KalypsoNAProjectWizardPage m_createMappingRiverPage;

  private KalypsoNAProjectPreferences m_createPreferencePage;

  private GMLWorkspace m_modelWS;

  private GMLWorkspace m_hydWS;

  public KalypsoNAProjectWizard( )
  {
    super( new ProjectTemplatePage( Messages.getString("KalypsoNAProjectWizard_2"), Messages.getString("KalypsoNAProjectWizard_3"), CATEGORY_TEMPLATE ), true, KalypsoModuleRRM.ID ); //$NON-NLS-1$ //$NON-NLS-2$

    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "KalypsoNAProjectWizard.9" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_createPreferencePage = new KalypsoNAProjectPreferences( PREFERENCE_PAGE );
    addPage( m_createPreferencePage );

    m_createMappingCatchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE, Messages.getString( "KalypsoNAProjectWizard.CatchmentPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getCatchmentTargetProperties() );

    addPage( m_createMappingCatchmentPage );
    m_createMappingRiverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, Messages.getString( "KalypsoNAProjectWizard.ChannelPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getChannelTargetProperties() );
    addPage( m_createMappingRiverPage );

    m_createMappingNodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, Messages.getString( "KalypsoNAProjectWizard.NodePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getNodeTargetProperties() ); //$NON-NLS-1$
    addPage( m_createMappingNodePage );
    m_createMappingHydrotopPage = new KalypsoNAProjectWizardPage( HYDROTOP_PAGE, Messages.getString( "KalypsoNAProjectWizard.HydrotopePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, getHydrotopeTargetProperties() ); //$NON-NLS-1$
    addPage( m_createMappingHydrotopPage );
  }

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#postCreateProject(org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus postCreateProject( final IProject project, final IProgressMonitor monitor ) throws CoreException
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
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("KalypsoNAProjectWizard_4"), e1 ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    // map catchment shape file
    final Map<IValuePropertyType, IValuePropertyType> catchmentMapping = m_createMappingCatchmentPage.getMapping();
    if( catchmentMapping != null && catchmentMapping.size() != 0 )
    {
      final List< ? > catchmentFeatureList = m_createMappingCatchmentPage.getFeatureList();
      mapCatchment( catchmentFeatureList, catchmentMapping );
    }

    // map river shape file
    final Map<IValuePropertyType, IValuePropertyType> riverMapping = m_createMappingRiverPage.getMapping();
    if( riverMapping != null && riverMapping.size() != 0 )
    {
      final List< ? > riverFeatureList = m_createMappingRiverPage.getFeatureList();
      mapRiver( riverFeatureList, riverMapping );
    }
    // map node shape file
    final Map<IValuePropertyType, IValuePropertyType> nodeMapping = m_createMappingNodePage.getMapping();
    if( nodeMapping != null && nodeMapping.size() != 0 )
    {
      final List< ? > nodeFeatureList = m_createMappingNodePage.getFeatureList();
      mapNode( nodeFeatureList, nodeMapping );
    }

    // map hydrotop shape file
    final Map<IValuePropertyType, IValuePropertyType> hydMapping = m_createMappingHydrotopPage.getMapping();
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
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("KalypsoNAProjectWizard_5"), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final GmlSerializeException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("KalypsoNAProjectWizard_6"), e ); //$NON-NLS-1$
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

  private void mapHyd( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NAHydrotop naHydrotop = (NAHydrotop) m_hydWS.getRootFeature();

    final IFeatureBindingCollection<IHydrotope> hydList = naHydrotop.getHydrotopes();

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature) sourceElement;
      final IHydrotope targetFeature = hydList.addNew( Hydrotop.QNAME, sourceFeature.getId() );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_SURFACE_2_MULTISURFACE );
    }
  }

  private void mapCatchment( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NaModell naModel = (NaModell) m_modelWS.getRootFeature();
    final IFeatureType catchmentFT = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT ); //$NON-NLS-1$

    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature) sourceElement;

      final String fid = getId( idColKey, sourceFeature, "TG" ); //$NON-NLS-1$

      final Catchment targetFeature = catchments.addNew( Catchment.FEATURE_CATCHMENT, fid );
      final IRelationType bodenkorrekturMemberRT = (IRelationType) catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_MULTISURFACE_2_SURFACE );

      /* Set area: TODO: probably not needed any more, check! */
      final GM_Surface< ? > geometry = targetFeature.getGeometry();
      if( geometry != null )
        targetFeature.setProperty( NaModelConstants.NA_MODEL_FLAECH_PROP, new Long( (long) geometry.getArea() ) );

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
    }
  }

  private void mapNode( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NaModell naModel = (NaModell) m_modelWS.getRootFeature();

    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature) sourceElement;
      final String fid = getId( idColKey, sourceFeature, "K" ); //$NON-NLS-1$
      final Feature targetFeature = nodes.addNew( Node.FEATURE_NODE, fid );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_NONE );
    }
  }

  private void mapRiver( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NaModell naModell = (NaModell) m_modelWS.getRootFeature();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    // StrangArt is defined in dummyFeatureType (member variable)
    final IValuePropertyType typeKey = findColumn( mapping, QNAME_STRANGART ); //$NON-NLS-1$
    // remove the channel type mapping (just needed once)
    removeColumn( mapping, QNAME_STRANGART );

    final IFeatureBindingCollection<Channel> channels = naModell.getChannels();

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature) sourceElement;
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

      final String fid = getId( idColKey, sourceFeature, "S" ); //$NON-NLS-1$

      final Feature targetFeature = createFeatureForChannelType( channelType, channels, fid );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_MULTICURVE_2_CURVE );
    }
  }

  private Channel createFeatureForChannelType( final int channelType, final IFeatureBindingCollection<Channel> channels, final String fid )
  {
    switch( channelType )
    {
      case 0:
        return channels.addNew( VirtualChannel.FEATURE_VIRTUAL_CHANNEL, fid );

      case 1:
      {
        final KMChannel targetChannel = channels.addNew( KMChannel.FEATURE_KM_CHANNEL, fid, KMChannel.class );
        final IFeatureBindingCollection<KMParameter> parameters = targetChannel.getParameters();
        parameters.clear();
        final int channelNo = Integer.parseInt( m_createPreferencePage.getKMChannelNo() );
        for( int j = 0; j < channelNo; j++ )
          parameters.addNew( KMParameter.FEATURE_KM_PARAMETER );

        return targetChannel;
      }

      case 2:
        return channels.addNew( StorageChannel.FEATURE_STORAGE_CHANNEL, fid );

      case 3:
        throw new NotImplementedException( Messages.getString( "KalypsoNAProjectWizard.ExceptionNotImplementedRHT" ) ); //$NON-NLS-1$

      default:
        throw new IllegalArgumentException( String.format( Messages.getString("KalypsoNAProjectWizard_7"), channelType ) ); //$NON-NLS-1$
    }
  }

  private void copyValues( final Feature sourceFeature, final Feature targetFeature, final Map<IValuePropertyType, IValuePropertyType> mapping, final int geoMappingOption )
  {
    for( final Entry<IValuePropertyType, IValuePropertyType> entry : mapping.entrySet() )
    {
      final IValuePropertyType targetkey = entry.getKey();
      final IValuePropertyType sourcekey = entry.getValue();

      copyValue( sourceFeature, sourcekey, targetFeature, targetkey, geoMappingOption );
    }
  }

  private void copyValue( final Feature sourceFeature, final IValuePropertyType sourcekey, final Feature targetFeature, final IValuePropertyType targetkey, final int geoMappingOption )
  {
    final IFeatureType targetFT = targetFeature.getFeatureType();
    final Object sourceValue = sourceFeature.getProperty( sourcekey );
    final Object targetValue = mapValue( targetFT, sourceValue, targetkey, geoMappingOption );
    targetFeature.setProperty( targetkey, targetValue );
  }

  private Object mapValue( final IFeatureType targetFT, final Object sourceValue, final IPropertyType targetPT, final int geoMappingOption )
  {
    if( sourceValue instanceof GM_Object )
      return mapGeometry( (GM_Object) sourceValue, geoMappingOption );

    final IGMLSchema schema = targetFT.getGMLSchema();
    final String gmlVersion = schema.getGMLVersion();
    if( gmlVersion.startsWith( "3" ) && Feature.QN_NAME.equals( targetPT.getQName() ) ) //$NON-NLS-1$
    {
      final List<String> nameList = new ArrayList<String>();
      nameList.add( ObjectUtils.toString( sourceValue, StringUtils.EMPTY ) );
      return nameList;
    }

    if( targetPT instanceof IValuePropertyType )
      return mapValuePropertyType( sourceValue, (IValuePropertyType) targetPT );

    return null;
  }

  private GM_Object mapGeometry( final GM_Object sourceValue, final int mappingOption )
  {
    if( (mappingOption & GEO_MAPPING_SURFACE_2_MULTISURFACE) != 0 && sourceValue instanceof GM_Surface )
    {
      final GM_Surface< ? >[] surfaces = new GM_Surface[] { (GM_Surface< ? >) ((GM_Surface< ? >) sourceValue) };
      return GeometryFactory.createGM_MultiSurface( surfaces, ((GM_Surface< ? >) ((GM_Surface< ? >) sourceValue)).getCoordinateSystem() );
    }

    if( (mappingOption & GEO_MAPPING_MULTISURFACE_2_SURFACE) != 0 && sourceValue instanceof GM_MultiSurface )
    {
      final GM_Surface< ? >[] surfaces = new GM_Surface[] { ((GM_MultiSurface) sourceValue).getSurfaceAt( 0 ) };
      return surfaces[0];
    }

    if( (mappingOption & GEO_MAPPING_MULTICURVE_2_CURVE) != 0 && sourceValue instanceof GM_MultiCurve )
    {
      final GM_Curve[] curves = new GM_Curve[] { ((GM_MultiCurve) sourceValue).getCurveAt( 0 ) };
      return curves[0];
    }


    return sourceValue;
  }

  private Object mapValuePropertyType( final Object sourceValue, final IValuePropertyType vpt )
  {
    if( sourceValue.getClass().equals( vpt.getTypeHandler().getValueClass() ) )
      return sourceValue;

    try
    {
      return SpecialPropertyMapper.map( sourceValue.getClass(), vpt.getTypeHandler().getValueClass(), sourceValue );
    }
    catch( final Exception e )
    {
      // we do not print the stack trace!
      final String msg = String.format( Messages.getString("KalypsoNAProjectWizard_9"), ObjectUtils.toString( sourceValue ), vpt.getQName(), vpt.getValueQName() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
      KalypsoUIRRMPlugin.getDefault().getLog().log( status );
    }

    return null;

  }

  private static IValuePropertyType findColumnForId( final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    return findColumn( mapping, Feature.QN_NAME );
  }

  private static IValuePropertyType findColumn( final Map<IValuePropertyType, IValuePropertyType> mapping, final QName qname )
  {
    for( final IValuePropertyType targetKey : mapping.keySet() )
    {
      if( qname.equals( targetKey.getQName() ) )
        return mapping.get( targetKey );
    }

    return null;
  }

  private static void removeColumn( final Map<IValuePropertyType, IValuePropertyType> mapping, final QName qname )
  {
    for( final IValuePropertyType targetKey : mapping.keySet() )
    {
      if( qname.equals( targetKey.getQName() ) )
      {
        mapping.remove( targetKey );
        return;
      }
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
  private String getId( final IValuePropertyType idColKey, final Feature sourceFeature, final String idText )
  {
    if( idColKey == null )
      return sourceFeature.getId();

    try
    {
      final String idKey = SpecialPropertyMapper.map( (sourceFeature.getProperty( idColKey )).getClass(), Integer.class, sourceFeature.getProperty( idColKey ) ).toString();

      final String key = idText + idKey;
      if( !m_IDMap.containsKey( key ) )
      {
        m_IDMap.put( key, sourceFeature );
        return key;
      }
      else
      {
        final String fid = sourceFeature.getId();
        m_IDMap.put( fid, sourceFeature );
        return fid;
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return sourceFeature.getId();
  }


  private IValuePropertyType[] getAllValueProperties( final IFeatureType targetFT )
  {
    final Collection<IValuePropertyType> result = new ArrayList<IValuePropertyType>();

    final IPropertyType[] targetFtp = targetFT.getProperties();
    for( final IPropertyType ftp : targetFtp )
    {
      final QName qName = ftp.getQName();
      if( qName.equals( GMLConstants.QN_LOCATION ) || qName.equals( Feature.QN_BOUNDED_BY ) )
        continue;

      if( ftp instanceof IValuePropertyType )
      {
        final IValuePropertyType targetPT = (IValuePropertyType) ftp;
        if( !targetPT.isVirtual() && !targetPT.isList() )
          result.add( targetPT );
      }
    }

    return result.toArray( new IValuePropertyType[result.size()] );
  }

  private IValuePropertyType[] getCatchmentTargetProperties( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT );
    return getAllValueProperties( featureType );
  }

  private IValuePropertyType[] getChannelTargetProperties( )
  {
    final IFeatureType channelFT = GMLSchemaUtilities.getFeatureTypeQuiet( Channel.FEATURE_CHANNEL );

    final IValuePropertyType propOrt = (IValuePropertyType) channelFT.getProperty( Channel.PROP_ORT );
    final IValuePropertyType propName = (IValuePropertyType) channelFT.getProperty( Channel.QN_NAME );
    final IValuePropertyType propDescription = (IValuePropertyType) channelFT.getProperty( Channel.QN_DESCRIPTION );

    // Create fake property type for 'channel type'
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
    final IAnnotation annoStrangArt = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.7" ) ); //$NON-NLS-1$
    final IValuePropertyType propStrangArt = GMLSchemaFactory.createValuePropertyType( QNAME_STRANGART, integerTH, 0, 1, false, annoStrangArt ); //$NON-NLS-1$ //$NON-NLS-2$

    return new IValuePropertyType[] { propOrt, propName, propDescription, propStrangArt };
  }

  private IValuePropertyType[] getNodeTargetProperties( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( Node.FEATURE_NODE );
    return getAllValueProperties( featureType );
  }

  private IValuePropertyType[] getHydrotopeTargetProperties( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( IHydrotope.QNAME );
    return getAllValueProperties( featureType );
  }

}
