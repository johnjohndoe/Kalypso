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
package org.kalypso.ui.rrm.internal.newproject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.model.hydrology.binding.model.channels.IVirtualChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class ImportRrmInitialDataOperation implements ICoreRunnableWithProgress
{
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

  private final Map<String, Feature> m_IDMap = new HashMap<>();

  private final RrmScenario m_scenario;

  private GMLWorkspace m_modelWS;

  private GMLWorkspace m_hydWS;

  private final int m_soilLayerNo;

  private final int m_channelNo;

  private final KalypsoNAMappingData m_catchmentMapping;

  private final KalypsoNAMappingData m_riverMapping;

  private final KalypsoNAMappingData m_nodeMapping;

  private final KalypsoNAMappingData m_hydrotopeMapping;

  public ImportRrmInitialDataOperation( final RrmScenario scenario, final int soilLayerNo, final int channelNo, final KalypsoNAMappingData catchmentMapping, final KalypsoNAMappingData riverMapping, final KalypsoNAMappingData nodeMapping, final KalypsoNAMappingData hydrotopeMapping )
  {
    m_scenario = scenario;
    m_soilLayerNo = soilLayerNo;
    m_channelNo = channelNo;

    m_catchmentMapping = catchmentMapping;
    m_riverMapping = riverMapping;
    m_nodeMapping = nodeMapping;
    m_hydrotopeMapping = hydrotopeMapping;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      readModels();

      // map catchment shape file
      final Map<IValuePropertyType, IValuePropertyType> catchmentMapping = m_catchmentMapping.getMapping();
      if( !MapUtils.isEmpty( catchmentMapping ) )
      {
        final List< ? > catchmentFeatureList = m_catchmentMapping.getSourceData();
        mapCatchment( catchmentFeatureList, catchmentMapping );
      }

      // map river shape file
      final Map<IValuePropertyType, IValuePropertyType> riverMapping = m_riverMapping.getMapping();
      if( !MapUtils.isEmpty( riverMapping ) )
      {
        final List< ? > riverFeatureList = m_riverMapping.getSourceData();
        mapRiver( riverFeatureList, riverMapping );
      }
      // map node shape file
      final Map<IValuePropertyType, IValuePropertyType> nodeMapping = m_nodeMapping.getMapping();
      if( !MapUtils.isEmpty( nodeMapping ) )
      {
        final List< ? > nodeFeatureList = m_nodeMapping.getSourceData();
        mapNode( nodeFeatureList, nodeMapping );
      }

      // map hydrotop shape file
      final Map<IValuePropertyType, IValuePropertyType> hydMapping = m_hydrotopeMapping.getMapping();
      if( !MapUtils.isEmpty( hydMapping ) )
      {
        final List< ? > hydFeatureList = m_hydrotopeMapping.getSourceData();
        mapHyd( hydFeatureList, hydMapping );
      }

      /* save changed workspace */
      GmlSerializer.serializeWorkspace( m_scenario.getModelFile(), m_modelWS, new NullProgressMonitor() );
      GmlSerializer.serializeWorkspace( m_scenario.getHydrotopGml(), m_hydWS, new NullProgressMonitor() );

      return Status.OK_STATUS;
    }
    finally
    {
      if( m_modelWS != null )
        m_modelWS.dispose();

      if( m_hydWS != null )
        m_hydWS.dispose();
    }
  }

  private void readModels( ) throws CoreException
  {
    try
    {
      m_modelWS = GmlSerializer.createGMLWorkspace( m_scenario.getModelFile() );
      m_hydWS = GmlSerializer.createGMLWorkspace( m_scenario.getHydrotopGml() );
    }
    catch( final Exception e1 )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "KalypsoNAProjectWizard_4" ), e1 ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private void mapHyd( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final HydrotopeCollection naHydrotop = (HydrotopeCollection)m_hydWS.getRootFeature();

    final IFeatureBindingCollection<IHydrotope> hydList = naHydrotop.getHydrotopes();

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature)sourceElement;
      final IHydrotope targetFeature = hydList.addNew( IHydrotope.FEATURE_HYDROTOPE, sourceFeature.getId() );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_SURFACE_2_MULTISURFACE );
    }
  }

  private void mapCatchment( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NaModell naModel = (NaModell)m_modelWS.getRootFeature();
    final IFeatureType catchmentFT = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT ); //$NON-NLS-1$

    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature)sourceElement;

      final String fid = getId( idColKey, sourceFeature, "TG" ); //$NON-NLS-1$

      final Catchment targetFeature = catchments.addNew( Catchment.FEATURE_CATCHMENT, fid );
      final IRelationType bodenkorrekturMemberRT = (IRelationType)catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_MULTISURFACE_2_SURFACE );

      /* Set area: TODO: probably not needed any more, check! */
      final GM_Polygon geometry = targetFeature.getGeometry();
      if( geometry != null )
        targetFeature.setProperty( NaModelConstants.NA_MODEL_FLAECH_PROP, new Long( (long)geometry.getArea() ) );

      // Bodenkorrekturparameter erstellen
      final List< ? > list = FeatureFactory.createFeatureList( targetFeature, bodenkorrekturMemberRT );
      targetFeature.setProperty( NaModelConstants.BODENKORREKTUR_MEMBER, list );

      for( int j = 0; j < m_soilLayerNo; j++ )
      {
        final IRelationType bodFtProp = (IRelationType)catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
        final IFeatureType bodenKorrekturFT = bodFtProp.getTargetFeatureType();
        final Feature soilCorection = m_modelWS.createFeature( targetFeature, bodenkorrekturMemberRT, bodenKorrekturFT );
        soilCorection.setDescription( Messages.getString( "KalypsoNAProjectWizard_10", j + 1 ) ); //$NON-NLS-1$

        try
        {
          m_modelWS.addFeatureAsComposition( targetFeature, bodenkorrekturMemberRT, j, soilCorection );
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
    final NaModell naModel = (NaModell)m_modelWS.getRootFeature();

    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature)sourceElement;
      final String fid = getId( idColKey, sourceFeature, "K" ); //$NON-NLS-1$
      final Feature targetFeature = nodes.addNew( INode.FEATURE_NODE, fid );

      copyValues( sourceFeature, targetFeature, mapping, GEO_MAPPING_NONE );
    }
  }

  private void mapRiver( final List< ? > sourceFeatureList, final Map<IValuePropertyType, IValuePropertyType> mapping )
  {
    final NaModell naModell = (NaModell)m_modelWS.getRootFeature();

    // find column for id
    final IValuePropertyType idColKey = findColumnForId( mapping );

    // StrangArt is defined in dummyFeatureType (member variable)
    final IValuePropertyType typeKey = findColumn( mapping, KalypsoNAProjectWizard.QNAME_STRANGART ); //$NON-NLS-1$
    // remove the channel type mapping (just needed once)
    removeColumn( mapping, KalypsoNAProjectWizard.QNAME_STRANGART );

    final IFeatureBindingCollection<Channel> channels = naModell.getChannels();

    for( final Object sourceElement : sourceFeatureList )
    {
      final Feature sourceFeature = (Feature)sourceElement;
      final Object o = sourceFeature.getProperty( typeKey );
      int channelType = 0;
      try
      {
        channelType = ((Integer)SpecialPropertyMapper.map( o.getClass(), Integer.class, o )).intValue();
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
        return channels.addNew( IVirtualChannel.FEATURE_VIRTUAL_CHANNEL, fid );

      case 1:
      {
        final KMChannel targetChannel = channels.addNew( KMChannel.FEATURE_KM_CHANNEL, fid, KMChannel.class );
        final IFeatureBindingCollection<KMParameter> parameters = targetChannel.getParameters();

        parameters.clear();

        for( int j = 0; j < m_channelNo; j++ )
          parameters.addNew( KMParameter.FEATURE_KM_PARAMETER );

        return targetChannel;
      }

      case 2:
        return channels.addNew( IStorageChannel.FEATURE_STORAGE_CHANNEL, fid );

      case 3:
        throw new UnsupportedOperationException( Messages.getString( "KalypsoNAProjectWizard.ExceptionNotImplementedRHT" ) ); //$NON-NLS-1$

      default:
        throw new IllegalArgumentException( String.format( Messages.getString( "KalypsoNAProjectWizard_7" ), channelType ) ); //$NON-NLS-1$
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
      return mapGeometry( (GM_Object)sourceValue, geoMappingOption );

    final IGMLSchema schema = targetFT.getGMLSchema();
    final String gmlVersion = schema.getGMLVersion();
    if( gmlVersion.startsWith( "3" ) && Feature.QN_NAME.equals( targetPT.getQName() ) ) //$NON-NLS-1$
    {
      final List<String> nameList = new ArrayList<>();
      nameList.add( ObjectUtils.toString( sourceValue, StringUtils.EMPTY ) );
      return nameList;
    }

    if( targetPT instanceof IValuePropertyType )
      return mapValuePropertyType( sourceValue, (IValuePropertyType)targetPT );

    return null;
  }

  private GM_Object mapGeometry( final GM_Object sourceValue, final int mappingOption )
  {
    if( (mappingOption & GEO_MAPPING_SURFACE_2_MULTISURFACE) != 0 && sourceValue instanceof GM_Polygon )
    {
      final GM_Polygon[] surfaces = new GM_Polygon[] { (GM_Polygon)sourceValue };
      return GeometryFactory.createGM_MultiSurface( surfaces, ((GM_Polygon)sourceValue).getCoordinateSystem() );
    }

    if( (mappingOption & GEO_MAPPING_MULTISURFACE_2_SURFACE) != 0 && sourceValue instanceof GM_MultiSurface )
    {
      final GM_Polygon[] surfaces = new GM_Polygon[] { ((GM_MultiSurface)sourceValue).getSurfaceAt( 0 ) };
      return surfaces[0];
    }

    if( (mappingOption & GEO_MAPPING_MULTICURVE_2_CURVE) != 0 && sourceValue instanceof GM_MultiCurve )
    {
      final GM_Curve[] curves = new GM_Curve[] { ((GM_MultiCurve)sourceValue).getCurveAt( 0 ) };
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
      final String msg = String.format( Messages.getString( "KalypsoNAProjectWizard_9" ), ObjectUtils.toString( sourceValue ), vpt.getQName(), vpt.getValueQName() ); //$NON-NLS-1$
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
   * and check if the ID doesn¥t exist in the idMap. if the idColKey is not set, use the ID of the sourceFeature (shape
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
      final String idKey = SpecialPropertyMapper.map( sourceFeature.getProperty( idColKey ).getClass(), Integer.class, sourceFeature.getProperty( idColKey ) ).toString();

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

}
