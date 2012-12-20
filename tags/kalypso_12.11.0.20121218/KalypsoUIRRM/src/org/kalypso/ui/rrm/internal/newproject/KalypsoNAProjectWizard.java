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
import java.util.Collection;
import java.util.Locale;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.afgui.wizards.NewProjectWizard;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.jface.wizard.ProjectTemplatePage;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.DefaultAnnotation;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.IChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.shape.ShapeType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.rrm.KalypsoModuleRRM;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.welcome.KalypsoRrmNewProjectHandler;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizard extends NewProjectWizard
{
  static final QName QNAME_STRANGART = new QName( "wizard.kalypso.na", "StrangArt" ); //$NON-NLS-1$ //$NON-NLS-2$

  public static final String CATEGORY_TEMPLATE = "org.kalypso.model.rrm.templateProjects";//$NON-NLS-1$

  private static final String CATCHMENT_PAGE = "page_type:catchment"; //$NON-NLS-1$

  private static final String HYDROTOP_PAGE = "page_type:hydrotop"; //$NON-NLS-1$

  private static final String NODE_PAGE = "page_type:node"; //$NON-NLS-1$

  private static final String RIVER_PAGE = "page_type:river"; //$NON-NLS-1$

  private static final String PREFERENCE_PAGE = "page_type:preferences"; //$NON-NLS-1$

  private KalypsoNAProjectPreferences m_preferencePage;

  private KalypsoNAMappingData m_catchmentMapping;

  private KalypsoNAMappingData m_channelMapping;

  private KalypsoNAMappingData m_nodeMapping;

  private KalypsoNAMappingData m_hydrotopeMapping;

  public KalypsoNAProjectWizard( )
  {
    super( new ProjectTemplatePage( Messages.getString( "KalypsoNAProjectWizard_2" ), Messages.getString( "KalypsoNAProjectWizard_3" ), CATEGORY_TEMPLATE ), false, KalypsoModuleRRM.ID ); //$NON-NLS-1$ //$NON-NLS-2$

    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "KalypsoNAProjectWizard.9" ) ); //$NON-NLS-1$

    final ProjectTemplatePage templatePage = getTemplatePage();

    /**
     * Automatically choose language by current settings. It is not so important any more.
     */
    final String language = Locale.getDefault().getLanguage();
    templatePage.selectTemplate( language );
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_preferencePage = new KalypsoNAProjectPreferences( PREFERENCE_PAGE );
    addPage( m_preferencePage );

    m_catchmentMapping = getCatchmentTargetData();
    final KalypsoNAProjectWizardPage catchmentPage = new KalypsoNAProjectWizardPage( CATCHMENT_PAGE, Messages.getString( "KalypsoNAProjectWizard.CatchmentPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, m_catchmentMapping );
    addPage( catchmentPage );

    m_channelMapping = getChannelTargetData();
    final KalypsoNAProjectWizardPage riverPage = new KalypsoNAProjectWizardPage( RIVER_PAGE, Messages.getString( "KalypsoNAProjectWizard.ChannelPageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, m_channelMapping );
    addPage( riverPage );

    m_nodeMapping = getNodeTargetProperties();
    final KalypsoNAProjectWizardPage nodePage = new KalypsoNAProjectWizardPage( NODE_PAGE, Messages.getString( "KalypsoNAProjectWizard.NodePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, m_nodeMapping ); //$NON-NLS-1$
    addPage( nodePage );

    m_hydrotopeMapping = getHydrotopeTargetProperties();
    final KalypsoNAProjectWizardPage hydrotopPage = new KalypsoNAProjectWizardPage( HYDROTOP_PAGE, Messages.getString( "KalypsoNAProjectWizard.HydrotopePageTitle" ), //$NON-NLS-1$
        ImageProvider.IMAGE_KALYPSO_ICON_BIG, m_hydrotopeMapping ); //$NON-NLS-1$
    addPage( hydrotopPage );
  }

  @Override
  public IStatus postCreateProject( final IProject project, final ProjectTemplate template, final IProgressMonitor monitor ) throws CoreException
  {
    final IStatus postCreateStatus = new KalypsoRrmNewProjectHandler().postCreateProject( project, template, new NullProgressMonitor() );
    // FIXME: better handling of bad postCreate status
    if( postCreateStatus.matches( IStatus.ERROR ) )
      return postCreateStatus;

    final RrmProject rrmProject = new RrmProject( project );
    final RrmScenario baseScenario = rrmProject.getBaseScenario();

    final int soilLayerNo = m_preferencePage.getSoilLayerNo();
    final int kmChannelNo = m_preferencePage.getKMChannelNo();

    final ImportRrmInitialDataOperation importHandler = new ImportRrmInitialDataOperation( baseScenario, soilLayerNo, kmChannelNo, m_catchmentMapping, m_channelMapping, m_nodeMapping, m_hydrotopeMapping );

    return importHandler.execute( new NullProgressMonitor() );
  }

  private IValuePropertyType[] getAllValueProperties( final IFeatureType targetFT )
  {
    final Collection<IValuePropertyType> result = new ArrayList<>();

    final IPropertyType[] targetFtp = targetFT.getProperties();
    for( final IPropertyType ftp : targetFtp )
    {
      final QName qName = ftp.getQName();
      if( qName.equals( Feature.QN_LOCATION ) || qName.equals( Feature.QN_BOUNDED_BY ) )
        continue;

      if( ftp instanceof IValuePropertyType )
      {
        final IValuePropertyType targetPT = (IValuePropertyType) ftp;

        final boolean isValid = isSuitableImportProperty( targetPT );

        if( isValid )
          result.add( targetPT );
      }
    }

    return result.toArray( new IValuePropertyType[result.size()] );
  }

  private boolean isSuitableImportProperty( final IValuePropertyType targetPT )
  {
    final QName qName = targetPT.getQName();

    if( Feature.QN_NAME.equals( qName ) )
      return true;

    if( targetPT.isVirtual() )
      return false;

    if( targetPT.isList() )
      return false;

    if( targetPT.isGeometry() )
      return false;

    final Class< ? > valueClass = targetPT.getValueClass();
    if( TimeseriesLinkType.class == valueClass )
      return false;

    if( IObservation.class == valueClass )
      return false;

    return true;
  }

  private KalypsoNAMappingData getCatchmentTargetData( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT );

    final IValuePropertyType[] allValueProperties = getAllValueProperties( featureType );

    return new KalypsoNAMappingData( new ShapeType[] { ShapeType.POLYGON, ShapeType.POLYGONZ }, featureType, allValueProperties );
  }

  private KalypsoNAMappingData getChannelTargetData( )
  {
    final IFeatureType channelFT = GMLSchemaUtilities.getFeatureTypeQuiet( IChannel.FEATURE_CHANNEL );

    final IValuePropertyType propName = (IValuePropertyType) channelFT.getProperty( Feature.QN_NAME );
    final IValuePropertyType propDescription = (IValuePropertyType) channelFT.getProperty( Feature.QN_DESCRIPTION );

    // Create fake property type for 'channel type'
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
    final IAnnotation annoStrangArt = new DefaultAnnotation( Locale.getDefault().getLanguage(), Messages.getString( "KalypsoNAProjectWizard.7" ) ); //$NON-NLS-1$
    final IValuePropertyType propStrangArt = GMLSchemaFactory.createValuePropertyType( QNAME_STRANGART, integerTH, 0, 1, false, annoStrangArt ); //$NON-NLS-1$ //$NON-NLS-2$

    final IValuePropertyType[] allValueProperties = new IValuePropertyType[] { propName, propDescription, propStrangArt };
    return new KalypsoNAMappingData( new ShapeType[] { ShapeType.POLYLINE, ShapeType.POLYLINEZ }, channelFT, allValueProperties );
  }

  private KalypsoNAMappingData getNodeTargetProperties( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( INode.FEATURE_NODE );

    final IValuePropertyType[] allValueProperties = getAllValueProperties( featureType );

    return new KalypsoNAMappingData( new ShapeType[] { ShapeType.POINT, ShapeType.POINTZ }, featureType, allValueProperties );
  }

  private KalypsoNAMappingData getHydrotopeTargetProperties( )
  {
    final IFeatureType hydrotopeFT = GMLSchemaUtilities.getFeatureTypeQuiet( IHydrotope.FEATURE_HYDROTOPE );

    final IValuePropertyType propName = (IValuePropertyType) hydrotopeFT.getProperty( Feature.QN_NAME );

    final IValuePropertyType propLanduse = (IValuePropertyType) hydrotopeFT.getProperty( IHydrotope.PROPERTY_LANDUSE );
    final IValuePropertyType propSoiltype = (IValuePropertyType) hydrotopeFT.getProperty( IHydrotope.PROPERTY_SOILTYPE );
    final IValuePropertyType propCorrSealing = (IValuePropertyType) hydrotopeFT.getProperty( IHydrotope.PROPERTY_CORR_SEALING );
    final IValuePropertyType propMaxPerk = (IValuePropertyType) hydrotopeFT.getProperty( IHydrotope.PROPERTY_MAX_PERCOLATION );
    final IValuePropertyType propMF1GWS = (IValuePropertyType) hydrotopeFT.getProperty( IHydrotope.PROPERTY_GW_INFLOW_RATE );

    final IValuePropertyType[] ftps = new IValuePropertyType[] { propName, propLanduse, propSoiltype, propCorrSealing, propMaxPerk, propMF1GWS };

    return new KalypsoNAMappingData( new ShapeType[] { ShapeType.POLYGON, ShapeType.POLYGONZ }, hydrotopeFT, ftps );
  }
}