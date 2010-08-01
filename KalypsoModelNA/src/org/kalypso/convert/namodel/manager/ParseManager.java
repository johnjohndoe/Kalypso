/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileFilter;
import java.net.URL;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class ParseManager
{
  private final GMLSchema m_paraSchema;

  private final NAConfiguration m_conf;

  private final BodenartManager m_bodartManager;

  private final BodentypManager m_bodtypManager;

  private final NutzungManager m_nutzManager;

  private final SchneeManager m_schneeManager;

  private final IdleLanduseManager m_idleLanduseManager;

  public ParseManager( final GMLSchema paraSchema, final NAConfiguration conf, final BodenartManager bodartManager, final BodentypManager bodtypManager, final NutzungManager nutzManager, final SchneeManager schneeManager, final IdleLanduseManager idleLanduseManager )
  {
    m_conf = conf;
    m_paraSchema = paraSchema;
    m_bodartManager = bodartManager;
    m_bodtypManager = bodtypManager;
    m_nutzManager = nutzManager;
    m_schneeManager = schneeManager;
    m_idleLanduseManager = idleLanduseManager;
  }

  public Feature parameterAsciiToFeature( ) throws Exception, Exception
  {
    final ModelManager modelManager = new ModelManager();
    // get all FeatureTypes...
    final IFeatureType naParaFT = m_paraSchema.getFeatureType( NaModelConstants.PARA_ROOT_FT );

    // create all Features (and FeatureCollections)
    final Feature naParaFe = modelManager.createFeature( naParaFT );
    Feature[] features;
    final IPropertyType soilLayerMemberPT = naParaFT.getProperty( NaModelConstants.PARA_SOIL_LAYER_MEMBER );
    final IPropertyType soiltypeMemberPT = naParaFT.getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );

    // complete Feature soilLayerMember
    try
    {
      features = m_bodartManager.parseFile( m_conf.getBodenartFile().toURL() );
      for( final Feature feature : features )
        FeatureHelper.addProperty( naParaFe, soilLayerMemberPT, feature );
    }
    catch( final Exception e )
    {
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ParseManager.1" ) ); //$NON-NLS-1$
    }

    // complete Feature soiltypeMember
    features = m_bodtypManager.parseFile( m_conf.getBodentypFile().toURL() );
    for( final Feature feature : features )
      FeatureHelper.addProperty( naParaFe, soiltypeMemberPT, feature );

    // complete Feature idealLandUseMember
    final File nutzungDir = m_conf.getNutzungDir();
    final FileFilter filter = FileFilterUtils.suffixFileFilter( ".nuz" ); //$NON-NLS-1$
    final File nutzFiles[] = nutzungDir.listFiles( filter );
    final IPropertyType idealLandUseMemberRT = naParaFT.getProperty( NaModelConstants.PARA_IDEAL_LANDUSE_MEMBER );
    for( final File nutzFile : nutzFiles )
    {
      // es kommt pro file immer nur ein feature zurück
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ParseManager.3", nutzFile.toURL().toString() ) ); //$NON-NLS-1$
      features = m_idleLanduseManager.parseFile( nutzFile.toURL() );
      for( final Feature feature : features )
        FeatureHelper.addProperty( naParaFe, idealLandUseMemberRT, feature );
    }
    final IPropertyType landuseMemberRT = naParaFT.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    // complete Feature landuseMember
    for( final File nutzFile : nutzFiles )
    {
      // es kommt pro file immer nur ein feature zurück
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ParseManager.4", nutzFile.toURL().toString() ) ); //$NON-NLS-1$
      features = m_nutzManager.parseFile( nutzFile.toURL() );
      for( final Feature feature : features )
        FeatureHelper.addProperty( naParaFe, landuseMemberRT, feature );
    }
    System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ParseManager.5", nutzFiles.length ) ); //$NON-NLS-1$

    final IPropertyType sealingMemberRT = naParaFT.getProperty( NaModelConstants.PARA_PROP_SEALING_MEMBER );
    final URL csvsealingURL = new File( nutzungDir, "Klassen_Sealing_KRUECK2007.csv" ).toURL(); //$NON-NLS-1$
    features = m_idleLanduseManager.parseSealingFilecsv( csvsealingURL );
    for( final Feature feature : features )
      FeatureHelper.addProperty( naParaFe, sealingMemberRT, feature );

    final URL csvURL = new File( nutzungDir, "Klassen_KRUECK2007.csv" ).toURL(); //$NON-NLS-1$
    features = m_idleLanduseManager.parseFilecsv( csvURL );
    for( final Feature feature : features )
      FeatureHelper.addProperty( naParaFe, landuseMemberRT, feature );

    final IPropertyType snowMemberRT = naParaFT.getProperty( NaModelConstants.PARA_PROP_SNOW_MEMBER );
    // complete Feature snowMember
    try
    {
      features = m_schneeManager.parseFile( m_conf.getSchneeFile().toURL() );
      for( final Feature feature : features )
        FeatureHelper.addProperty( naParaFe, snowMemberRT, feature );
    }
    catch( final Exception e )
    {
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.ParseManager.9" ) ); //$NON-NLS-1$
    }

    System.out.println( "\n\n-----------------" ); //$NON-NLS-1$
    return naParaFe;
  }
}