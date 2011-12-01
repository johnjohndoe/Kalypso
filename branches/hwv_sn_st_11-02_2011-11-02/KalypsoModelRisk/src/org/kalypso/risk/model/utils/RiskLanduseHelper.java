/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.utils;

import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAssetValueClass;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Thomas Jung
 * 
 */
public class RiskLanduseHelper
{

  public static void createNewLanduseClasses( final Set<String> landuseTypeSet, final IRasterizationControlModel controlModel, final List<Feature> predefinedLanduseColorsCollection, final QName propName, final QName propDataMember, final QName propValue )
  {
    for( final String landuseType : landuseTypeSet )
    {
      if( !controlModel.containsLanduseClass( landuseType ) )
      {
        final ILanduseClass landuseClass = controlModel.createNewLanduseClass();
        landuseClass.setName( landuseType );
        landuseClass.setColorStyle( getLanduseClassDefaultColor( landuseType, predefinedLanduseColorsCollection, propName, propDataMember, propValue ) );
        landuseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
        landuseClass.setDescription( Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.0" ) ); //$NON-NLS-1$
        final IAssetValueClass suggestedAssetValueClass = controlModel.getSuggestedAssetValueClass( landuseClass.getName() );
        if( suggestedAssetValueClass != null )
          landuseClass.setAssetValue( suggestedAssetValueClass );
        final IDamageFunction suggestedDamageFunction = controlModel.getSuggestedDamageFunction( landuseClass.getName() );
        if( suggestedDamageFunction != null )
          landuseClass.setDamageFunction( suggestedDamageFunction );
      }
    }
  }

  public static IRiskLanduseStatistic getLanduseStatisticEntry( final ILanduseClass landuseClass, final int returnPeriod, final double cellSize )
  {
    if( !landuseClass.containsStatisticEntry( returnPeriod ) )
    {
      final IRiskLanduseStatistic entry = landuseClass.createNewStatisticEntry();
      final String entryName = Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.1", returnPeriod ); //$NON-NLS-1$

      entry.setName( entryName );
      entry.setReturnPeriod( returnPeriod );
      entry.setCellSize( new BigDecimal( cellSize ).setScale( 4, BigDecimal.ROUND_HALF_UP ) );
      entry.setDescription( Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.2" ) + returnPeriod + Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return entry;
    }

    return landuseClass.getStatistic( returnPeriod );
  }

  @SuppressWarnings("unchecked")
  public static RGB getLanduseClassDefaultColor( final String landuseClassName, final List<Feature> predefinedLanduseColorsCollection, final QName propName, final QName propDataMember, final QName propValue )
  {
    for( final Feature feature : predefinedLanduseColorsCollection )
    {
      final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( propDataMember );
      for( final Feature featureMember : dataMemberList )
      {
        final List<String> nameList = (List<String>) featureMember.getProperty( propName );
        if( nameList != null && nameList.size() > 0 )
        {
          if( !nameList.get( 0 ).equals( landuseClassName ) )
            continue;
          final String value = featureMember.getProperty( propValue ).toString();
          final Pattern pattern = Pattern.compile( "#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})" ); //$NON-NLS-1$
          final Matcher matcher = pattern.matcher( value );
          if( matcher.matches() )
          {
            final int r = Integer.parseInt( matcher.group( 1 ), 16 );
            final int g = Integer.parseInt( matcher.group( 2 ), 16 );
            final int b = Integer.parseInt( matcher.group( 3 ), 16 );
            return new RGB( r, g, b );
          }
          // TODO: If color format is not correct, return random color, or throw an exception?
          return getRandomColor();
        }
      }
      break;
    }
    return getRandomColor();
  }

  private static RGB getRandomColor( )
  {
    return new RGB( new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue() );
  }

  @SuppressWarnings("unchecked")
  public static void handleDBImport( final String externalProjectName, final IRasterizationControlModel controlModel, final IFolder scenarioFolder )
  {
    try
    {
      final Map<String, String> landuseClassesGmlIDsMap = new HashMap<String, String>();
      final Map<String, String> damageFunctionsGmlIDsMap = new HashMap<String, String>();
      final URL url = new URL( scenarioFolder.getLocation().append( "/models/RasterizationControlModel.gml" ).toPortableString() ); //$NON-NLS-1$
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );
      final List<Feature> landuseClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
      final List<Feature> assetValueClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
      final List<Feature> damageFunctionsFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );

      for( final Feature importedFeature : damageFunctionsFeatureList )
      {
        final IDamageFunction entry = (IDamageFunction) importedFeature.getAdapter( IDamageFunction.class );
        if( entry != null )
        {
          final IDamageFunction newDamageFunction = controlModel.createNewDamageFunction();
          newDamageFunction.setName( entry.getName() );
          newDamageFunction.setDescription( "[Import from " + externalProjectName + "] " + entry.getDescription() ); //$NON-NLS-1$ //$NON-NLS-2$
          newDamageFunction.setFunction( entry.getFunction() );
          damageFunctionsGmlIDsMap.put( entry.getGmlID(), newDamageFunction.getGmlID() );
        }
      }

      for( final Feature importedFeature : landuseClassesFeatureList )
      {
        final ILanduseClass entry = (ILanduseClass) importedFeature.getAdapter( ILanduseClass.class );
        if( entry != null )
        {
          final ILanduseClass newLanduseClass = controlModel.createNewLanduseClass();
          newLanduseClass.setName( entry.getName() );
          newLanduseClass.setDescription( "[Import from " + externalProjectName + "] " + entry.getDescription() ); //$NON-NLS-1$ //$NON-NLS-2$
          newLanduseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
          newLanduseClass.setColorStyle( entry.getColorStyle() );
          final String damageFunctionGmlID = damageFunctionsGmlIDsMap.get( entry.getDamageFunctionGmlID() );
          for( final IDamageFunction damageFunction : controlModel.getDamageFunctionsList() )
            if( damageFunction.getGmlID().equals( damageFunctionGmlID ) )
              newLanduseClass.setDamageFunction( damageFunction );
          landuseClassesGmlIDsMap.put( entry.getGmlID(), newLanduseClass.getGmlID() );
        }
      }
      for( final Feature importedFeature : assetValueClassesFeatureList )
      {
        final IAssetValueClass entry = (IAssetValueClass) importedFeature.getAdapter( IAssetValueClass.class );
        if( entry != null )
        {
          final String description = "[Import from " + externalProjectName + "] " + entry.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
          final String name = entry.getName();
          final Double assetValue = entry.getAssetValue();

          final IAssetValueClass newAssetValueClass = controlModel.createNewAssetValueClass();
          newAssetValueClass.setName( name );
          newAssetValueClass.setDescription( description );
          newAssetValueClass.setAssetValue( assetValue );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public static void handleUsePreDefinedData( final String damageFunctionsCollectionName, final String assetValuesCollectionName, final IRasterizationControlModel controlModel, final List<Feature> predefinedDamageFunctionsCollection, final List<Feature> predefinedAssetValueClassesCollection, final QName propName, final QName propDataMember, final QName propDesc, final QName propValue )
  {
    createDamageFunctions( damageFunctionsCollectionName, controlModel, predefinedDamageFunctionsCollection, propName, propDataMember, propDesc, propValue );

    createAssetValues( assetValuesCollectionName, controlModel, predefinedAssetValueClassesCollection, propName, propDataMember, propValue );
  }

  @SuppressWarnings("unchecked")
  private static void createAssetValues( final String assetValuesCollectionName, final IRasterizationControlModel controlModel, final List<Feature> predefinedAssetValueClassesCollection, final QName propName, final QName propDataMember, final QName propValue )
  {
    // delete already existing asset values
    controlModel.getAssetValueClassesList().clear();

    /* asset values */
    for( final Feature assetFeatureClass : predefinedAssetValueClassesCollection )
    {
      final List<String> names = (List<String>) assetFeatureClass.getProperty( propName );
      if( names != null && names.size() > 0 )
        if( names.get( 0 ).equals( assetValuesCollectionName ) )
        {
          final List<Feature> dataMemberList = (List<Feature>) assetFeatureClass.getProperty( propDataMember );
          for( final Feature featureMember : dataMemberList )
          {
            final List<String> nameList = (List<String>) featureMember.getProperty( propName );
            if( nameList != null && nameList.size() > 0 )
            {
              final String landuseClassName = nameList.get( 0 );
              final Double assetValue = Double.valueOf( featureMember.getProperty( propValue ).toString() );

              /* create new landuse classes */
              // if( !controlModel.containsLanduseClass( landuseClassName ) )
              // {
              // final ILanduseClass newLanduseClass = controlModel.createNewLanduseClass();
              // newLanduseClass.setName( landuseClassName );
              // newLanduseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
              // newLanduseClass.setColorStyle( RiskLanduseHelper.getLanduseClassDefaultColor( landuseClassName,
              // predefinedLanduseColorsCollection, propName, propDataMember, propValue ) );
              // newLanduseClass.setDescription( "Created by " + assetValuesCollectionName + " asset values import" );
              // }
              final IAssetValueClass assetValueClass = controlModel.getAssetValueClass( assetValue, landuseClassName, "[" + assetValuesCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$

              // we assign here the asset value to the landuse class
              final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();
              for( final ILanduseClass landuseClass : landuseClassesList )
              {
                if( landuseClass.getName().equals( landuseClassName ) )
                  landuseClass.setAssetValue( assetValueClass );
              }
            }
          }
          break;
        }
    }
  }

  @SuppressWarnings("unchecked")
  private static void createDamageFunctions( final String damageFunctionsCollectionName, final IRasterizationControlModel controlModel, final List<Feature> predefinedDamageFunctionsCollection, final QName propName, final QName propDataMember, final QName propDesc, final QName propValue )
  {
    // delete already existing damage functions
    controlModel.getDamageFunctionsList().clear();

    /* damage functions */
    for( final Feature damageFeatureClass : predefinedDamageFunctionsCollection )
    {
      final List<String> names = (List<String>) damageFeatureClass.getProperty( propName );
      if( names != null && names.size() > 0 )
        if( names.get( 0 ).equals( damageFunctionsCollectionName ) )
        {
          final List<Feature> dataMemberList = (List<Feature>) damageFeatureClass.getProperty( propDataMember );
          for( final Feature featureMember : dataMemberList )
          {
            final List<String> nameList = (List<String>) featureMember.getProperty( propName );
            if( nameList != null && nameList.size() > 0 )
            {
              final String name = nameList.get( 0 );
              final String value = featureMember.getProperty( propValue ).toString();
              final String description = (String) featureMember.getProperty( propDesc );

              /* create a new damage function */
              // check, if damage function already exists
              IDamageFunction damageFunction = controlModel.getDamageFunction( name, value, description );
              if( damageFunction == null )
              {
                damageFunction = controlModel.createNewDamageFunction();
                damageFunction.setName( name );

                if( description != null && description.length() > 0 )
                  damageFunction.setDescription( description + " [" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                else
                  damageFunction.setDescription( "[" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                damageFunction.setFunction( value );
              }
            }
          }
          break;
        }
    }
  }

  public static List<Feature> createLandusePolygons( final String landuseProperty, final IProgressMonitor monitor, final List< ? > shapeFeatureList, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection, final List<ILanduseClass> landuseClassesList ) throws CloneNotSupportedException
  {
    monitor.subTask( Messages.getString( "org.kalypso.risk.model.utils.ImportLanduseWizard.9" ) ); //$NON-NLS-1$
    final List<Feature> createdFeatures = new ArrayList<Feature>();

    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final Feature shpFeature = (Feature) shapeFeatureList.get( i );
      final QName shpGeomQName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), ShapeSerializer.PROPERTY_GEOM );
      final QName shpPropQName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), landuseProperty );
      final String shpPropertyValue = shpFeature.getProperty( shpPropQName ).toString();

      final GM_Object shpGeometryProperty = (GM_Object) shpFeature.getProperty( shpGeomQName );

      // we don't like multi surfaces, so...
      if( shpGeometryProperty instanceof GM_MultiSurface )
      {
        final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) shpGeometryProperty).clone();
        final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
        for( final GM_Surface< ? > surface : surfaces )
        {
          final ILandusePolygon polygon = landusePolygonCollection.addNew( ILandusePolygon.QNAME );
          polygon.setGeometry( surface );
          polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), shpPropertyValue, landuseClassesList ) );
          polygon.getFeature().setEnvelopesUpdated();
          createdFeatures.add( polygon.getFeature() );
          // style and landuse class ordinal number will be set automatically (property functions)
        }
      }
      else if( shpGeometryProperty instanceof GM_Surface )
      {
        final ILandusePolygon polygon = landusePolygonCollection.addNew( ILandusePolygon.QNAME );
        polygon.setGeometry( (GM_Surface< ? >) shpGeometryProperty );
        polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), shpPropertyValue, landuseClassesList ) );
        // polygon.setStyleType( shpPropertyValue );
        polygon.getFeature().setEnvelopesUpdated();
        createdFeatures.add( polygon.getFeature() );
      }
      else
        throw new RuntimeException( Messages.getString( "org.kalypso.risk.model.utils.ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
    }
    return createdFeatures;
  }

  private static Feature getLanduseClassByName( final Feature feature, final String className, final List<ILanduseClass> landuseClassesList )
  {
    // TODO: is this good?
    final String linkedFeaturePath = "RasterizationControlModel.gml#"; //$NON-NLS-1$
    for( final Object object : landuseClassesList )
      if( object instanceof ILanduseClass )
      {
        final ILanduseClass landuseClass = (ILanduseClass) object;
        if( landuseClass.getName().equals( className ) )
        {
          final String xlinkedFeaturePath = linkedFeaturePath + landuseClass.getGmlID();
          final XLinkedFeature_Impl linkedFeature_Impl = new XLinkedFeature_Impl( feature, landuseClass.getFeature().getParentRelation(), landuseClass.getFeature().getFeatureType(), xlinkedFeaturePath, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
          return linkedFeature_Impl;
        }
      }
    return null;
  }

  public static HashSet<String> getLanduseTypeSet( final List< ? > shapeFeatureList, final String propertyLanduse )
  {
    final HashSet<String> set = new HashSet<String>();

    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final Feature shpFeature = (Feature) shapeFeatureList.get( i );
      final QName shapeLandusePropertyName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), propertyLanduse );
      final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
      if( !set.contains( shpPropertyValue ) )
        set.add( shpPropertyValue );
    }

    return set;
  }

  public static ILanduseClass[] getLanduseTypeSet( final IRasterizationControlModel conrolModel, final IVectorDataModel vectorModel )
  {
    final ILanduseClass[] classes = conrolModel.getLanduseClassesList().toArray( new ILanduseClass[] {} );

    final Set<ILanduseClass> myTypes = new LinkedHashSet<ILanduseClass>();
    for( final ILandusePolygon polygon : vectorModel.getLandusePolygonCollection() )
    {
      final ILanduseClass landuse = polygon.getLanduseClass( conrolModel );
      for( final ILanduseClass c : classes )
      {
        if( c.getName().equals( landuse.getName() ) )
        {
          myTypes.add( c );
          break;
        }
      }
    }

    return myTypes.toArray( new ILanduseClass[] {} );
  }

}
