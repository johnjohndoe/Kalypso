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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.actions.dataImport.landuse.Messages;
import org.kalypso.risk.model.schema.binding.IAdministrationUnit;
import org.kalypso.risk.model.schema.binding.IAssetValueClass;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
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
public class RiskImportLanduseHelper
{

  public static void createNewLanduseClasses( final HashSet<String> landuseTypeSet, final IRasterizationControlModel controlModel, final List<IAdministrationUnit> administrationUnits, List<Feature> predefinedLanduseColorsCollection, final QName propName, final QName propDataMember, final QName propValue )
  {
    for( final String landuseType : landuseTypeSet )
    {
      if( !controlModel.containsLanduseClass( landuseType ) )
      {
        final ILanduseClass landuseClass = controlModel.createNewLanduseClass();
        landuseClass.setName( landuseType );
        landuseClass.setColorStyle( getLanduseClassDefaultColor( landuseType, predefinedLanduseColorsCollection, propName, propDataMember, propValue ) );
        landuseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
        landuseClass.setDescription( "Created as value of imported landuse shape" ); //$NON-NLS-1$
        for( final IAdministrationUnit administrationUnit : administrationUnits )
          controlModel.createNewAssetValueClass( landuseClass.getGmlID(), administrationUnit.getGmlID(), 0.0, "" ); //$NON-NLS-1$
      }
    }
  }

  @SuppressWarnings("unchecked")
  public static RGB getLanduseClassDefaultColor( final String landuseClassName, final List<Feature> predefinedLanduseColorsCollection, final QName propName, final QName propDataMember, final QName propValue )
  {
    for( final Feature feature : predefinedLanduseColorsCollection )
    {
      final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( propDataMember );
      for( final Feature featureMember : dataMemberList )
      {
        final List<String> nameList = ((List<String>) featureMember.getProperty( propName ));
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
          // If color format is not correct, return random color, or throw an exception?
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
  public static void handleDBImport( final String externalProjectName, final IRasterizationControlModel controlModel, IFolder scenarioFolder )
  {
    try
    {
      final Map<String, String> landuseClassesGmlIDsMap = new HashMap<String, String>();
      final Map<String, String> damageFunctionsGmlIDsMap = new HashMap<String, String>();
      final Map<String, String> administrationUnitsGmlIDsMap = new HashMap<String, String>();
      final URL url = scenarioFolder.getProject().getParent().getRawLocation().append( "/" + externalProjectName + "/Basis/models/RasterizationControlModel.gml" ).toFile().toURL(); //$NON-NLS-1$ //$NON-NLS-2$
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, new UrlResolver(), null );
      final List<Feature> landuseClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
      final List<Feature> assetValueClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
      final List<Feature> damageFunctionsFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );
      final List<Feature> administrationUnitsFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_ADMINISTRATION_UNIT_MEMBER );
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
      for( final Feature importedFeature : administrationUnitsFeatureList )
      {
        final IAdministrationUnit entry = (IAdministrationUnit) importedFeature.getAdapter( IAdministrationUnit.class );
        if( entry != null )
        {
          final String description = "[Import from " + externalProjectName + "] " + entry.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
          final IAdministrationUnit newAdministrationUnit = controlModel.createNewAdministrationUnit( entry.getName(), description );
          administrationUnitsGmlIDsMap.put( entry.getGmlID(), newAdministrationUnit.getGmlID() );
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
          final String landuseClassGmlID = landuseClassesGmlIDsMap.get( entry.getLanduseClassGmlID() );
          final String administrationUnitGmlID = administrationUnitsGmlIDsMap.get( entry.getAdministrationUnitGmlID() );
          final String description = "[Import from " + externalProjectName + "] " + entry.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
          controlModel.createNewAssetValueClass( landuseClassGmlID, administrationUnitGmlID, entry.getAssetValue(), description );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @SuppressWarnings("unchecked")
  public static void handleUserDefinedData( final String damageFunctionsCollectionName, final String assetValuesCollectionName, final IRasterizationControlModel controlModel, final List<IAdministrationUnit> administrationUnits, List<Feature> predefinedDamageFunctionsCollection, List<Feature> predefinedAssetValueClassesCollection, final QName propName, final QName propDataMember, final QName propDesc, final QName propValue, List<Feature> predefinedLanduseColorsCollection )
  {
    for( final Feature feature : predefinedDamageFunctionsCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( propName );
      if( names != null && names.size() > 0 )
        if( names.get( 0 ).equals( damageFunctionsCollectionName ) )
        {
          final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( propDataMember );
          for( final Feature featureMember : dataMemberList )
          {
            final List<String> nameList = ((List<String>) featureMember.getProperty( propName ));
            if( nameList != null && nameList.size() > 0 )
            {
              final String name = nameList.get( 0 );
              final String value = featureMember.getProperty( propValue ).toString();
              final String description = (String) featureMember.getProperty( propDesc );
              final IDamageFunction damageFunction = controlModel.createNewDamageFunction();
              damageFunction.setName( name );
              if( description != null && description.length() > 0 )
                damageFunction.setDescription( description + " [" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
              else
                damageFunction.setDescription( "[" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
              damageFunction.setFunction( value );
            }
          }
          break;
        }
    }
    for( final Feature feature : predefinedAssetValueClassesCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( propName );
      if( names != null && names.size() > 0 )
        if( names.get( 0 ).equals( assetValuesCollectionName ) )
        {
          final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( propDataMember );
          for( final Feature featureMember : dataMemberList )
          {
            final List<String> nameList = ((List<String>) featureMember.getProperty( propName ));
            if( nameList != null && nameList.size() > 0 )
            {
              final String landuseClassName = nameList.get( 0 );
              final Double assetValue = new Double( featureMember.getProperty( propValue ).toString() );

              if( !controlModel.containsLanduseClass( landuseClassName ) )
              {
                final ILanduseClass newLanduseClass = controlModel.createNewLanduseClass();
                newLanduseClass.setName( landuseClassName );
                newLanduseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
                newLanduseClass.setColorStyle( RiskImportLanduseHelper.getLanduseClassDefaultColor( landuseClassName, predefinedLanduseColorsCollection, propName, propDataMember, propValue ) );

                newLanduseClass.setDescription( "Created by " + assetValuesCollectionName + " asset values import" ); //$NON-NLS-1$ //$NON-NLS-2$

                final String landuseClassGmlID = newLanduseClass.getGmlID();
                for( final IAdministrationUnit administrationUnit : administrationUnits )
                  controlModel.createNewAssetValueClass( landuseClassGmlID, administrationUnit.getGmlID(), assetValue, "[" + assetValuesCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
              }
              else
              {
                final String landuseClassGmlID = controlModel.getLanduseClassID( landuseClassName ).get( 0 );
                for( final IAdministrationUnit administrationUnit : administrationUnits )
                {
                  final IAssetValueClass assetValueClass = controlModel.getAssetValueClass( landuseClassGmlID, administrationUnit.getGmlID(), true );
                  assetValueClass.setAssetValue( assetValue );
                  assetValueClass.setDescription( "[" + assetValuesCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                }
              }
            }
          }
          break;
        }
    }
  }

  @SuppressWarnings("unchecked")
  public static List<Feature> createLandusePolygons( final String landuseProperty, final IProgressMonitor monitor, final IFolder scenarioFolder, final List shapeFeatureList, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection, final List<ILanduseClass> landuseClassesList ) throws CloneNotSupportedException
  {
    monitor.subTask( Messages.getString( "ImportLanduseWizard.9" ) ); //$NON-NLS-1$
    final List<Feature> createdFeatures = new ArrayList<Feature>();

    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final Feature shpFeature = (Feature) shapeFeatureList.get( i );
      final QName shpPropQName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), landuseProperty );
      final String shpPropertyValue = shpFeature.getProperty( shpPropQName ).toString();
      // final String shpPropertyValue = shpPropQName.toString();
      final ILandusePolygon polygon = landusePolygonCollection.addNew( ILandusePolygon.QNAME );
      final GM_Object shpGeometryProperty = (GM_Object) shpFeature.getProperty( ShapeSerializer.PROPERTY_GEOMETRY );

      // we don't like multi surfaces, so...
      if( shpGeometryProperty instanceof GM_MultiSurface )
      {
        final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) shpGeometryProperty).clone();
        final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
        for( int k = 0; k < surfaces.length; k++ )
        {
          polygon.setGeometry( surfaces[k] );
          polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue, landuseClassesList ) );
          polygon.getFeature().invalidEnvelope();
          createdFeatures.add( polygon.getFeature() );
          // stule and landuse class ordinal number will be set automatically (property functions)
        }
      }
      else if( shpGeometryProperty instanceof GM_Surface )
      {
        polygon.setGeometry( (GM_Surface< ? >) shpGeometryProperty );
        polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue, landuseClassesList ) );
        // polygon.setStyleType( shpPropertyValue );
        polygon.getFeature().invalidEnvelope();
        createdFeatures.add( polygon.getFeature() );
      }
      else
        throw new RuntimeException( Messages.getString( "ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
    }
    return createdFeatures;
  }

  private static Feature getLanduseClassByName( final Feature feature, final IFolder projectFolder, final String className, List<ILanduseClass> landuseClassesList )
  {
    final String linkedFeaturePath = "project:/" + projectFolder.getProjectRelativePath().toPortableString() + "/models/RasterizationControlModel.gml#"; //$NON-NLS-1$ //$NON-NLS-2$
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

  public static IStatus isRightParameterUsed( final String shapeLandusePropertyName )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final UIJob runnable = new UIJob( display, "proba" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        if( MessageDialog.openQuestion( display.getActiveShell(), "Question", Messages.getString( "ImportLanduseWizard.60" ) + shapeLandusePropertyName //$NON-NLS-1$ //$NON-NLS-2$
            + Messages.getString( "ImportLanduseWizard.61" ) ) ) //$NON-NLS-1$
          return Status.OK_STATUS;
        else
          return Status.CANCEL_STATUS;
      }
    };
    runnable.setUser( true );
    runnable.schedule();
    return null;
  }

}
