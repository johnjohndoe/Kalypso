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

package org.kalypso.floodrisk.data;

import java.io.File;
import java.io.FileWriter;
import java.net.URL;
import java.util.Hashtable;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.floodrisk.mathTool.ParseFunction;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Class for reading ContextModelData
 * 
 * @author N. Peiler
 *  
 */
public class ContextModel
{

  private GMLWorkspace workspace;

  private Feature rootFeature;

  //final URL contextModel_schemaURL = getClass().getResource( "../schema/ContextModell.xsd" );

  /**
   * constructor for initializing a GMLWorkspace of the ContextModelData
   * 
   * @param gmlURL
   *          Instance location of ContextModelData
   */
  public ContextModel( URL gmlURL )
  {
    //System.out.println( "/n/n/nDejan: Read " + gmlURL.getPath() + ".../n/n/n" );
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( gmlURL, null );
      rootFeature = workspace.getRootFeature();
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  /**
   * returns the LanduseTypeList of this ContextModel
   * 
   * @return LanduseTypeList (key=Name of LanduseType (String), value=landuseTypeKey (Integer))
   */
  public Hashtable getLanduseList()
  {
    QName l_LanduseCollectionMember = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseCollectionMember");
    QName l_LanduseMember           = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseMember");
    QName l_Name                    = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "Name");
    
    Hashtable<Object, Long> landuseList = new Hashtable<Object, Long>();
    Feature landuseCollection = (Feature)rootFeature.getProperty(l_LanduseCollectionMember);
    Object landuseMemberList = landuseCollection.getProperty(l_LanduseMember);
    if( landuseMemberList instanceof List )
    {
      List list = (List)landuseMemberList;
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        Object name = feat.getProperty(l_Name);
        String fid = feat.getId();
        landuseList.put( name, getID( fid, feat.getFeatureType() ) );
        //System.out.println( "Feature " + getID( fid, feat.getFeatureType() )
        // + ": " + name );
      }
    }
    return landuseList;
  }

  /**
   * returns the AdministrationUnitList of this ContextModel
   * 
   * @return AdministrationUnitList (key=Name of AdministrationUnit (String), value=administrationUnitKey (Integer))
   */
  public Hashtable getAdministrationUnitList()
  {
    Hashtable<Object, Long> administrationUnitList = null;
    if( rootFeature.getProperty( "AdministrationUnitCollectionMember" ) != null )
    {
      administrationUnitList = new Hashtable<Object, Long>();
      Feature administrationUnitCollection = (Feature)rootFeature.getProperty( "AdministrationUnitCollectionMember" );
      Object administrationUnitMemberList = administrationUnitCollection.getProperty( "AdministrationUnitMember" );
      if( administrationUnitMemberList instanceof List )
      {
        List list = (List)administrationUnitMemberList;
        for( int i = 0; i < list.size(); i++ )
        {
          Feature feat = (Feature)list.get( i );
          Object name = feat.getProperty( "Name" );
          String fid = feat.getId();
          administrationUnitList.put( name, getID( fid, feat.getFeatureType() ) );
          //System.out.println( "Feature " + getID( fid, feat.getFeatureType()
          // ) + ": " + name );
        }
      }
    }
    return administrationUnitList;
  }

  /**
   * returns the DamageFunctionList for this ContextModel
   * 
   * @return DamageFunctionList (key=landuseTypeKey (String), DamageFunction (ParseFunction))
   */
  public Hashtable getDamageFunctionList()
  {
    Hashtable<String, ParseFunction> damageFunctionList = new Hashtable<String, ParseFunction>();
    Feature damageFunctionMappingCollection = (Feature)rootFeature
        .getProperty( "DamageFunctionMappingCollectionMember" );
    Object mappingList = damageFunctionMappingCollection.getProperty( "DamageFunctionMappingMember" );
    if( mappingList instanceof List )
    {
      List list = (List)mappingList;
      System.out.println( "Damage functions:" );
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        final IRelationType lanuseLinkPT = (IRelationType) feat.getFeatureType().getProperty("LanduseLink" );
        Feature[] landuse = workspace.resolveLinks( feat, lanuseLinkPT);
        final IRelationType damageFctLinkPT = (IRelationType) feat.getFeatureType().getProperty( "DamageFunctionLink" );
        Feature damageFunction = workspace.resolveLink( feat, damageFctLinkPT);
        for( int j = 0; j < landuse.length; j++ )
        {
          String function = (String)damageFunction.getProperty( "Function" );
          String landuseFID = landuse[j].getId();
          ParseFunction parseFunction = new ParseFunction( function );
          if( parseFunction.parse() )
          {
            damageFunctionList.put( getID( landuseFID, landuse[j].getFeatureType() ).toString(), parseFunction );
          }
          System.out.println( "Function=" + function + ", Landuse=" + landuse[j].getProperty( "Name" ) );
        }
      }
    }
    return damageFunctionList;
  }

  /**
   * returns the AssetValueList for this ContextModel
   * 
   * @return AssetValueList if(administrationUnitLink!=null){ (key="landuseTypeKey,administrationUnitKey"(String),
   *         value=asset(Double)) }else{ (key=landuseTypeKey(String),
   */
  public Hashtable getAssetValueList()
  {
    Hashtable<String, Double> assetValueList = new Hashtable<String, Double>();
    Feature assetValueMappingCollection = (Feature)rootFeature.getProperty( "AssetValueMappingCollectionMember" );
    Object mappingList = assetValueMappingCollection.getProperty( "AssetValueMappingMember" );
    if( mappingList instanceof List )
    {
      List list = (List)mappingList;
      System.out.println( "Asset values:" );
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        final IRelationType landUseLink = (IRelationType) feat.getFeatureType().getProperty("LanduseLink" );
        Feature[] landuse = workspace.resolveLinks( feat,landUseLink);
        final IRelationType assetValueLink = (IRelationType) feat.getFeatureType().getProperty( "AssetValueLink" );
        Feature assetValue = workspace.resolveLink( feat, assetValueLink);
        if( feat.getProperty( "AdministrationUnitLink" ) != null )
        {
          final IRelationType administrationUniLink = (IRelationType) feat.getFeatureType().getProperty("AdministrationUnitLink" );
          Feature administrationUnit = workspace.resolveLink( feat, administrationUniLink);
          for( int j = 0; j < landuse.length; j++ )
          {
            Double value = (Double)assetValue.getProperty( "Value" );
            String landuseFID = landuse[j].getId();
            String administrationUnitFID = administrationUnit.getId();
            String key = getID( landuseFID, landuse[j].getFeatureType() ) + ","
                + getID( administrationUnitFID, administrationUnit.getFeatureType() );
            assetValueList.put( key, value );
            System.out.println( "Value=" + value + ", Landuse=" + landuse[j].getProperty( "Name" )
                + ", AdministrationUnit=" + administrationUnit.getProperty( "Name" ) );
          }
        }
        else
        {
          for( int j = 0; j < landuse.length; j++ )
          {
            Double value = (Double) assetValue.getProperty( "Value" );
            String landuseFID = landuse[j].getId();
            assetValueList.put( getID( landuseFID, landuse[j].getFeatureType() ).toString(), value );
            System.out.println( "Value=" + value + ", Landuse=" + landuse[j].getProperty( "Name" ) );
          }
        }
      }
    }
    return assetValueList;
  }

//  /**
//   * returns the IntegerValue of the featureID (Format: "Name_ID")
//   * 
//   * @deprecated should use getID( String fid, IFeatureType featureType )
//   * 
//   * @param fid
//   *          featureID (Format: "Name_ID")
//   * @return ID as Integer
//   */
//  private Integer get_ID( String fid )
//  {
//    String[] fidStrings = fid.split( "_" );
//    String id = fidStrings[1];
//    return new Integer( id );
//  }

  /**
   * returns the IntegerValue of the featureID (Format: "FeatureTypeNameID")
   * 
   * @param fid
   *          featureID (Format: "FeatureTypeNameID") analog GMLWorkspace_Impl createFeatureID
   * @param featureType
   *          featureType of the feature
   * @return ID as Integer
   */
  private Long getID( String fid, IFeatureType featureType )
  {
    String id = fid.replaceFirst( featureType.getQName().getLocalPart(), "" );
    return new Long( id );
  }

  /**
   * writes ContextModelData to file
   * 
   * @param outFile
   */
  public void toFile( File outFile )
  {
    try
    {
      FileWriter writer = new FileWriter( outFile );
      GmlSerializer.serializeWorkspace( writer, workspace );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }
}