package damageAnalysis;

import java.io.File;
import java.io.FileWriter;
import java.util.Hashtable;
import java.util.List;

import mathTool.ParseFunction;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

import view.LogView;

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

  /**
   * constructor for initializing a GMLWorkspace of the ContextModelData
   * 
   * @param gmlFile
   *          Instance location of ContextModelData
   * @param schemaFile
   *          Schema location of ContextModelData
   */
  public ContextModel( File gmlFile, File schemaFile )
  {
    LogView.println( "Read " + gmlFile.getName() + "..." );
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( gmlFile.toURL(), schemaFile.toURL() );
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
   * @return LanduseTypeList (key=Name of LanduseType (String),
   *         value=landuseTypeKey (Integer))
   */
  public Hashtable getLanduseList()
  {
    Hashtable landuseList = new Hashtable();
    Feature landuseCollection = (Feature)rootFeature.getProperty( "LanduseCollectionMember" );
    Object landuseMemberList = landuseCollection.getProperty( "LanduseMember" );
    if( landuseMemberList instanceof List )
    {
      List list = (List)landuseMemberList;
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        Object name = feat.getProperty( "Name" );
        String fid = feat.getId();
        landuseList.put( name, getID( fid ) );
        System.out.println( "Feature " + getID( fid ) + ": " + name );
      }
    }
    return landuseList;
  }

  /**
   * returns the AdministrationUnitList of this ContextModel
   * 
   * @return AdministrationUnitList (key=Name of AdministrationUnit (String),
   *         value=administrationUnitKey (Integer))
   */
  public Hashtable getAdministrationUnitList()
  {
    Hashtable administrationUnitList = null;
    if( rootFeature.getProperty( "AdministrationUnitCollectionMember" ) != null )
    {
      administrationUnitList = new Hashtable();
      Feature administrationUnitCollection = (Feature)rootFeature
          .getProperty( "AdministrationUnitCollectionMember" );
      Object administrationUnitMemberList = administrationUnitCollection
          .getProperty( "AdministrationUnitMember" );
      if( administrationUnitMemberList instanceof List )
      {
        List list = (List)administrationUnitMemberList;
        for( int i = 0; i < list.size(); i++ )
        {
          Feature feat = (Feature)list.get( i );
          Object name = feat.getProperty( "Name" );
          String fid = feat.getId();
          administrationUnitList.put( name, getID( fid ) );
          System.out.println( "Feature " + getID( fid ) + ": " + name );
        }
      }
    }
    return administrationUnitList;
  }

  /**
   * returns the DamageFunctionList for this ContextModel
   * 
   * @return DamageFunctionList (key=landuseTypeKey (String), DamageFunction
   *         (ParseFunction))
   */
  public Hashtable getDamageFunctionList()
  {
    Hashtable damageFunctionList = new Hashtable();
    Feature damageFunctionMappingCollection = (Feature)rootFeature
        .getProperty( "DamageFunctionMappingCollectionMember" );
    Object mappingList = damageFunctionMappingCollection
        .getProperty( "DamageFunctionMappingMember" );
    if( mappingList instanceof List )
    {
      List list = (List)mappingList;
      LogView.println( "Damage functions:" );
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        Feature[] landuse = workspace.resolveLinks( feat, "LanduseLink" );
        Feature damageFunction = workspace.resolveLink( feat, "DamageFunctionLink" );
        for( int j = 0; j < landuse.length; j++ )
        {
          String function = (String)damageFunction.getProperty( "Function" );
          String landuseFID = landuse[j].getId();
          ParseFunction parseFunction = new ParseFunction( function );
          if( parseFunction.parse() )
          {
            damageFunctionList.put( getID( landuseFID ).toString(), parseFunction );
          }
          LogView
              .println( "Function=" + function + ", Landuse=" + landuse[j].getProperty( "Name" ) );
        }
      }
    }
    return damageFunctionList;
  }

  /**
   * returns the AssetValueList for this ContextModel
   * 
   * @return AssetValueList if(administrationUnitGrid!=null){
   *         (key="landuseTypeKey,administrationUnitKey"(String),
   *         value=asset(Double)) }else{ (key=landuseTypeKey(String),
   */
  public Hashtable getAssetValueList()
  {
    Hashtable assetValueList = new Hashtable();
    Feature assetValueMappingCollection = (Feature)rootFeature
        .getProperty( "AssetValueMappingCollectionMember" );
    Object mappingList = assetValueMappingCollection.getProperty( "AssetValueMappingMember" );
    if( mappingList instanceof List )
    {
      List list = (List)mappingList;
      LogView.println( "Asset values:" );
      for( int i = 0; i < list.size(); i++ )
      {
        Feature feat = (Feature)list.get( i );
        Feature[] landuse = workspace.resolveLinks( feat, "LanduseLink" );
        Feature assetValue = workspace.resolveLink( feat, "AssetValueLink" );
        if( feat.getProperty( "AdministrationUnitLink" ) != null )
        {
          Feature administrationUnit = workspace.resolveLink( feat, "AdministrationUnitLink" );
          for( int j = 0; j < landuse.length; j++ )
          {
            Double value = (Double)assetValue.getProperty( "Value" );
            String landuseFID = landuse[j].getId();
            String administrationUnitFID = administrationUnit.getId();
            String key = getID( landuseFID ) + "," + getID( administrationUnitFID );
            assetValueList.put( key, value );
            LogView.println( "Value=" + value + ", Landuse=" + landuse[j].getProperty( "Name" )
                + ", AdministrationUnit=" + administrationUnit.getProperty( "Name" ) );
          }
        }
        else
        {
          for( int j = 0; j < landuse.length; j++ )
          {
            Double value = (Double)assetValue.getProperty( "Value" );
            String landuseFID = landuse[j].getId();
            assetValueList.put( getID( landuseFID ).toString(), value );
            LogView.println( "Value=" + value + ", Landuse=" + landuse[j].getProperty( "Name" ) );
          }
        }
      }
    }
    return assetValueList;
  }

  /**
   * returns the IntegerValue of the featureID (Format: "Name_ID")
   * 
   * @param fid
   *          featureID (Format: "Name_ID")
   * @return ID as Integer
   */
  private Integer getID( String fid )
  {
    String[] fidStrings = fid.split( "_" );
    String id = fidStrings[1];
    return new Integer( id );
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