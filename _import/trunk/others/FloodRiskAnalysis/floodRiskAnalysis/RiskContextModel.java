package floodRiskAnalysis;

import java.io.File;
import java.io.FileWriter;
import java.util.Hashtable;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

import tools.Interval;
import view.LogView;

/**
 * Class for reading RiskContextModelData
 * 
 * @author N. Peiler
 *  
 */
public class RiskContextModel
{

  private GMLWorkspace workspace;

  private Feature rootFeature;

  /**
   * constructor for initializing a GMLWorkspace of the RiskContextModelData
   * 
   * @param gmlFile
   *          Instance location of RiskContextModelData
   * @param schemaFile
   *          Schema location of RiskContextModelData
   *  
   */
  public RiskContextModel( File gmlFile, File schemaFile )
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
   * returns the LanduseTypeList of this RiskContextModel
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
   * returns the RiskClassKeyList of this RiskContextModel
   * 
   * @return RiskClassKeyList (key=Risk (String) {gering,mittel,hoch},
   *         value=riskClassKey (Integer))
   */
  public Hashtable getRiskClassKeyList()
  {
    Hashtable riskClassKeyList = new Hashtable();
    Feature riskClassCollection = (Feature)rootFeature.getProperty( "RiskClassCollectionMember" );
    List riskClassList = (List)riskClassCollection.getProperty( "RiskClassMember" );
    for( int i = 0; i < riskClassList.size(); i++ )
    {
      Feature riskClassMember = (Feature)riskClassList.get( i );
      String fid = riskClassMember.getId();
      String risk = (String)riskClassMember.getProperty( "Risk" );
      riskClassKeyList.put( risk, getID( fid ) );
      System.out.println( "Risk: " + risk + ", ID: " + getID( fid ) );
    }
    return riskClassKeyList;
  }

  /**
   * returns a Hashtable of RiskClassLists for this RiskContextModel
   * 
   * @return RiskClassLists (key=landuseTypeKey(Integer), value=riskClassList)
   */
  public Hashtable getRiskClassLists()
  {
    Hashtable riskClassLists = new Hashtable();
    Feature intervalMappingCollection = (Feature)rootFeature
        .getProperty( "IntervalMappingCollectionMember" );
    List intervalMappingList = (List)intervalMappingCollection
        .getProperty( "IntervalMappingMember" );
    for( int i = 0; i < intervalMappingList.size(); i++ )
    {
      Feature intervalMappingMember = (Feature)intervalMappingList.get( i );
      Feature intervalCollection = workspace.resolveLink( intervalMappingMember,
          "IntervalCollectionLink" );
      LogView.println( "RiskClassList:" );
      Hashtable riskClassList = getRiskClassList( intervalCollection );
      Feature[] landuseLinks = workspace.resolveLinks( intervalMappingMember, "LanduseLink" );
      for( int j = 0; j < landuseLinks.length; j++ )
      {
        String fid = landuseLinks[j].getId();
        Integer landuseKey = getID( fid );
        riskClassLists.put( landuseKey, riskClassList );
        LogView.println( "# " + landuseLinks[j].getProperty( "Name" ) );
      }
    }
    return riskClassLists;
  }

  /**
   * returns the RiskClassList for a landuseType
   * 
   * @param intervalCollection
   *          Collection of RiskClassIntervals
   * @return RiskClassList (key=riskClassKey(Integer), value=RiskClassInterval)
   */
  public Hashtable getRiskClassList( Feature intervalCollection )
  {
    Hashtable riskClassList = new Hashtable();
    List intervalMemberList = (List)intervalCollection.getProperty( "IntervalMember" );
    for( int i = 0; i < intervalMemberList.size(); i++ )
    {
      Feature intervalMember = (Feature)intervalMemberList.get( i );
      Feature featRisk = workspace.resolveLink( intervalMember, "RiskClassLink" );
      Double minValue = (Double)intervalMember.getProperty( "minValue" );
      Double maxValue = (Double)intervalMember.getProperty( "maxValue" );
      Interval interval = new Interval( minValue.doubleValue(), maxValue.doubleValue() );
      Integer key = getID( featRisk.getId() );
      riskClassList.put( key, interval );
      LogView.println( "RiskClass " + key + ": " + "MinValue=" + interval.getLowerLimit()
          + ", MaxValue=" + interval.getUpperLimit() );
    }
    return riskClassList;
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
   * writes RiskContextModelData to file
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