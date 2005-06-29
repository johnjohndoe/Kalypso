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

import org.kalypso.floodrisk.tools.Interval;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
   * @param gmlURL
   *          Instance location of RiskContextModelData
   *  
   */
  public RiskContextModel( URL gmlURL )
  {
    //System.out.println( "Read " + gmlFile.getName() + "..." );
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( gmlURL );
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
   * @return LanduseTypeList (key=Name of LanduseType (String), value=landuseTypeKey (Integer))
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
        landuseList.put( name, getID( fid, feat.getFeatureType() ) );
        System.out.println( "Feature " + getID( fid, feat.getFeatureType() ) + ": " + name );
      }
    }
    return landuseList;
  }

  /**
   * returns the RiskClassKeyList of this RiskContextModel
   * 
   * @return RiskClassKeyList (key=Risk (String) {gering,mittel,hoch}, value=riskClassKey (Integer))
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
      riskClassKeyList.put( risk, getID( fid, riskClassMember.getFeatureType() ) );
      System.out.println( "Risk: " + risk + ", ID: " + getID( fid, riskClassMember.getFeatureType() ) );
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
    Feature intervalMappingCollection = (Feature)rootFeature.getProperty( "IntervalMappingCollectionMember" );
    List intervalMappingList = (List)intervalMappingCollection.getProperty( "IntervalMappingMember" );
    for( int i = 0; i < intervalMappingList.size(); i++ )
    {
      Feature intervalMappingMember = (Feature)intervalMappingList.get( i );
      Feature intervalCollection = workspace.resolveLink( intervalMappingMember, "IntervalCollectionLink" );
      System.out.println( "RiskClassList:" );
      Hashtable riskClassList = getRiskClassList( intervalCollection );
      Feature[] landuseLinks = workspace.resolveLinks( intervalMappingMember, "LanduseLink" );
      for( int j = 0; j < landuseLinks.length; j++ )
      {
        String fid = landuseLinks[j].getId();
        Integer landuseKey = getID( fid, landuseLinks[j].getFeatureType() );
        riskClassLists.put( landuseKey, riskClassList );
        System.out.println( "# " + landuseLinks[j].getProperty( "Name" ) );
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
      Integer key = getID( featRisk.getId(), featRisk.getFeatureType() );
      riskClassList.put( key, interval );
      System.out.println( "RiskClass " + key + ": " + "MinValue=" + interval.getLowerLimit() + ", MaxValue="
          + interval.getUpperLimit() );
    }
    return riskClassList;
  }

  /**
   * returns the IntegerValue of the featureID (Format: "Name_ID")
   * 
   * @deprecated should use getID( String fid, FeatureType featureType )
   * 
   * @param fid
   *          featureID (Format: "Name_ID")
   * @return ID as Integer
   */
  private Integer get_ID( String fid )
  {
    String[] fidStrings = fid.split( "_" );
    String id = fidStrings[1];
    return new Integer( id );
  }

  /**
   * returns the IntegerValue of the featureID (Format: "FeatureTypeNameID")
   * 
   * @param fid
   *          featureID (Format: "FeatureTypeNameID") analog GMLWorkspace_Impl createFeatureID
   * @param featureType
   *          featureType of the feature
   * @return ID as Integer
   */
  private Integer getID( String fid, FeatureType featureType )
  {
    String id = fid.replaceFirst( featureType.getName(), "" );
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