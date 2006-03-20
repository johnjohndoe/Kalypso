/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

package org.kalypso.floodrisk.damageAnalysis;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import org.kalypso.floodrisk.mathTool.ParseFunction;
import org.kalypso.floodrisk.tools.GridGeometryHelper;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

/**
 * 
 * Collection of methods for calculation of damage
 * 
 * @author N. Peiler
 *  
 */
public class DamageAnalysis
{
  /**
   * returns a TreeMap of damagePercentageGrids as RectifiedGridCoverages for a given TreeMap of waterlevelGrids
   * (RectifiedGridCoverages)
   * 
   * @param waterlevelGrids
   *          key=annuality, value=waterlevelGrid as RectifiedGridCoverage
   * @param landuseGrid
   * @param damageFunctions
   *          key=landuseKey, value=parseFunction
   * @return TreeMap of damagePercentageGrids
   * @throws Exception
   */
  public static TreeMap calculateDamagePercentages( TreeMap waterlevelGrids, RectifiedGridCoverage landuseGrid,
      Hashtable damageFunctions ) throws Exception
  {
    System.out.println( "Calculate DamagePercentageGrids..." );
    TreeMap<Double, RectifiedGridCoverage> damagePercentageGrids = new TreeMap<Double, RectifiedGridCoverage>();
    Iterator it = waterlevelGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage waterlevelGrid = (RectifiedGridCoverage)waterlevelGrids.get( key );
      double annuality = 1 / key.doubleValue();
      System.out.println( "HQ " + ( new Double( annuality ) ).intValue() );
      RectifiedGridCoverage damagePercentageGrid = calculateDamagePercentage( waterlevelGrid, landuseGrid,
          damageFunctions );
      damagePercentageGrids.put( key, damagePercentageGrid );
    }
    return damagePercentageGrids;
  }

  /**
   * calculates the percentage of damage for each cell in the waterlevelGrid
   * 
   * @param waterlevelGrid
   * @param landuseGrid
   * @param damageFunctions
   *          key=landuseKey, value=parseFunction
   * @return RectifiedGridCoverage damagePercentageGrid
   * @throws Exception
   */
  private static RectifiedGridCoverage calculateDamagePercentage( RectifiedGridCoverage waterlevelGrid,
      RectifiedGridCoverage landuseGrid, Hashtable damageFunctions ) throws Exception
  {
    RectifiedGridCoverage damagePercentageGrid = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( waterlevelGrid.getGridDomain(), landuseGrid.getGridDomain() );

    RectifiedGridDomain damagePercentage_gridDomain = new RectifiedGridDomain( waterlevelGrid.getGridDomain()
        .getOrigin( null ), waterlevelGrid.getGridDomain().getOffset(), waterlevelGrid.getGridDomain().getGridRange() );
    Vector waterlevel_rangeSetData = waterlevelGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector<Vector<Double>> damagePercentage_rangeSetData = new Vector<Vector<Double>>();
    for( int i = 0; i < waterlevel_rangeSetData.size(); i++ )
    {
      Vector waterlevel_rowData = (Vector)waterlevel_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector<Double> damagePercentage_rowData = new Vector<Double>();
      for( int j = 0; j < waterlevel_rowData.size(); j++ )
      {
        if( waterlevel_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
        {
          try
          {
            double damagePercentage = 0;
            double waterlevel = ( (Double)waterlevel_rowData.get( j ) ).doubleValue();
            Double landuseKey = (Double)landuse_rowData.get( j );
            String landuseKeyAsString = Integer.toString( landuseKey.intValue() );
            if( damageFunctions.get( landuseKeyAsString ) != null )
            {
              ParseFunction damageFunction = (ParseFunction)damageFunctions.get( landuseKeyAsString );
              damagePercentage = damageFunction.getResult( waterlevel );
            }
            damagePercentage_rowData.addElement( new Double( damagePercentage ) );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
        else
        {
          damagePercentage_rowData.addElement( null );
        }
      }//for j
      damagePercentage_rangeSetData.addElement( damagePercentage_rowData );
      /*
       * System.out.println(i + " rows of " + waterlevel_rangeSetData.size() + " calculated");
       */
    }//for i
    RangeSet damagePercentage_rangeSet = new RangeSet( damagePercentage_rangeSetData, null );
    damagePercentageGrid = new RectifiedGridCoverage( damagePercentage_gridDomain, damagePercentage_rangeSet );
    return damagePercentageGrid;
  }

  /**
   * returns a TreeMap of damageGrids as RectifiedGridCoverages for a given TreeMap of damagePercentageGrids
   * 
   * @param damagePercentageGrids
   *          key=annuality, value=damagePercentageGrid as RectifiedGridCoverage
   * @param landuseGrid
   * @param administrationUnitGrid
   * @param assets
   *          key=landuseKey,administrationKey, value=asset as Double
   * @return TreeMap of DamageGrids
   * @throws Exception
   */
  public static TreeMap calculateDamages( TreeMap damagePercentageGrids, RectifiedGridCoverage landuseGrid,
      RectifiedGridCoverage administrationUnitGrid, Hashtable assets ) throws Exception
  {
    System.out.println( "Calculate DamageGrids..." );
    TreeMap<Double, RectifiedGridCoverage> damageGrids = new TreeMap<Double, RectifiedGridCoverage>();
    Iterator it = damagePercentageGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage damagePercentageGrid = (RectifiedGridCoverage)damagePercentageGrids.get( key );
      RectifiedGridCoverage damageGrid = null;
      double annuality = 1 / key.doubleValue();
      System.out.println( "HQ " + ( new Double( annuality ) ).intValue() );
      if( administrationUnitGrid != null )
      {
        damageGrid = calculateDamage( damagePercentageGrid, landuseGrid, administrationUnitGrid, assets );
      }
      else
      {
        damageGrid = calculateDamage( damagePercentageGrid, landuseGrid, assets );
      }
      damageGrids.put( key, damageGrid );
    }
    return damageGrids;
  }

  /**
   * calculates the damage for each cell in the damagePercentageGrid
   * 
   * @param damagePercentageGrid
   * @param landuseGrid
   * @param administrationUnitGrid
   * @param assets
   *          key=landuseKey,(administrationKey), value=asset as Double
   * @return RectifiedGridCoverage damageGrid
   * @throws Exception
   */
  private static RectifiedGridCoverage calculateDamage( RectifiedGridCoverage damagePercentageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid, Hashtable assets )
      throws Exception
  {
    RectifiedGridCoverage damageGrid = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damagePercentageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    GridGeometryHelper.controlGridGeometries( damagePercentageGrid.getGridDomain(), administrationUnitGrid
        .getGridDomain() );

    RectifiedGridDomain damage_gridDomain = new RectifiedGridDomain( damagePercentageGrid.getGridDomain().getOrigin(
        null ), damagePercentageGrid.getGridDomain().getOffset(), damagePercentageGrid.getGridDomain().getGridRange() );
    Vector damagePercentage_rangeSetData = damagePercentageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = administrationUnitGrid.getRangeSet().getRangeSetData();
    Vector<Vector<Double>> damage_rangeSetData = new Vector<Vector<Double>>();
    for( int i = 0; i < damagePercentage_rangeSetData.size(); i++ )
    {
      Vector damagePercentage_rowData = (Vector)damagePercentage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector)administrationUnit_rangeSetData.get( i );
      Vector<Double> damage_rowData = new Vector<Double>();
      for( int j = 0; j < damagePercentage_rowData.size(); j++ )
      {
        if( damagePercentage_rowData.get( j ) != null && landuse_rowData.get( j ) != null
            && administrationUnit_rowData != null )
        {
          try
          {
            double damage = 0;
            double damagePercentage = ( (Double)damagePercentage_rowData.get( j ) ).doubleValue();
            Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );
            Integer administrationUnitKey = new Integer( ( (Double)administrationUnit_rowData.get( j ) ).intValue() );
            String assetKey1 = new String( landuseKey.toString() + "," + administrationUnitKey.toString() );
            String assetKey2 = new String( landuseKey.toString() );
            if( assets.get( assetKey1 ) != null )
            {
              double asset = ( (Double)assets.get( assetKey1 ) ).doubleValue();
              damage = asset * ( damagePercentage / 100 );
            }
            else if( assets.get( assetKey2 ) != null )
            {
              double asset = ( (Double)assets.get( assetKey2 ) ).doubleValue();
              damage = asset * ( damagePercentage / 100 );
            }
            damage_rowData.addElement( new Double( damage ) );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
        else
        {
          damage_rowData.addElement( null );
        }
      }//for j
      damage_rangeSetData.addElement( damage_rowData );
      /*
       * System.out.println(i + " rows of " + damagePercentage_rangeSetData.size() + " calculated");
       */
    }//for i
    RangeSet damage_RangeSet = new RangeSet( damage_rangeSetData, null );
    damageGrid = new RectifiedGridCoverage( damage_gridDomain, damage_RangeSet );
    return damageGrid;
  }

  /**
   * calculates the damage for each cell in the damagePercentageGrid
   * 
   * @param damagePercentageGrid
   * @param landuseGrid
   * @param assets
   *          key=landuseKey,administrationKey, value=asset as Double
   * @return RectifiedGridCoverage damageGrid
   * @throws Exception
   */
  private static RectifiedGridCoverage calculateDamage( RectifiedGridCoverage damagePercentageGrid,
      RectifiedGridCoverage landuseGrid, Hashtable assets ) throws Exception
  {
    RectifiedGridCoverage damageGrid = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damagePercentageGrid.getGridDomain(), landuseGrid.getGridDomain() );

    RectifiedGridDomain damage_gridDomain = new RectifiedGridDomain( damagePercentageGrid.getGridDomain().getOrigin(
        null ), damagePercentageGrid.getGridDomain().getOffset(), damagePercentageGrid.getGridDomain().getGridRange() );
    Vector damagePercentage_rangeSetData = damagePercentageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector<Vector<Double>> damage_rangeSetData = new Vector<Vector<Double>>();
    for( int i = 0; i < damagePercentage_rangeSetData.size(); i++ )
    {
      Vector damagePercentage_rowData = (Vector)damagePercentage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector<Double> damage_rowData = new Vector<Double>();
      for( int j = 0; j < damagePercentage_rowData.size(); j++ )
      {
        if( damagePercentage_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
        {
          try
          {
            double damage = 0;
            double damagePercentage = ( (Double)damagePercentage_rowData.get( j ) ).doubleValue();
            Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );
            String assetKey = new String( landuseKey.toString() );
            if( assets.get( assetKey ) != null )
            {
              double asset = ( (Double)assets.get( assetKey ) ).doubleValue();
              damage = asset * ( damagePercentage / 100 );
            }
            damage_rowData.addElement( new Double( damage ) );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
        else
        {
          damage_rowData.addElement( null );
        }
      }//for j
      damage_rangeSetData.addElement( damage_rowData );
      /*
       * System.out.println(i + " rows of " + damagePercentage_rangeSetData.size() + " calculated");
       */
    }//for i
    RangeSet damage_RangeSet = new RangeSet( damage_rangeSetData, null );
    damageGrid = new RectifiedGridCoverage( damage_gridDomain, damage_RangeSet );
    return damageGrid;
  }

  /**
   * returns a Vector of tempGrids(RectifiedGridCoverages) for a given TreeMap of damageGrids(RectifiedGridCoverages)
   * 
   * @param damageGrids
   *          key=annuality(P), value=damageGrid(RectifiedGridCoverage)
   * @return tempGrids
   * @throws Exception
   */
  public static Vector calculateTempGridsAnnualDamage( TreeMap damageGrids ) throws Exception
  {
    System.out.println( "Calculate TempGrids..." );
    Vector<RectifiedGridCoverage> tempGrids = new Vector<RectifiedGridCoverage>();
    Object[] keys = damageGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        double deltaP = nextKey.doubleValue() - key.doubleValue();
        System.out.println( "deltaP=" + deltaP );
        RectifiedGridCoverage grid = (RectifiedGridCoverage)damageGrids.get( key );
        RectifiedGridCoverage nextGrid = (RectifiedGridCoverage)damageGrids.get( nextKey );
        // control Geometries
        GridGeometryHelper.controlGridGeometries( grid.getGridDomain(), nextGrid.getGridDomain() );

        RectifiedGridDomain temp_gridDomain = new RectifiedGridDomain( grid.getGridDomain().getOrigin( null ), grid
            .getGridDomain().getOffset(), grid.getGridDomain().getGridRange() );
        Vector grid_rangeSetData = grid.getRangeSet().getRangeSetData();
        Vector nextGrid_rangeSetData = nextGrid.getRangeSet().getRangeSetData();
        Vector<Vector<Double>> tempGrid_rangeSetData = new Vector<Vector<Double>>();
        for( int j = 0; j < grid_rangeSetData.size(); j++ )
        {
          Vector grid_rowData = (Vector)grid_rangeSetData.get( j );
          Vector nextGrid_rowData = (Vector)nextGrid_rangeSetData.get( j );
          Vector<Double> tempGrid_rowData = new Vector<Double>();
          for( int k = 0; k < grid_rowData.size(); k++ )
          {
            if( grid_rowData.get( k ) == null && nextGrid_rowData.get( k ) == null )
            {
              tempGrid_rowData.addElement( null );
            }
            else
            {
              double damage = 0;
              double nextDamage = 0;
              if( grid_rowData.get( k ) != null )
              {
                damage = ( (Double)grid_rowData.get( k ) ).doubleValue();
              }
              if( nextGrid_rowData.get( k ) != null )
              {
                nextDamage = ( (Double)nextGrid_rowData.get( k ) ).doubleValue();
              }
              try
              {
                double tempDamage = ( ( damage + nextDamage ) / 2 ) * deltaP;
                tempGrid_rowData.addElement( new Double( tempDamage ) );
              }
              catch( Exception e )
              {
                System.out.println( e );
              }
            }
          }//for k
          tempGrid_rangeSetData.addElement( tempGrid_rowData );
          /*
           * System.out.println(j + " rows of " + grid_rangeSetData.size() + " calculated");
           */
        }//for j
        RangeSet temp_RangeSet = new RangeSet( tempGrid_rangeSetData, null );
        RectifiedGridCoverage tempGrid = new RectifiedGridCoverage( temp_gridDomain, temp_RangeSet );
        tempGrids.addElement( tempGrid );
        System.out.println( "Key: " + key + ", NextKey: " + nextKey );
      }
    }//for i
    return tempGrids;
  }

  /**
   * returns the AnnualDamageGrid(RectifiedGridCoverage) for a given Vector of tempGrids(RectifiedGridCoverages)
   * 
   * @param tempGrids
   * @return annualDamageGrid(RectifiedGridCoverage)
   * @throws Exception
   */
  public static RectifiedGridCoverage calculateAnnualDamage( Vector tempGrids ) throws Exception
  {
    System.out.println( "Calculate AnnualDamageGrid..." );
    RectifiedGridCoverage firstGrid = (RectifiedGridCoverage)tempGrids.firstElement();
    RectifiedGridDomain annualDamage_gridDomain = new RectifiedGridDomain( firstGrid.getGridDomain().getOrigin( null ),
        firstGrid.getGridDomain().getOffset(), firstGrid.getGridDomain().getGridRange() );
    Vector firstGrid_rangeSetData = firstGrid.getRangeSet().getRangeSetData();
    Vector<Vector<Double>> annualDamageGrid_rangeSetData = new Vector<Vector<Double>>();
    for( int j = 0; j < firstGrid_rangeSetData.size(); j++ )
    {
      Vector firstGrid_rowData = (Vector)firstGrid_rangeSetData.get( j );
      Vector<Double> annualDamageGrid_rowData = new Vector<Double>();
      for( int k = 0; k < firstGrid_rowData.size(); k++ )
      {
        if( firstGrid_rowData.get( k ) != null )
        {
          double annualDamage = 0;
          for( int n = 0; n < tempGrids.size(); n++ )
          {
            RectifiedGridCoverage tempGrid = (RectifiedGridCoverage)tempGrids.get( n );
            Vector tempGrid_rangeSetData = tempGrid.getRangeSet().getRangeSetData();
            double tempDamage = 0;
            if( tempGrid_rangeSetData.get( j ) != null )
            {
              Vector tempGrid_rowData = (Vector)tempGrid_rangeSetData.get( j );
              if( tempGrid_rowData.get( k ) != null )
              {
                tempDamage = ( (Double)tempGrid_rowData.get( k ) ).doubleValue();
              }
            }
            annualDamage = annualDamage + tempDamage;
          }//for i
          annualDamageGrid_rowData.addElement( new Double( annualDamage ) );
        }
        else
        {
          annualDamageGrid_rowData.addElement( null );
        }
      }// for j
      annualDamageGrid_rangeSetData.addElement( annualDamageGrid_rowData );
      /*
       * System.out.println(j + " rows of " + annualDamageGrid_rangeSetData.size() + " calculated");
       */
    }//for k
    RangeSet annualDamage_rangeSet = new RangeSet( annualDamageGrid_rangeSetData, null );
    RectifiedGridCoverage annualDamageGrid = new RectifiedGridCoverage( annualDamage_gridDomain, annualDamage_rangeSet );
    return annualDamageGrid;
  }

}