package damageAnalysis;

import java.math.BigDecimal;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import mathTool.ParseFunction;

import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.opengis.cs.CS_CoordinateSystem;

import tools.Number;
import view.LogView;

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
   * returns a TreeMap of damagePercentageGrids (RectifiedGridCoverages) for a
   * given DamageAnalysisContext
   * 
   * @param daContext
   *          DamageAnalysisContext
   * @return TreeMap of DamagePercentageGrids (key=annuality,
   *         value=RectifiedGridCoverage)
   * @throws Exception
   */
  public static TreeMap calculateDamagePercentages( DamageAnalysisContext daContext )
      throws Exception
  {

    TreeMap waterlevelGrids = daContext.getWaterlevelGrids();
    RectifiedGridCoverage landuseGrid = daContext.getLanduseGrid();
    Hashtable damageFunctions = daContext.getDamagefunctions();

    TreeMap damagePercentageGrids = calculateDamagePercentages( waterlevelGrids, landuseGrid,
        damageFunctions );

    return damagePercentageGrids;
  }

  /**
   * returns a TreeMap of damagePercentageGrids as RectifiedGridCoverages for a
   * given TreeMap of waterlevelGrids (RectifiedGridCoverages)
   * 
   * @param waterlevelGrids
   *          key=annuality, value=waterlevelGrid as RectifiedGridCoverage
   * @param landuseGrid
   * @param damageFunctions
   *          key=landuseKey, value=parseFunction
   * @return TreeMap of damagePercentageGrids
   * @throws Exception
   */
  private static TreeMap calculateDamagePercentages( TreeMap waterlevelGrids,
      RectifiedGridCoverage landuseGrid, Hashtable damageFunctions ) throws Exception
  {
    LogView.println( "Calculate DamagePercentageGrids..." );
    TreeMap damagePercentageGrids = new TreeMap();
    Iterator it = waterlevelGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage waterlevelGrid = (RectifiedGridCoverage)waterlevelGrids.get( key );
      double annuality = 1 / key.doubleValue();
      LogView.println( "HQ " + ( new Double( annuality ) ).intValue() );
      RectifiedGridCoverage damagePercentageGrid = calculateDamagePercentage( waterlevelGrid,
          landuseGrid, damageFunctions );
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
  private static RectifiedGridCoverage calculateDamagePercentage(
      RectifiedGridCoverage waterlevelGrid, RectifiedGridCoverage landuseGrid,
      Hashtable damageFunctions ) throws Exception
  {
    RectifiedGridCoverage damagePercentageGrid = null;
    // control Geometries
    controlGridGeometries( waterlevelGrid.getGridDomain(), landuseGrid.getGridDomain() );

    RectifiedGridDomain damagePercentage_gridDomain = new RectifiedGridDomain( waterlevelGrid
        .getGridDomain().getOrigin( null ), waterlevelGrid.getGridDomain().getOffset(),
        waterlevelGrid.getGridDomain().getGridRange() );
    Vector waterlevel_rangeSetData = waterlevelGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector damagePercentage_rangeSetData = new Vector();
    for( int i = 0; i < waterlevel_rangeSetData.size(); i++ )
    {
      Vector waterlevel_rowData = (Vector)waterlevel_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector damagePercentage_rowData = new Vector();
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
              ParseFunction damageFunction = (ParseFunction)damageFunctions
                  .get( landuseKeyAsString );
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
       * System.out.println(i + " rows of " + waterlevel_rangeSetData.size() + "
       * calculated");
       */
    }//for i
    RangeSet damagePercentage_rangeSet = new RangeSet( damagePercentage_rangeSetData, null );
    damagePercentageGrid = new RectifiedGridCoverage( damagePercentage_gridDomain,
        damagePercentage_rangeSet );
    return damagePercentageGrid;
  }

  /**
   * returns a TreeMap of damageGrids for a given TreeMap of
   * damagePercentageGrids and DamageAnalysisContext
   * 
   * @param damagePercentageGrids
   *          key=annuality, value=RectifiedGridCoverage
   * @param daContext
   *          DamageAnalysisContext
   * @return TreeMap of DamageGrids (key=annuality, value=RectifiedGridCoverage)
   * @throws Exception
   */
  public static TreeMap calculateDamages( TreeMap damagePercentageGrids,
      DamageAnalysisContext daContext ) throws Exception
  {
    RectifiedGridCoverage landuseGrid = daContext.getLanduseGrid();
    RectifiedGridCoverage administrationUnitGrid = null;
    if( daContext.getAdministrationUnitGrid() != null )
    {
      administrationUnitGrid = daContext.getAdministrationUnitGrid();
    }
    Hashtable assets = daContext.getAssets();

    TreeMap damageGrids = DamageAnalysis.calculateDamages( damagePercentageGrids, landuseGrid,
        administrationUnitGrid, assets );
    return damageGrids;
  }

  /**
   * returns a TreeMap of damageGrids as RectifiedGridCoverages for a given
   * TreeMap of damagePercentageGrids
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
  private static TreeMap calculateDamages( TreeMap damagePercentageGrids,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid,
      Hashtable assets ) throws Exception
  {
    LogView.println( "Calculate DamageGrids..." );
    TreeMap damageGrids = new TreeMap();
    Iterator it = damagePercentageGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage damagePercentageGrid = (RectifiedGridCoverage)damagePercentageGrids
          .get( key );
      RectifiedGridCoverage damageGrid = null;
      double annuality = 1 / key.doubleValue();
      LogView.println( "HQ " + ( new Double( annuality ) ).intValue() );
      if( administrationUnitGrid != null )
      {
        damageGrid = calculateDamage( damagePercentageGrid, landuseGrid, administrationUnitGrid,
            assets );
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
   *          key=landuseKey,administrationKey, value=asset as Double
   * @return RectifiedGridCoverage damageGrid
   * @throws Exception
   */
  private static RectifiedGridCoverage calculateDamage( RectifiedGridCoverage damagePercentageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid,
      Hashtable assets ) throws Exception
  {
    RectifiedGridCoverage damageGrid = null;
    // control Geometries
    controlGridGeometries( damagePercentageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    controlGridGeometries( damagePercentageGrid.getGridDomain(), administrationUnitGrid
        .getGridDomain() );

    RectifiedGridDomain damage_gridDomain = new RectifiedGridDomain( damagePercentageGrid
        .getGridDomain().getOrigin( null ), damagePercentageGrid.getGridDomain().getOffset(),
        damagePercentageGrid.getGridDomain().getGridRange() );
    Vector damagePercentage_rangeSetData = damagePercentageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = administrationUnitGrid.getRangeSet().getRangeSetData();
    Vector damage_rangeSetData = new Vector();
    for( int i = 0; i < damagePercentage_rangeSetData.size(); i++ )
    {
      Vector damagePercentage_rowData = (Vector)damagePercentage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector)administrationUnit_rangeSetData.get( i );
      Vector damage_rowData = new Vector();
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
            Integer administrationUnitKey = new Integer( ( (Double)administrationUnit_rowData
                .get( j ) ).intValue() );
            String assetKey1 = new String( landuseKey.toString() + ","
                + administrationUnitKey.toString() );
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
       * System.out.println(i + " rows of " +
       * damagePercentage_rangeSetData.size() + " calculated");
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
    controlGridGeometries( damagePercentageGrid.getGridDomain(), landuseGrid.getGridDomain() );

    RectifiedGridDomain damage_gridDomain = new RectifiedGridDomain( damagePercentageGrid
        .getGridDomain().getOrigin( null ), damagePercentageGrid.getGridDomain().getOffset(),
        damagePercentageGrid.getGridDomain().getGridRange() );
    Vector damagePercentage_rangeSetData = damagePercentageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector damage_rangeSetData = new Vector();
    for( int i = 0; i < damagePercentage_rangeSetData.size(); i++ )
    {
      Vector damagePercentage_rowData = (Vector)damagePercentage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector damage_rowData = new Vector();
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
       * System.out.println(i + " rows of " +
       * damagePercentage_rangeSetData.size() + " calculated");
       */
    }//for i
    RangeSet damage_RangeSet = new RangeSet( damage_rangeSetData, null );
    damageGrid = new RectifiedGridCoverage( damage_gridDomain, damage_RangeSet );
    return damageGrid;
  }

  /**
   * returns a Vector of tempGrids(RectifiedGridCoverages) for a given TreeMap
   * of damageGrids(RectifiedGridCoverages)
   * 
   * @param damageGrids
   *          key=annuality(P), value=damageGrid(RectifiedGridCoverage)
   * @return tempGrids
   * @throws Exception
   */
  public static Vector calculateTempGridsAnnualDamage( TreeMap damageGrids ) throws Exception
  {
    LogView.println( "Calculate TempGrids..." );
    Vector tempGrids = new Vector();
    Object[] keys = damageGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        double deltaP = nextKey.doubleValue() - key.doubleValue();
        LogView.println( "deltaP=" + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) );
        RectifiedGridCoverage grid = (RectifiedGridCoverage)damageGrids.get( key );
        RectifiedGridCoverage nextGrid = (RectifiedGridCoverage)damageGrids.get( nextKey );
        // control Geometries
        controlGridGeometries( grid.getGridDomain(), nextGrid.getGridDomain() );

        RectifiedGridDomain temp_gridDomain = new RectifiedGridDomain( grid.getGridDomain()
            .getOrigin( null ), grid.getGridDomain().getOffset(), grid.getGridDomain()
            .getGridRange() );
        Vector grid_rangeSetData = grid.getRangeSet().getRangeSetData();
        Vector nextGrid_rangeSetData = nextGrid.getRangeSet().getRangeSetData();
        Vector tempGrid_rangeSetData = new Vector();
        for( int j = 0; j < grid_rangeSetData.size(); j++ )
        {
          Vector grid_rowData = (Vector)grid_rangeSetData.get( j );
          Vector nextGrid_rowData = (Vector)nextGrid_rangeSetData.get( j );
          Vector tempGrid_rowData = new Vector();
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
           * System.out.println(j + " rows of " + grid_rangeSetData.size() + "
           * calculated");
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
   * returns the AnnualDamageGrid(RectifiedGridCoverage) for a given Vector of
   * tempGrids(RectifiedGridCoverages)
   * 
   * @param tempGrids
   * @return annualDamageGrid(RectifiedGridCoverage)
   * @throws Exception
   */
  public static RectifiedGridCoverage calculateAnnualDamage( Vector tempGrids ) throws Exception
  {
    LogView.println( "Calculate AnnualDamageGrid..." );
    RectifiedGridCoverage firstGrid = (RectifiedGridCoverage)tempGrids.firstElement();
    RectifiedGridDomain annualDamage_gridDomain = new RectifiedGridDomain( firstGrid
        .getGridDomain().getOrigin( null ), firstGrid.getGridDomain().getOffset(), firstGrid
        .getGridDomain().getGridRange() );
    Vector firstGrid_rangeSetData = firstGrid.getRangeSet().getRangeSetData();
    Vector annualDamageGrid_rangeSetData = new Vector();
    for( int j = 0; j < firstGrid_rangeSetData.size(); j++ )
    {
      Vector firstGrid_rowData = (Vector)firstGrid_rangeSetData.get( j );
      Vector annualDamageGrid_rowData = new Vector();
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
       * System.out.println(j + " rows of " +
       * annualDamageGrid_rangeSetData.size() + " calculated");
       */
    }//for k
    RangeSet annualDamage_rangeSet = new RangeSet( annualDamageGrid_rangeSetData, null );
    RectifiedGridCoverage annualDamageGrid = new RectifiedGridCoverage( annualDamage_gridDomain,
        annualDamage_rangeSet );
    return annualDamageGrid;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each
   * landuseType
   * 
   * @param damageGrid
   * @param landuseGrid
   * @return Hashtable key=landuseTypeKey, value=Vector {sum,min,max}
   * @throws Exception
   */
  public static Hashtable getStatistics( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid ) throws Exception
  {
    Hashtable statistics = null;
    //control Geometries
    controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );

    statistics = new Hashtable();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    for( int i = 0; i < damage_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector)damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );

      for( int j = 0; j < damage_rowData.size(); j++ )
      {
        if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
        {
          try
          {
            double damagePerSquaremeter = ( (Double)damage_rowData.get( j ) ).doubleValue();
            double damage = damagePerSquaremeter * cellArea;
            Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );

            if( !statistics.containsKey( landuseKey ) )
            {
              Vector statisticVector = new Vector();
              statisticVector.addElement( new Double( damage ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statistics.put( landuseKey, statisticVector );
            }
            else
            {
              Vector actualStatisticVector = (Vector)statistics.get( landuseKey );
              double actualDamage = ( (Double)actualStatisticVector.get( 0 ) ).doubleValue();
              double actualMinValue = ( (Double)actualStatisticVector.get( 1 ) ).doubleValue();
              double actualMaxValue = ( (Double)actualStatisticVector.get( 2 ) ).doubleValue();
              actualDamage = actualDamage + damage;
              if( actualMinValue > damagePerSquaremeter )
              {
                actualMinValue = damagePerSquaremeter;
              }
              if( actualMaxValue < damagePerSquaremeter )
              {
                actualMaxValue = damagePerSquaremeter;
              }
              Vector statisticVector = new Vector();
              statisticVector.addElement( new Double( actualDamage ) );
              statisticVector.addElement( new Double( actualMinValue ) );
              statisticVector.addElement( new Double( actualMaxValue ) );
              statistics.put( landuseKey, statisticVector );
            }
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }//for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + "
       * calculated");
       */
    }//for i
    return statistics;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each
   * landuseType, only gridCells with value=1 in templateGrid are considered
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param templateGrid
   *          Grid with only cellValues 0 or 1
   * @return Hashtable key=landuseTypeKey, value=Vector {sum,min,max}
   * @throws Exception
   */
  public static Hashtable getStatisticsWithTemplate( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage templateGrid ) throws Exception
  {
    Hashtable statistics = null;
    // control Geometries
    controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    controlGridGeometries( damageGrid.getGridDomain(), templateGrid.getGridDomain() );

    statistics = new Hashtable();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector template_rangeSetData = templateGrid.getRangeSet().getRangeSetData();
    Double data = new Double( 1 );
    for( int i = 0; i < template_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector)damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector template_rowData = (Vector)template_rangeSetData.get( i );

      for( int j = 0; j < template_rowData.size(); j++ )
      {
        if( template_rowData.get( j ) != null )
        {
          if( template_rowData.get( j ).equals( data ) )
          {
            if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
            {
              try
              {
                double damagePerSquaremeter = ( (Double)damage_rowData.get( j ) ).doubleValue();
                double damage = damagePerSquaremeter * cellArea;
                Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );

                if( !statistics.containsKey( landuseKey ) )
                {
                  Vector statisticVector = new Vector();
                  statisticVector.addElement( new Double( damage ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statistics.put( landuseKey, statisticVector );
                }
                else
                {
                  Vector actualStatisticVector = (Vector)statistics.get( landuseKey );
                  double actualDamage = ( (Double)actualStatisticVector.get( 0 ) ).doubleValue();
                  double actualMinValue = ( (Double)actualStatisticVector.get( 1 ) ).doubleValue();
                  double actualMaxValue = ( (Double)actualStatisticVector.get( 2 ) ).doubleValue();
                  actualDamage = actualDamage + damage;
                  if( actualMinValue > damagePerSquaremeter )
                  {
                    actualMinValue = damagePerSquaremeter;
                  }
                  if( actualMaxValue < damagePerSquaremeter )
                  {
                    actualMaxValue = damagePerSquaremeter;
                  }
                  Vector statisticVector = new Vector();
                  statisticVector.addElement( new Double( actualDamage ) );
                  statisticVector.addElement( new Double( actualMinValue ) );
                  statisticVector.addElement( new Double( actualMaxValue ) );
                  statistics.put( landuseKey, statisticVector );
                }
              }
              catch( Exception e )
              {
                System.out.println( e );
              }
            }
          }
        }
      }//for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + "
       * calculated");
       */
    }//for i
    return statistics;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each
   * administrationUnit and landuseType
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param administrationUnitGrid
   * @return Hashtable key=administrationUnitKey, value=Hashtable
   *         (key=landuseTypeKey, value=Vector {sum,min,max})
   * @throws Exception
   */
  public static Hashtable getStatistics( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid )
      throws Exception
  {
    Hashtable statistics = null;
    // control Geometries
    controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    controlGridGeometries( damageGrid.getGridDomain(), administrationUnitGrid.getGridDomain() );

    statistics = new Hashtable();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = administrationUnitGrid.getRangeSet().getRangeSetData();
    for( int i = 0; i < damage_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector)damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector)administrationUnit_rangeSetData.get( i );
      for( int j = 0; j < damage_rowData.size(); j++ )
      {
        if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null
            && administrationUnit_rowData != null )
        {
          try
          {
            double damagePerSquaremeter = ( (Double)damage_rowData.get( j ) ).doubleValue();
            double damage = damagePerSquaremeter * cellArea;
            Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );
            Integer administrationUnitKey = new Integer( ( (Double)administrationUnit_rowData
                .get( j ) ).intValue() );
            if( !statistics.containsKey( administrationUnitKey ) )
            {
              Hashtable statistics_landuse = new Hashtable();
              Vector statisticVector = new Vector();
              statisticVector.addElement( new Double( damage ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statistics_landuse.put( landuseKey, statisticVector );
              statistics.put( administrationUnitKey, statistics_landuse );
            }
            else
            {
              Hashtable statistics_landuse = (Hashtable)statistics.get( administrationUnitKey );
              if( !statistics_landuse.containsKey( landuseKey ) )
              {
                Vector statisticVector = new Vector();
                statisticVector.addElement( new Double( damage ) );
                statisticVector.addElement( new Double( damagePerSquaremeter ) );
                statisticVector.addElement( new Double( damagePerSquaremeter ) );
                statistics_landuse.put( landuseKey, statisticVector );
              }
              else
              {
                Vector actualStatisticVector = (Vector)statistics_landuse.get( landuseKey );
                double actualDamage = ( (Double)actualStatisticVector.get( 0 ) ).doubleValue();
                double actualMinValue = ( (Double)actualStatisticVector.get( 1 ) ).doubleValue();
                double actualMaxValue = ( (Double)actualStatisticVector.get( 2 ) ).doubleValue();
                actualDamage = actualDamage + damage;
                if( actualMinValue > damagePerSquaremeter )
                {
                  actualMinValue = damagePerSquaremeter;
                }
                if( actualMaxValue < damagePerSquaremeter )
                {
                  actualMaxValue = damagePerSquaremeter;
                }
                Vector statisticVector = new Vector();
                statisticVector.addElement( new Double( actualDamage ) );
                statisticVector.addElement( new Double( actualMinValue ) );
                statisticVector.addElement( new Double( actualMaxValue ) );
                statistics_landuse.put( landuseKey, statisticVector );
              }
            }
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }//for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + "
       * calculated");
       */
    }//for i
    return statistics;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each
   * administrationUnit and landuseType, only gridCells with value=1 in
   * templateGrid are considered
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param administrationUnitGrid
   * @param templateGrid
   *          Grid with only cellValues 0 or 1
   * @return Hashtable key=administrationUnitKey, value=Hashtable
   *         (key=landuseTypeKey, value=Vector {sum,min,max})
   * @throws Exception
   */
  public static Hashtable getStatisticsWithTemplate( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid,
      RectifiedGridCoverage templateGrid ) throws Exception
  {
    Hashtable statistics = null;
    // control Geometries
    controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    controlGridGeometries( damageGrid.getGridDomain(), administrationUnitGrid.getGridDomain() );
    controlGridGeometries( damageGrid.getGridDomain(), templateGrid.getGridDomain() );

    statistics = new Hashtable();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = administrationUnitGrid.getRangeSet().getRangeSetData();
    Vector template_rangeSetData = templateGrid.getRangeSet().getRangeSetData();
    Double data = new Double( 1 );
    for( int i = 0; i < template_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector)damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector)administrationUnit_rangeSetData.get( i );
      Vector template_rowData = (Vector)template_rangeSetData.get( i );

      for( int j = 0; j < template_rowData.size(); j++ )
      {
        if( template_rowData.get( j ) != null )
        {
          if( template_rowData.get( j ).equals( data ) )
          {
            if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null
                && administrationUnit_rowData != null )
            {
              try
              {
                double damagePerSquaremeter = ( (Double)damage_rowData.get( j ) ).doubleValue();
                double damage = damagePerSquaremeter * cellArea;
                Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );
                Integer administrationUnitKey = new Integer( ( (Double)administrationUnit_rowData
                    .get( j ) ).intValue() );
                if( !statistics.containsKey( administrationUnitKey ) )
                {
                  Hashtable statistics_landuse = new Hashtable();
                  Vector statisticVector = new Vector();
                  statisticVector.addElement( new Double( damage ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statistics_landuse.put( landuseKey, statisticVector );
                  statistics.put( administrationUnitKey, statistics_landuse );
                }
                else
                {
                  Hashtable statistics_landuse = (Hashtable)statistics.get( administrationUnitKey );
                  if( !statistics_landuse.containsKey( landuseKey ) )
                  {
                    Vector statisticVector = new Vector();
                    statisticVector.addElement( new Double( damage ) );
                    statisticVector.addElement( new Double( damagePerSquaremeter ) );
                    statisticVector.addElement( new Double( damagePerSquaremeter ) );
                    statistics_landuse.put( landuseKey, statisticVector );
                  }
                  else
                  {
                    Vector actualStatisticVector = (Vector)statistics_landuse.get( landuseKey );
                    double actualDamage = ( (Double)actualStatisticVector.get( 0 ) ).doubleValue();
                    double actualMinValue = ( (Double)actualStatisticVector.get( 1 ) )
                        .doubleValue();
                    double actualMaxValue = ( (Double)actualStatisticVector.get( 2 ) )
                        .doubleValue();
                    actualDamage = actualDamage + damage;
                    if( actualMinValue > damagePerSquaremeter )
                    {
                      actualMinValue = damagePerSquaremeter;
                    }
                    if( actualMaxValue < damagePerSquaremeter )
                    {
                      actualMaxValue = damagePerSquaremeter;
                    }
                    Vector statisticVector = new Vector();
                    statisticVector.addElement( new Double( actualDamage ) );
                    statisticVector.addElement( new Double( actualMinValue ) );
                    statisticVector.addElement( new Double( actualMaxValue ) );
                    statistics_landuse.put( landuseKey, statisticVector );
                  }
                }
              }
              catch( Exception e )
              {
                System.out.println( e );
              }
            }
          }
        }
      }//for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + "
       * calculated");
       */
    }//for i

    return statistics;
  }

  /**
   * controls, if coordinate system, origin, grid extent and offset is equal for
   * to given RectifiedGridDomains; If not, an Exception is thrown with a
   * certain errorMessage
   * 
   * @param gridDomain1
   * @param gridDomain2
   * @throws Exception
   */
  public static void controlGridGeometries( RectifiedGridDomain gridDomain1,
      RectifiedGridDomain gridDomain2 ) throws Exception
  {
    //check coordinateSystems
    CS_CoordinateSystem cs1 = gridDomain1.getOrigin( null ).getCoordinateSystem();
    CS_CoordinateSystem cs2 = gridDomain2.getOrigin( null ).getCoordinateSystem();
    if( !cs1.getName().equals( cs2.getName() ) )
    {
      String errorMessage = "Diffrent Coordinate System. (CS 1: " + cs1.getName() + ", CS 2: "
          + cs2.getName() + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }

    //check origin
    GM_Point origin1 = gridDomain1.getOrigin( null );
    GM_Point origin2 = gridDomain2.getOrigin( null );
    // check x-Coordinate
    if( origin1.getX() != origin2.getX() )
    {
      String errorMessage = "Diffrent x-coordinate of origin. (x-coordinate 1: " + origin1.getX()
          + ", x-coordinate 2: " + origin2.getX() + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check y-Coordinate
    if( origin1.getY() != origin2.getY() )
    {
      String errorMessage = "Diffrent y-coordinate of origin. (y-coordinate 1: " + origin1.getY()
          + ", y-coordinate 2: " + origin2.getY() + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }

    // check extent
    int numCols1 = gridDomain1.getNumColumns();
    int numCols2 = gridDomain2.getNumColumns();
    int numRows1 = gridDomain1.getNumRows();
    int numRows2 = gridDomain2.getNumRows();
    // check number of columns
    if( numCols1 != numCols2 )
    {
      String errorMessage = "Diffrent number of columns. (Number of columns 1: " + numCols1
          + ", Number of columns 2: " + numCols2 + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    //check number of rows
    if( numRows1 != numRows2 )
    {
      String errorMessage = "Diffrent number of rows. (Number of rows 1: " + numRows1
          + ", Number of rows 2: " + numRows2 + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check offset
    double offsetX1 = gridDomain1.getOffsetX( cs1 );
    double offsetX2 = gridDomain2.getOffsetX( cs2 );
    double offsetY1 = gridDomain1.getOffsetY( cs1 );
    double offsetY2 = gridDomain2.getOffsetY( cs2 );
    // check offsetX
    if( offsetX1 != offsetX2 )
    {
      String errorMessage = "Diffrent offsetX. (OffsetX 1: " + offsetX1 + ", OffsetX 2: "
          + offsetX2 + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check offsetY
    if( offsetY1 != offsetY2 )
    {
      String errorMessage = "Diffrent offsetY. (OffsetY 1: " + offsetY1 + ", OffsetY 2: "
          + offsetY2 + ")";
      LogView.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
  }
}