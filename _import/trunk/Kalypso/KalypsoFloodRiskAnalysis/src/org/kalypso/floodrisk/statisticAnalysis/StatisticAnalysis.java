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
package org.kalypso.floodrisk.statisticAnalysis;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.floodrisk.tools.GridGeometryHelper;
import org.kalypso.floodrisk.tools.Number;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * StatisticAnalysis
 * <p>
 * Methods for calculating statistics created by
 * 
 * @author Nadja Peiler (15.06.2005)
 */
public class StatisticAnalysis
{

  /**
   * returns the statistic values sum, minValue and maxValue of a damageGrid for each landuseType
   * 
   * @param damageGrid
   * @param landuseGrid
   * @return Hashtable key=landuseTypeKey, value=Vector {sum,min,max}
   * @throws Exception
   */
  public static Hashtable getStatistics( RectifiedGridCoverage2 damageGrid, RectifiedGridCoverage2 landuseGrid ) throws Exception
  {
    Hashtable<Integer, Vector<Double>> statistics = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );

    statistics = new Hashtable<Integer, Vector<Double>>();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = null;//damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = null;//landuseGrid.getRangeSet().getRangeSetData();
    for( int i = 0; i < damage_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector) damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector) landuse_rangeSetData.get( i );

      for( int j = 0; j < damage_rowData.size(); j++ )
      {
        if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
        {
          try
          {
            double damagePerSquaremeter = ((Double) damage_rowData.get( j )).doubleValue();
            double damage = damagePerSquaremeter * cellArea;
            Integer landuseKey = new Integer( ((Double) landuse_rowData.get( j )).intValue() );

            if( !statistics.containsKey( landuseKey ) )
            {
              Vector<Double> statisticVector = new Vector<Double>();
              statisticVector.addElement( new Double( damage ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statistics.put( landuseKey, statisticVector );
            }
            else
            {
              Vector actualStatisticVector = statistics.get( landuseKey );
              double actualDamage = ((Double) actualStatisticVector.get( 0 )).doubleValue();
              double actualMinValue = ((Double) actualStatisticVector.get( 1 )).doubleValue();
              double actualMaxValue = ((Double) actualStatisticVector.get( 2 )).doubleValue();
              actualDamage = actualDamage + damage;
              if( actualMinValue > damagePerSquaremeter )
              {
                actualMinValue = damagePerSquaremeter;
              }
              if( actualMaxValue < damagePerSquaremeter )
              {
                actualMaxValue = damagePerSquaremeter;
              }
              Vector<Double> statisticVector = new Vector<Double>();
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
      }// for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + " calculated");
       */
    }// for i
    return statistics;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each landuseType, only gridCells with value=1 in
   * templateGrid are considered
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param templateGrid
   *          Grid with only cellValues 0 or 1
   * @return Hashtable key=landuseTypeKey, value=Vector {sum,min,max}
   * @throws Exception
   */
  public static Hashtable getStatisticsWithTemplate( RectifiedGridCoverage2 damageGrid, RectifiedGridCoverage2 landuseGrid, RectifiedGridCoverage2 templateGrid ) throws Exception
  {
    Hashtable<Integer, Vector<Double>> statistics = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), templateGrid.getGridDomain() );

    statistics = new Hashtable<Integer, Vector<Double>>();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = null;//damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = null;//landuseGrid.getRangeSet().getRangeSetData();
    Vector template_rangeSetData = null;//templateGrid.getRangeSet().getRangeSetData();
    Double data = new Double( 1 );
    for( int i = 0; i < template_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector) damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector) landuse_rangeSetData.get( i );
      Vector template_rowData = (Vector) template_rangeSetData.get( i );

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
                double damagePerSquaremeter = ((Double) damage_rowData.get( j )).doubleValue();
                double damage = damagePerSquaremeter * cellArea;
                Integer landuseKey = new Integer( ((Double) landuse_rowData.get( j )).intValue() );

                if( !statistics.containsKey( landuseKey ) )
                {
                  Vector<Double> statisticVector = new Vector<Double>();
                  statisticVector.addElement( new Double( damage ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statistics.put( landuseKey, statisticVector );
                }
                else
                {
                  Vector actualStatisticVector = statistics.get( landuseKey );
                  double actualDamage = ((Double) actualStatisticVector.get( 0 )).doubleValue();
                  double actualMinValue = ((Double) actualStatisticVector.get( 1 )).doubleValue();
                  double actualMaxValue = ((Double) actualStatisticVector.get( 2 )).doubleValue();
                  actualDamage = actualDamage + damage;
                  if( actualMinValue > damagePerSquaremeter )
                  {
                    actualMinValue = damagePerSquaremeter;
                  }
                  if( actualMaxValue < damagePerSquaremeter )
                  {
                    actualMaxValue = damagePerSquaremeter;
                  }
                  Vector<Double> statisticVector = new Vector<Double>();
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
      }// for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + " calculated");
       */
    }// for i
    return statistics;
  }

  /**
   * interprets the results of the statistical evaluation and creates a resultFile (Schema: StatisticData.xsd) for the
   * user
   * 
   * @param statistics
   *          Hashtable(key=landuseTypeKey, value=Vector {sum,min,max})
   * @param landuseTypeList
   *          Hashtable(key=Name of LanduseType (String), value=landuseTypeKey (Integer))
   * @param statisticDataURL
   *          URL of resultFile
   * @throws Exception
   */
  public static void exportStatisticAsXML( Hashtable statistics, Hashtable landuseTypeList, URL statisticDataURL ) throws Exception
  {
    // load schema
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_STATISTICDATA, (String) null );

    // create feature and workspace gml
    String rootFeatureName = "StatisticData"; //$NON-NLS-1$
    String collectionFeatureName = "Collection"; //$NON-NLS-1$
    String collectionPropertyName = "CollectionMember"; //$NON-NLS-1$
    String landuseFeatureName = "Landuse"; //$NON-NLS-1$
    String landusePropertyName = "LanduseMember"; //$NON-NLS-1$
    String sumPropertyName = "Sum"; //$NON-NLS-1$
    String overallSumProperty = "OverallSum"; //$NON-NLS-1$
    // create rootFeature: StatisticData
    final IFeatureType rootFT = schema.getFeatureType( rootFeatureName );
    Feature rootFeature = FeatureFactory.createFeature( null, null, "StatisticData0", rootFT, true ); //$NON-NLS-1$
    // create Collection
    final IFeatureType collFT = schema.getFeatureType( collectionFeatureName );
    final IRelationType collectionRelation = (IRelationType) rootFT.getProperty( collectionPropertyName );
    final Feature collection = FeatureFactory.createFeature( rootFeature, collectionRelation, "Collection0", collFT, true ); //$NON-NLS-1$
    FeatureHelper.addProperty( rootFeature, collectionRelation, collection );
    // create landuseFeatures
    double sumAll = 0;
    Iterator it = statistics.keySet().iterator();
    while( it.hasNext() )
    {
      Integer key = (Integer) it.next();
      Vector statisticsVector = (Vector) statistics.get( key );
      Double sum = (Double) statisticsVector.get( 0 );
      Double min = (Double) statisticsVector.get( 1 );
      Double max = (Double) statisticsVector.get( 2 );
      Iterator iterator = landuseTypeList.keySet().iterator();
      String landuse = null;
      while( iterator.hasNext() )
      {
        landuse = (String) iterator.next();
        Integer actualKey = (Integer) landuseTypeList.get( landuse );
        if( actualKey.equals( key ) )
        {
          break;
        }
      }
      Object[] properties = { "", "", null, landuse, min, max, sum }; //$NON-NLS-1$ //$NON-NLS-2$
      Feature landuseFeature = FeatureFactory.createFeature( null, null, "Landuse" + landuseTypeList.get( landuse ), schema.getFeatureType( landuseFeatureName ), properties ); //$NON-NLS-1$
      FeatureHelper.addProperty( collection, collFT.getProperty( landusePropertyName ), landuseFeature );

      int mode = BigDecimal.ROUND_HALF_EVEN;
      System.out.println( landuse + ": "+Messages.getString("statisticAnalysis.StatisticAnalysis.Sum")+"=" + Number.round( sum.doubleValue(), 2, mode ) + ", "+Messages.getString("statisticAnalysis.StatisticAnalysis.MinValue")+"=" + Number.round( min.doubleValue(), 4, mode ) + ", "+Messages.getString("statisticAnalysis.StatisticAnalysis.MaxValue")+"=" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
          + Number.round( max.doubleValue(), 4, mode ) );
      sumAll = sumAll + sum.doubleValue();
    }

    FeatureHelper.addProperty( collection, collFT.getProperty( sumPropertyName ), new Double( sumAll ) );
    FeatureHelper.addProperty( rootFeature, rootFT.getProperty( overallSumProperty ), new Double( sumAll ) );

    System.out.println( Messages.getString("statisticAnalysis.StatisticAnalysis.TotalDamage")+"= " + Number.round( sumAll, 2, BigDecimal.ROUND_HALF_EVEN ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // create workspace
    IFeatureType[] types = schema.getAllFeatureTypes();
    GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, statisticDataURL, "", null ); //$NON-NLS-1$

    // serialize Workspace
    FileWriter fw = new FileWriter( statisticDataURL.getFile() );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each administrationUnit and landuseType
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param administrationUnitGrid
   * @return Hashtable key=administrationUnitKey, value=Hashtable (key=landuseTypeKey, value=Vector {sum,min,max})
   * @throws Exception
   */
  public static Hashtable getStatistics( RectifiedGridCoverage2 damageGrid, RectifiedGridCoverage2 landuseGrid, RectifiedGridCoverage2 administrationUnitGrid ) throws Exception
  {
    Hashtable<Integer, Hashtable<Integer, Vector<Double>>> statistics = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), administrationUnitGrid.getGridDomain() );

    statistics = new Hashtable<Integer, Hashtable<Integer, Vector<Double>>>();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = null;//damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = null;//landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = null;//administrationUnitGrid.getRangeSet().getRangeSetData();
    for( int i = 0; i < damage_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector) damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector) landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector) administrationUnit_rangeSetData.get( i );
      for( int j = 0; j < damage_rowData.size(); j++ )
      {
        if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null && administrationUnit_rowData != null )
        {
          try
          {
            double damagePerSquaremeter = ((Double) damage_rowData.get( j )).doubleValue();
            double damage = damagePerSquaremeter * cellArea;
            Integer landuseKey = new Integer( ((Double) landuse_rowData.get( j )).intValue() );
            Integer administrationUnitKey = new Integer( ((Double) administrationUnit_rowData.get( j )).intValue() );
            if( !statistics.containsKey( administrationUnitKey ) )
            {
              Hashtable<Integer, Vector<Double>> statistics_landuse = new Hashtable<Integer, Vector<Double>>();
              Vector<Double> statisticVector = new Vector<Double>();
              statisticVector.addElement( new Double( damage ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statisticVector.addElement( new Double( damagePerSquaremeter ) );
              statistics_landuse.put( landuseKey, statisticVector );
              statistics.put( administrationUnitKey, statistics_landuse );
            }
            else
            {
              Hashtable<Integer, Vector<Double>> statistics_landuse = statistics.get( administrationUnitKey );
              if( !statistics_landuse.containsKey( landuseKey ) )
              {
                Vector<Double> statisticVector = new Vector<Double>();
                statisticVector.addElement( new Double( damage ) );
                statisticVector.addElement( new Double( damagePerSquaremeter ) );
                statisticVector.addElement( new Double( damagePerSquaremeter ) );
                statistics_landuse.put( landuseKey, statisticVector );
              }
              else
              {
                Vector actualStatisticVector = statistics_landuse.get( landuseKey );
                double actualDamage = ((Double) actualStatisticVector.get( 0 )).doubleValue();
                double actualMinValue = ((Double) actualStatisticVector.get( 1 )).doubleValue();
                double actualMaxValue = ((Double) actualStatisticVector.get( 2 )).doubleValue();
                actualDamage = actualDamage + damage;
                if( actualMinValue > damagePerSquaremeter )
                {
                  actualMinValue = damagePerSquaremeter;
                }
                if( actualMaxValue < damagePerSquaremeter )
                {
                  actualMaxValue = damagePerSquaremeter;
                }
                Vector<Double> statisticVector = new Vector<Double>();
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
      }// for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + " calculated");
       */
    }// for i
    return statistics;
  }

  /**
   * returns the statistic values sum, minValue and maxValue for each administrationUnit and landuseType, only gridCells
   * with value=1 in templateGrid are considered
   * 
   * @param damageGrid
   * @param landuseGrid
   * @param administrationUnitGrid
   * @param templateGrid
   *          Grid with only cellValues 0 or 1
   * @return Hashtable key=administrationUnitKey, value=Hashtable (key=landuseTypeKey, value=Vector {sum,min,max})
   * @throws Exception
   */
  public static Hashtable getStatisticsWithTemplate( RectifiedGridCoverage2 damageGrid, RectifiedGridCoverage2 landuseGrid, RectifiedGridCoverage2 administrationUnitGrid, RectifiedGridCoverage2 templateGrid ) throws Exception
  {
    Hashtable<Integer, Hashtable<Integer, Vector<Double>>> statistics = null;
    // control Geometries
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), landuseGrid.getGridDomain() );
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), administrationUnitGrid.getGridDomain() );
    GridGeometryHelper.controlGridGeometries( damageGrid.getGridDomain(), templateGrid.getGridDomain() );

    statistics = new Hashtable<Integer, Hashtable<Integer, Vector<Double>>>();
    GM_Point origin = damageGrid.getGridDomain().getOrigin( null );
    double offsetX = damageGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    double offsetY = damageGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    double cellArea = offsetX * offsetY;
    Vector damage_rangeSetData = null;//damageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = null;//landuseGrid.getRangeSet().getRangeSetData();
    Vector administrationUnit_rangeSetData = null;//administrationUnitGrid.getRangeSet().getRangeSetData();
    Vector template_rangeSetData = null;//templateGrid.getRangeSet().getRangeSetData();
    Double data = new Double( 1 );
    for( int i = 0; i < template_rangeSetData.size(); i++ )
    {
      Vector damage_rowData = (Vector) damage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector) landuse_rangeSetData.get( i );
      Vector administrationUnit_rowData = (Vector) administrationUnit_rangeSetData.get( i );
      Vector template_rowData = (Vector) template_rangeSetData.get( i );

      for( int j = 0; j < template_rowData.size(); j++ )
      {
        if( template_rowData.get( j ) != null )
        {
          if( template_rowData.get( j ).equals( data ) )
          {
            if( damage_rowData.get( j ) != null && landuse_rowData.get( j ) != null && administrationUnit_rowData != null )
            {
              try
              {
                double damagePerSquaremeter = ((Double) damage_rowData.get( j )).doubleValue();
                double damage = damagePerSquaremeter * cellArea;
                Integer landuseKey = new Integer( ((Double) landuse_rowData.get( j )).intValue() );
                Integer administrationUnitKey = new Integer( ((Double) administrationUnit_rowData.get( j )).intValue() );
                if( !statistics.containsKey( administrationUnitKey ) )
                {
                  Hashtable<Integer, Vector<Double>> statistics_landuse = new Hashtable<Integer, Vector<Double>>();
                  Vector<Double> statisticVector = new Vector<Double>();
                  statisticVector.addElement( new Double( damage ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statisticVector.addElement( new Double( damagePerSquaremeter ) );
                  statistics_landuse.put( landuseKey, statisticVector );
                  statistics.put( administrationUnitKey, statistics_landuse );
                }
                else
                {
                  Hashtable<Integer, Vector<Double>> statistics_landuse = statistics.get( administrationUnitKey );
                  if( !statistics_landuse.containsKey( landuseKey ) )
                  {
                    Vector<Double> statisticVector = new Vector<Double>();
                    statisticVector.addElement( new Double( damage ) );
                    statisticVector.addElement( new Double( damagePerSquaremeter ) );
                    statisticVector.addElement( new Double( damagePerSquaremeter ) );
                    statistics_landuse.put( landuseKey, statisticVector );
                  }
                  else
                  {
                    Vector actualStatisticVector = statistics_landuse.get( landuseKey );
                    double actualDamage = ((Double) actualStatisticVector.get( 0 )).doubleValue();
                    double actualMinValue = ((Double) actualStatisticVector.get( 1 )).doubleValue();
                    double actualMaxValue = ((Double) actualStatisticVector.get( 2 )).doubleValue();
                    actualDamage = actualDamage + damage;
                    if( actualMinValue > damagePerSquaremeter )
                    {
                      actualMinValue = damagePerSquaremeter;
                    }
                    if( actualMaxValue < damagePerSquaremeter )
                    {
                      actualMaxValue = damagePerSquaremeter;
                    }
                    Vector<Double> statisticVector = new Vector<Double>();
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
      }// for j
      /*
       * System.out.println(i + " rows of " + damage_rangeSetData.size() + " calculated");
       */
    }// for i

    return statistics;
  }

  /**
   * interprets the results of the statistical evaluation and creates a resultFile (Schema: StatisticData.xsd) for the
   * user Attention: if you want to use this method you have to change the schema, property "CollectionMember" must have
   * maxOccurs="unbounded"
   * 
   * @param statistics
   *          Hashtable(key=administrationUnitKey, value=Hashtable(key=landuseTypeKey, value=Vector {sum,min,max}))
   * @param administrationUnitList
   *          Hashtable(key=Name of AdministrationUnit (String), value=administrationUnitKey (Integer))
   * @param landuseTypeList
   *          Hashtable(key=Name of LanduseType (String), value=landuseTypeKey (Integer))
   * @param statisticDataURL
   *          URL of resultFile
   * @throws IOException
   * @throws GmlSerializeException
   */
  public static void exportStatisticAsXML( Hashtable statistics, Hashtable administrationUnitList, Hashtable landuseTypeList, URL statisticDataURL ) throws IOException, GmlSerializeException, InvocationTargetException
  {
    // load schema
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_STATISTICDATA, (String) null );

    // create feature and workspace gml
    String rootFeatureName = "StatisticData"; //$NON-NLS-1$
    String collectionFeatureName = "Collection"; //$NON-NLS-1$
    String collectionPropertyName = "CollectionMember"; //$NON-NLS-1$
    String namePropertyName = "Name"; //$NON-NLS-1$
    String landuseFeatureName = "Landuse"; //$NON-NLS-1$
    String landusePropertyName = "LanduseMember"; //$NON-NLS-1$
    String sumPropertyName = "Sum"; //$NON-NLS-1$
    String overallSumProperty = "OverallSum"; //$NON-NLS-1$
    // create rootFeature: StatisticData
    final IFeatureType rootFT = schema.getFeatureType( rootFeatureName );
    Feature rootFeature = FeatureFactory.createFeature( null, null, "StatisticData0", rootFT, true ); //$NON-NLS-1$

    double sumAll = 0;
    Iterator it = statistics.keySet().iterator();
    // create Collections
    int numOfCol = 0;
    int numOfFeat = 0;
    while( it.hasNext() )
    {
      Integer administrationUnitKey = (Integer) it.next();
      Hashtable statistics_landuse = (Hashtable) statistics.get( administrationUnitKey );
      Iterator it1 = statistics_landuse.keySet().iterator();
      double sum_adminUnit = 0;
      Iterator iter = administrationUnitList.keySet().iterator();
      String adminUnit = null;
      while( iter.hasNext() )
      {
        adminUnit = (String) iter.next();
        Integer actualKey = (Integer) administrationUnitList.get( adminUnit );
        if( actualKey.equals( administrationUnitKey ) )
        {
          break;
        }
      }
      // create Collection
      final IFeatureType collFT = schema.getFeatureType( collectionFeatureName );
      final IRelationType collectionRelation = (IRelationType) rootFT.getProperty( collectionPropertyName );
      Feature collection = FeatureFactory.createFeature( rootFeature, collectionRelation, "Collection" + numOfCol, collFT, true ); 
      FeatureHelper.addProperty( collection, collFT.getProperty( namePropertyName ), adminUnit );
      FeatureHelper.addProperty( rootFeature, collectionRelation, collection );

      System.out.println( adminUnit + ": " ); //$NON-NLS-1$
      // create landuse-features
      while( it1.hasNext() )
      {
        Integer landuseKey = (Integer) it1.next();
        Vector statisticsVector = (Vector) statistics_landuse.get( landuseKey );
        Double sum = (Double) statisticsVector.get( 0 );
        sum_adminUnit = sum_adminUnit + sum.doubleValue();
        Double min = (Double) statisticsVector.get( 1 );
        Double max = (Double) statisticsVector.get( 2 );
        Iterator iterator = landuseTypeList.keySet().iterator();
        String landuse = null;
        while( iterator.hasNext() )
        {
          landuse = (String) iterator.next();
          Integer actualKey = (Integer) landuseTypeList.get( landuse );
          if( actualKey.equals( landuseKey ) )
          {
            break;
          }
        }
        Object[] properties = { "", "", null, landuse, min, max, sum }; //$NON-NLS-1$ //$NON-NLS-2$
        final IRelationType landuseRelation = (IRelationType) collFT.getProperty( landusePropertyName );
        final Feature landuseFeature = FeatureFactory.createFeature( collection, landuseRelation, "Landuse" + numOfFeat, schema.getFeatureType( landuseFeatureName ), properties ); //$NON-NLS-1$
        numOfFeat = numOfFeat + 1;
        FeatureHelper.addProperty( collection, landuseRelation, landuseFeature );
        int mode = BigDecimal.ROUND_HALF_EVEN;
        System.out.println( landuse + ": "+Messages.getString("statisticAnalysis.StatisticAnalysis.Sum")+"=" + Number.round( sum.doubleValue(), 2, mode ) + ", "+Messages.getString("statisticAnalysis.StatisticAnalysis.MinValue")+"=" + Number.round( min.doubleValue(), 4, mode ) + ", "+Messages.getString("statisticAnalysis.StatisticAnalysis.MaxValue")+"=" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
            + Number.round( max.doubleValue(), 4, mode ) );
      }
      int mode = BigDecimal.ROUND_HALF_EVEN;
      System.out.println( Messages.getString("statisticAnalysis.StatisticAnalysis.SummedDamage")+"=" + Number.round( sum_adminUnit, 2, mode ) ); //$NON-NLS-1$ //$NON-NLS-2$
      FeatureHelper.addProperty( collection, collFT.getProperty( sumPropertyName ), new Double( sum_adminUnit ) );
      sumAll = sumAll + sum_adminUnit;
      numOfCol = numOfCol + 1;
    }
    System.out.println( Messages.getString("statisticAnalysis.StatisticAnalysis.TotalDamage")+"=" + Number.round( sumAll, 2, BigDecimal.ROUND_HALF_EVEN ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    FeatureHelper.addProperty( rootFeature, rootFT.getProperty( overallSumProperty ), new Double( sumAll ) );

    // create workspace
    final IFeatureType[] types = schema.getAllFeatureTypes();
    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, statisticDataURL, "", null ); //$NON-NLS-1$

    // serialize Workspace
    FileWriter fw = new FileWriter( statisticDataURL.getFile() );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

}