package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

public class LanduseClass extends AbstractFeatureBinder implements ILanduseClass
{
  final List<IRiskLanduseStatistic> m_statList = new ArrayList<IRiskLanduseStatistic>();

  private double m_statisticsAverageAnnualDamage;

  private int m_statisticsNumberOfRasterCells;

  private double m_cellSize = Double.NaN;

  public LanduseClass( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  @Override
  public void setName( final String name )
  {
    getFeature().setProperty( ILanduseClass.PROP_NAME, name );
  }

  @Override
  public void setDescription( final String desc )
  {
    getFeature().setProperty( ILanduseClass.PROP_DESCRIPTION, desc );
  }

  @Override
  public String getName( )
  {
    return (String) getFeature().getProperty( ILanduseClass.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String) getFeature().getProperty( ILanduseClass.PROP_DESCRIPTION );
  }

  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( ILanduseClass.PROP_COLOR_STYLE );
  }

  public void setColorStyle( final RGB rgb )
  {
    getFeature().setProperty( ILanduseClass.PROP_COLOR_STYLE, rgb );
  }

  public void setOrdinalNumber( int value )
  {
    getFeature().setProperty( ILanduseClass.PROP_ORDINAL_NUMBER, value );
  }

  public int getOrdinalNumber( )
  {
    final Integer value = (Integer) getFeature().getProperty( ILanduseClass.PROP_ORDINAL_NUMBER );
    return value == null ? 0 : value.intValue();
  }

  public double getMaxAnnualDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MAX_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public double getMinAnnualDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MIN_DAMAGE );
    return value == null ? Double.MAX_VALUE : value.intValue();
  }

  public double getTotalDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_TOTAL_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public double getAverageAnnualDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public void setMaxAnnualDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MAX_DAMAGE, value );
  }

  public void setMinAnnualDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MIN_DAMAGE, value );
  }

  public void setTotalDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_TOTAL_DAMAGE, value );
  }

  public void setAverageAnnualDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE, value );
  }

  public String getDamageFunctionGmlID( )
  {
    final Object property = getFeature().getProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK );
    if( property != null && property instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl) property).getFeatureId();
    return "";
  }

  public void setDamageFunction( final IDamageFunction damageFunction )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_NAME + "#" + damageFunction.getGmlID();
    final XLinkedFeature_Impl xFeature = new XLinkedFeature_Impl( getFeature(), damageFunction.getFeature().getParentRelation(), damageFunction.getFeature().getFeatureType(), xFeaturePath, "", "", "", "", "" );
    getFeature().setProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK, xFeature );
  }

  public IAssetValueClass getAssetValue( )
  {
    final Object property = getFeature().getProperty( ILanduseClass.PROP_ASSET_VALUE_LINK );
    if( property != null && property instanceof XLinkedFeature_Impl )
    {
      XLinkedFeature_Impl xLinkedFeature = (XLinkedFeature_Impl) property;

      Feature assetFeature = xLinkedFeature.getFeature();
      IAssetValueClass assetValueClass = (IAssetValueClass) assetFeature.getAdapter( IAssetValueClass.class );
      return assetValueClass;
    }
    return null;

    // Feature parent = getFeature().getParent();
    // if( parent == null )
    // return null;
    //
    // final IRasterizationControlModel model = (IRasterizationControlModel) parent.getAdapter(
    // IRasterizationControlModel.class );
    // if( model == null )
    // return null;
    //
    // final List<IAssetValueClass> assetValueClassesList = model.getAssetValueClassesList();

    // TODO: probably the data model is not correctly modelled; this reverse search is not nice at all
    // Possible solutions: 1) link from landuseClass to assesValue instead
    // or 2) link from landusePolygon to assetValue

    // // TODO For the moment, administration units are ignored; consider using administration units
    //
    // final String landuseClassGmlID = getGmlID();
    // for( final IAssetValueClass assetValueClass : assetValueClassesList )
    // {
    // if( assetValueClass.getLanduseClassGmlID().equals( landuseClassGmlID ) )
    // return assetValueClass;
    // }
    //
    // return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#setAssetValue(org.kalypso.risk.model.schema.binding.IAssetValueClass)
   */
  public void setAssetValue( IAssetValueClass assetValueClass )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_NAME + "#" + assetValueClass.getGmlID();
    final XLinkedFeature_Impl xFeature = new XLinkedFeature_Impl( getFeature(), assetValueClass.getFeature().getParentRelation(), assetValueClass.getFeature().getFeatureType(), xFeaturePath, "", "", "", "", "" );
    getFeature().setProperty( ILanduseClass.PROP_ASSET_VALUE_LINK, xFeature );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#updateStatistic(double, double)
   */
  public void updateStatistic( final int returnPeriod )
  {
    for( IRiskLanduseStatistic entry : m_statList )
    {
      if( entry.getReturnPeriod() == returnPeriod )
        entry.finish();
    }
  }

  public IRiskLanduseStatistic getStatisticEntry( final int returnPeriod )
  {
    final List<IRiskLanduseStatistic> statisticList = getLanduseStatisticList();

    for( IRiskLanduseStatistic entry : statisticList )
    {
      if( entry.getReturnPeriod() == returnPeriod )
        return entry;
    }
    return null;
  }

  public List<IRiskLanduseStatistic> getLanduseStatisticList( )
  {
    final List<IRiskLanduseStatistic> statList = new ArrayList<IRiskLanduseStatistic>();

    final FeatureList list = (FeatureList) getFeature().getProperty( ILanduseClass.PROP_DAMAGE_STATISTIC_LIST );
    for( Object object : list )
    {
      if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        final IRiskLanduseStatistic statEntry = (IRiskLanduseStatistic) feature.getAdapter( IRiskLanduseStatistic.class );
        statList.add( statEntry );
      }
    }
    return statList;
  }

  public boolean containsStatisticEntry( final int returnPeriod )
  {
    for( final IRiskLanduseStatistic entry : m_statList )
      if( entry.getReturnPeriod() == returnPeriod )
        return true;
    return false;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#createNewLanduseClass()
   */
  public IRiskLanduseStatistic createNewStatisticEntry( )
  {
    final FeatureList list = (FeatureList) getFeature().getProperty( ILanduseClass.PROP_DAMAGE_STATISTIC_LIST );

    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( list, IRiskLanduseStatistic.QNAME, -1 );
      final IRiskLanduseStatistic listEntry = (IRiskLanduseStatistic) feature.getAdapter( IRiskLanduseStatistic.class );
      m_statList.add( listEntry );
      return listEntry;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#getStatistic(int, double)
   */
  public IRiskLanduseStatistic getStatistic( int returnPeriod )
  {
    for( final IRiskLanduseStatistic entry : m_statList )
      if( entry.getReturnPeriod() == returnPeriod )
        return entry;
    return null;

  }

  /**
   * adds a average annual damage value to the polygon
   */
  public void updateStatisticsAverageAnnualDamage( final double value )
  {
    /* check for min / max */
    if( value < getMinAnnualDamage() )
      setMinAnnualDamage( value );
    if( value > getMaxAnnualDamage() )
      setMaxAnnualDamage( value );

    /* update total damage value of the current landuse class */
    final double totalDamage = getTotalDamage() + value * m_cellSize;
    setTotalDamage( totalDamage );

    /* get the current overall average annual damage value (€/a) */
    final double currentValue = m_statisticsAverageAnnualDamage * m_statisticsNumberOfRasterCells;

    /* add the new value */
    final double updatedValue = currentValue + value;

    /* raise number of cells */
    m_statisticsNumberOfRasterCells++;

    /* calculate the average annual damage value (€/a) per cell */
    m_statisticsAverageAnnualDamage = updatedValue / m_statisticsNumberOfRasterCells;

    setAverageAnnualDamage( m_statisticsAverageAnnualDamage );
  }

  public double getStatisticsAverageAnnualDamage( )
  {
    return m_statisticsAverageAnnualDamage;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#setCellSize(int)
   */
  public void setCellSize( double cellSize )
  {
    m_cellSize = cellSize;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#getCellSize()
   */
  public double getCellSize( )
  {
    return m_cellSize;
  }

}
